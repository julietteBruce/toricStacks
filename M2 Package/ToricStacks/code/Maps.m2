---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-------------------------- TYPE / CONSTRUCTORS ----------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

-----------------------------------------------------------------------------
---- TORIC STACK MAP TYPE DECLARATION
-----------------------------------------------------------------------------
ToricStackMap = new Type of HashTable
ToricStackMap.synonym = "toric stack map"

-----------------------------------------------------------------------------
---- This is the main toric stack constructor, but this version is (probably)
---- never used by the user. It is specified by a tuple (D2, D1, A)
---- D1 = source toric stack with lattice N_1 and presentation quot. ZZ^(m1)
---- D2 = target toric stack with lattice N_2 and presentation quot. ZZ^(m2)
---- A = {bigPhi, littlePhi}
---- bigPhi = N_1 ---> N_2
---- littlePhi = ZZ^(m_1) ---> ZZ^(m_2)
---- satisfying descent conditions so littlePhi descends to a map L1 ---> L2
---- that is compatible with bigPhi and the stacky data of D2 and D1
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, List) := ToricStackMap => opts -> (D2, D1, A) -> (
    if #A != 2 then error "Expected list to be a pair {bigPhi, littlePhi}";
    stackMap := new ToricStackMap from {
    	symbol source => D1,
    	symbol target => D2,
    	symbol map => A,
    	symbol cache => new CacheTable} ; 
    assert(isWellDefined stackMap);
    stackMap
    )

-----------------------------------------------------------------------------
---- Contructs from two matrices instead of a list of matrices
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, Matrix, Matrix) := ToricStackMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
    A := {bigPhi, littlePhi};
    map(D2, D1, A, opts)
    )

-----------------------------------------------------------------------------
---- Contructs either the zero-map or the map given by m*Id.
---- If m != 0 the source and target lattices must be the same.
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, ZZ) := ToricStackMap => opts -> (D2, D1, m) -> (
    bigPhi := map(source (D2.map), source (D1.map), 0);
    littlePhi := map(target (D2.map), target (D1.map),  0);
    if not inducesWellDefinedMap(coker(D2.presentation), coker(D1.presentation), littlePhi) then (
	error "Multuplication by m does not descend to a well-defined map";
	)
    lhs := (D2.map)*(bigPhi);
    rhs := inducedMap(coker(D2.presentation), source(D1.map), (littlePhi)*(D1.map));
    if lhs != rhs then (
	error "Multiplication by m does not commute to give a map of toric stacks.";
	)
    map(D2,D1,{bigPhi,littlePhi}, opts)
    )

-----------------------------------------------------------------------------
---- isWellDefined for ToricStackMap
-----------------------------------------------------------------------------

isWellDefined(ToricStackMap) := Boolean => f -> (
    --- checks the source and target are well-defined toric stacls
    (D1, D2) := (source f, target f);
    if not isWellDefined D1 then (
        if debugLevel > 0 then 
            << "-- expected source of map to be a well-defined toric stack." << endl;
            return false
	);
    if not isWellDefined D2 then (
        if debugLevel > 0 then 
            << "-- exected target of map to be a well-defined toric stack." << endl;
            return false
	);
    --- checks the map has correct input type
    phiList := map f;
    if not instance (phiList, List) then (
	if debugLevel > 0 then
	    << "-- expected map to be a list." << endl;
	    return false
	);
    if #phiList != 2 then (
	if debuglevel > 0 then
	    << "-- expected map to be a list of size 2." << endl;
	    return false
	);
    (bigPhi, littlePhi) := (phiList#0, phiList#1);
    if not instance (bigPhi, Matrix) or not instance (littlePhi, Matrix) then (
        if debugLevel > 0 then 
            << "-- expected map to be described by list of two matrices." << endl;
            return false
        );
    --- checks bigPhi has correct target and source
    if rank source bigPhi != rank source D1.map then (
        if debugLevel > 0 then 
            << "-- expected source of bigPhi be the fan lattice of the source stack." << endl;
        return false
        );
    if rank target bigPhi != rank source D2.map then (
        if debugLevel > 0 then 
            << "-- expected target of bigPhi be the fan lattice of the target stack." << endl;
        return false
        );
    --- checks littlePhi has correct target and source
    if rank source littlePhi != rank target D1.presentation then (
        if debugLevel > 0 then 
            << "-- expected source of littlePhi be the target presentation of the source stack" << endl;
        return false
        );
    if rank target littlePhi != rank target D2.presentation then (
        if debugLevel > 0 then 
            << "-- expected target of littlePhi be target presentation of the target stack" << endl;
        return false
        );
    --- checks if littlePhi is compatible with Q1 and Q2 to descend to a map of L1 and L2
    if not inducesWellDefinedMap(coker(D2.presentation), coker(D1.presentation), littlePhi) then (
	if debugLevel > 0 then
	    << "-- expected littlePhi to descend to a map coker(Q1) ---> coker(Q2)." << endl;
	    return false
	);
    --- checks that maps commute on L1 and L2
    --- note since littlePhi descends we only need
    --- beta2*bigPhi == (littePhi * beta1) mod Q2
    lhs := (D2.map)*(bigPhi);
    rhs := inducedMap(coker(D2.presentation), source(D1.map), (littlePhi)*(D1.map));
    if lhs != rhs then (
        if debugLevel > 0 then 
            << "-- expected maps to commute" << endl;
        return false
    );
    --- checks bigPhi sends cones to cones. 
    if not mapsConestoCones(fan D2, fan D1, bigPhi) then (
        if debugLevel > 0 then 
            << "-- expected that bigPhi sends cones to cones" << endl;
        return false
        );
    return true
)
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
----------------------------------- BASICS --------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
    
-----------------------------------------------------------------------------
---- Defines the id map on a ToricStack
-----------------------------------------------------------------------------
ToricStack#id = D -> map(D,D,1)

-----------------------------------------------------------------------------
---- Defines source, target, map for ToricStackMap
-----------------------------------------------------------------------------
source ToricStackMap := ToricStack => f -> f.source
target ToricStackMap := ToricStack => f -> f.target
map ToricStackMap := List => opts -> f -> f.map

-----------------------------------------------------------------------------
---- Defines == for ToricStackMap
-----------------------------------------------------------------------------
ToricStackMap == ToricStackMap := Boolean => (f1, f2) -> (
    source f1 === source f2 and target f1 === target f2 and map f1 == map f2
    )

-----------------------------------------------------------------------------
---- Defines compostion for ToricStackMap
-----------------------------------------------------------------------------
ToricStackMap * ToricStackMap := ToricStackMap => (f1, f2) -> (
    if target f1 =!= source f2 then error "-- expected composable maps";
    -- defines source and target
    D1 := source f1;
    D2 := target f2;
    -- composes maps
    bigPhi := ((map f2)#0)*((map f1)#0);
    littlePhi := ((map f2)#1)*((map f1)#1);
    A := {bigPhi, littlePhi};
    -- returns map
    map(D2,D2,A)
    )

-----------------------------------------------------------------------------
---- Returns the induced map on the stacky "abelian group"
-----------------------------------------------------------------------------
stackyAbelianGroupMap= method()
stackyAbelianGroupMap(ToricStackMap) := Matrix => f -> (
    (D1, D2) := (source f, target f);
    littlePhi := (map f)#1;
    inducedMap(coker(D2.presentation), coker(D1.presentation), littlePhi)
)

-----------------------------------------------------------------------------
---- Returns the map on the fan lattices
-----------------------------------------------------------------------------
fanLatticeMap= method()
fanLatticeMap(ToricStackMap) := Matrix => f -> (
    (map f)#0
)


-----------------------------------------------------------------------------
---- diagonalMap for toric stacks
-----------------------------------------------------------------------------
diagonalMap (ToricStack, ZZ, Array) := ToricStackMap => (D, m, A) -> (
    --- checks
    if m <= 0 then error "Expected a positive integer.";
    if #A == 0 then error "Expected the array to be non-empty";
    if not all(A, i -> member(i, toList(0 .. m-1))) then (
	error "Expected the array to be index the factors.";
	);
    ---
    Dm := D ^** m;
    ---
    bigId := id_(source D.map);
    littleId := id_(target D.map);

    bigPhi := transpose matrix {
        apply(m, i -> if member(i, A) then bigId else 0 * bigId)
        };

    littlePhi := transpose matrix {
        apply(m, i -> if member(i, A) then littleId else 0 * littleId)
        };

    map(Dm, D, bigPhi, littlePhi)
    )

diagonalMap (ToricStack, ZZ) := ToricStackMap => (D, m) -> (
    if m <= 0 then error "Expected a positive integer.";
    A := new Array from (0 .. m-1);
    diagonalMap(D, m, A)
    )

diagonalMap ToricStack := ToricStackMap => D -> diagonalMap(D, 2)
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
----------------------------- ISOMOSPHISMS --------------------------------------
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------

preimageCones = method()
preimageCones(ToricStackMap, Cone) := List => (f, sigma) -> (
    (D1, D2) := (source f, target f);
    bigPhi := (map f)#0;
    select(maxFacesAsCones(fan D1), tau -> contains(tau,affineImage(bigPhi, tau)))
)
--- might need to change this to include non maximal cones!

isInjectiveOnCones = method()
isInjectiveOnCones(ToricStackMap) := Boolean => f -> (
    all(apply(maxFacesAsCones(fan target f),
        tau -> length(preimageCones(f, tau)) == 1))
)
--- I'm implementing this because it shows up, e.g. in theorem 6.3 condition (1) for a good moduli morphism.



-* EXAMPLE 6.23
beta1 = matrix {{1,1},{0,2}};
rayList1 = {{1,0},{0,1}};
coneList1 = {{0,1}};
D1 = toricStack(beta1, rayList1, coneList1);

beta2 = matrix {{1,0},{0,1}};
rayList2 = {{1,0},{1,2}};
coneList2 = {{0,1}};
D2 = toricStack(beta2, rayList2, coneList2);

bigPhi = matrix {{1,1},{0,2}};
littlePhi = matrix {{1,0},{0,1}};
f = map(D2, D1, bigPhi, littlePhi)

assert(isInjectiveOnCones f)
*-

--TODO: Now in position to implement Theorem 6.3.

--isGoodModuliMap = method()
--isGoodModuliMap(ToricStackMap) := Boolean => f -> (



--- must check if cok of beta and beta' are finite...
isIsomorphism(ToricStackMap) := Boolean => f -> (
    phiList := map f;
    (D1, D2) := (source f, target f);
    (bigPhi, littlePhi) := (phiList#0, phiList#1);
    ---
    beta1 = (map D1)|(presentation D2)
    beta2 = (map D2)|(presentation D2)
    --- This uses Theorem B.3 in Geraschencko and Satriano
    if rank((coker beta1) ** QQ) == 0 and rank((coker beta2) ** QQ) == 0 then (
	condition1 := isIsomorphism(stackyAbelianGroupMap(f));
        condition2 := (
            apply(maxFacesAsCones(D2.max),
                tau -> class(affinePreimage(bigPhi, tau)) === Cone
            ));
        -- is this right?? I'm a little confused about the condition 2 for Theorem B.3
        condition3 := apply(maxFacesAsCones(D2.max),
                tau -> (   
                    getHilbRays affinePreimage(bigPhi, tau) == getHilbRays tau
                ));
        all({condition1} | condition2 | condition3, bool -> bool)
    ) else (
        error("-- need to implement Proposition B.21 of Garschenko and Satriano to handle when the maps have nonfinite cokernel")
    )
)

-*
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
bigPhi = matrix {{2,0},{0,2}}
littlePhi = matrix {{2,0},{0,2}}
A = {bigPhi, littlePhi};
f = map(D1,D1,A)

source f
target f
map f
*-





-*


betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A1)

bigPhi2 = matrix {{4,0},{0,4}}
littlePhi2 = matrix {{4,0},{0,4}}
A2 = {bigPhi2, littlePhi2};
f2 = map(D1,D1,A2)

f2*f1
*-
