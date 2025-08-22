---------------------------------------------------------------------------
---- TORIC STACK MAP TYPE DECLARATION
-----------------------------------------------------------------------------
ToricStackMap = new Type of HashTable
ToricStackMap.synonym = "toric stack map"


-----------------------------------------------------------------------------
----- map(ToricStack,ToricStack,-)
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
---- Main Constructor: Inputs betaMap as a matrix and toric variety in the 
---- form of a list of rays and a list of maximal cones.
----
---- **ALL** other constructors should compute a D2, D1, and an A map
---- then call this main version of map for ToricStack. This is
---- for consistentcy and easy of debugging. 
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, List) := ToricStackMap => opts -> (D2, D1, A) -> (
    bigPhi := A#0;
    littlePhi := A#1;
    if ring bigPhi =!= ZZ or ring littlePhi =!= ZZ then error "-- expected integer matrices";
    if rank source bigPhi != rank source D1.map then 
        error("-- expected source of bigPhi be the source lattice of D1");
    if rank target bigPhi != rank source D2.map then 
        error("-- expected target of bigPhi be the source lattice of D1");
    if rank source littlePhi != rank target D1.map then 
        error("-- expected source of littlePhi be the target lattice of D2");
    if rank target littlePhi != rank target D2.map then
        error("-- expected target of littlePhi be the target lattice of D2");
    if (D2.map)*(bigPhi) != (littlePhi)*(D1.map) then
        error("-- expected maps to commute");
    if not mapsConestoCones(fan D2, fan D1, bigPhi)
    then error("-- expected that map sends cones to cones");
    new ToricStackMap from {
    	symbol source => D1,
    	symbol target => D2,
    	symbol map => A,
    	symbol cache => new CacheTable}
    )

-----------------------------------------------------------------------------
---- Contructs from two matrices instead of a list of matrices
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, Matrix, Matrix) := ToricStackMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
    A := {bigPhi, littlePhi};
    map(D2, D1, A)
    )

-----------------------------------------------------------------------------
---- Contructs either the zero-map or the map given by m*Id.
---- If m != 0 the source and target lattices must be the same.
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, ZZ) := ToricStackMap => opts -> (D2, D1, m) -> (
    rankSource := {rank source D1.map, rank target D2.map};
    rankTarget := {rank source D2.map, rank target D2.map};
    if m == 0 then (
	littlePhi := map(ZZ^(rankTarget#0), ZZ^(rankSource#0), 0);
	bigPhi := map(ZZ^(rankTarget#1), ZZ^(rankSource#1), 0);
	A := {bigPhi, littlePhi};
	return map(D2, D1, A)
	)
    else if rankSource == rankTarget then (
	littlePhi = map(ZZ^(rankTarget#0), ZZ^(rankSource#0), m);
	bigPhi = map(ZZ^(rankTarget#1), ZZ^(rankSource#1), m);
	A = {bigPhi, littlePhi};
	return map(D2, D1, A)
	)
    else error "source and target must have same rank or m=0"
	)

-----------------------------------------------------------------------------
---- Defines the id map on a ToricStack
-----------------------------------------------------------------------------
ToricStack#id = D -> map(D,D,1)

--- This uses Theorem B.3 in Geraschencko and Satriano
isIsomorphism(ToricStackMap) := Boolean => f -> (
    phiList := map f;
    (D1, D2) := (source f, target f);
    (bigPhi, littlePhi) := (phiList#0, phiList#1);
    condition1 := (rank littlePhi == numcols littlePhi);
    condition2 := (
        apply(maxFacesAsCones(fan D2),
            tau -> class(affinePreimage(bigPhi, tau)) === Cone
        ));
    -- is this right?? I'm a little confused about the condition 2 for Theorem B.3
    condition3 := apply(maxFacesAsCones(fan D2),
            tau -> (   
                getHilbRays affinePreimage(bigPhi, tau) == getHilbRays tau
            ));
    all({condition1} | condition2 | condition3, bool -> bool)
)
--------------------------------------------------------------------
--------------------------------------------------------------------
----------------------------- BASICS -------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

-----------------------------------------------------------------------------
---- Defines source, target, and map of ToricStackDatumMap
-----------------------------------------------------------------------------
source ToricStackMap := ToricStack => f -> f.source
target ToricStackMap := ToricStack => f -> f.target
map ToricStackMap := List => opts -> f -> f.map

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


-----------------------------------------------------------------------------
---- Defines == for ToricStackDatumMap
-----------------------------------------------------------------------------
ToricStackMap == ToricStackMap := Boolean => (f1, f2) -> (
    source f1 === source f2 and target f1 === target f2 and map f1 == map f2
    )

-----------------------------------------------------------------------------
----- rankSource
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
rankSource = method ()
rankSource (ToricStackMap) := (f) -> ({rank source (f.source).map, rank target (f.source).map})

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A)

rankSource f1

-----------------------------------------------------------------------------
----- rankSource
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
rankTarget = method ()
rankTarget (ToricStackMap) := (f) -> ({rank source (f.target).map, rank target (f.target).map})

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A)

rankTarget f1


-----------------------------------------------------------------------------
---- Defines compostion for ToricStackDatumMap
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
