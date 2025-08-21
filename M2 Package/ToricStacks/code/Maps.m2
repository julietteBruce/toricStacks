needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"


--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- CREATE TYPE ------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";


-----------------------------------------------------------------------------
---- Stack  TYPE DECLARATION
-----------------------------------------------------------------------------

Stack = new Type of MutableHashTable
Stack.synonym = "stack"
Stack.GlobalAssignHook = globalAssignFunction
Stack.GlobalReleaseHook = globalReleaseFunction

-----------------------------------------------------------------------------
---- TORIC STACK TYPE DECLARATION
-----------------------------------------------------------------------------

ToricStack = new Type of Stack
ToricStack.synonym = "toric stack datum"
ToricStack.GlobalAssignHook = globalAssignFunction
ToricStack.GlobalReleaseHook = globalReleaseFunction
expression ToricStack := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
   (describe D)#0
describe ToricStack := D -> Describe (expression toricStackDatum) (
    expression D.map, expression D.rays, expression D.max)



--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- CONSTRUCTORS -----------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

-----------------------------------------------------------------------------
----- toricStackDatum
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
----------------------------------------------------------------------------- 
toricStackDatum = method (
    TypicalValue => ToricStack, 
    Options => {
    	CoefficientRing   => QQ,
    	Variable          => getSymbol "x",
	NonStrict         => false
	}
    )


-----------------------------------------------------------------------------
---- Main Constructor: Inputs betaMap as a matrix and toric variety in the 
---- form of a list of rays and a list of maximal cones.
----
---- **ALL** other constructors should compute a betaMap, raysList, and
---- coneList and then call this main version of toricStackDatum. This is
---- for consistentcy and easy of debugging. 
-----------------------------------------------------------------------------
toricStackDatum (Matrix, List, List) := opts -> (betaMap, rayList, coneList) -> (
   -- sorting rays/cones gives a slight more uniform output.
    rayList' := sort rayList;
    coneList' := sort apply(coneList, sigma -> sort sigma);
    D := new ToricStack from {
	symbol map => betaMap,
    	symbol rays  => rayList',
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D.cache.NonStrict = opts.NonStrict;    D
    )

-----------------------------------------------------------------------------
---- TORIC STACK MAP TYPE DECLARATION
-----------------------------------------------------------------------------
ToricStackDatumMap = new Type of HashTable
ToricStackDatumMap.synonym = "toric stack datum map"


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
map(ToricStack, ToricStack, List) := ToricStackDatumMap => opts -> (D2, D1, A) -> (
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
    if (D2.map)*(bigPhi) != (D1.map)*(littlePhi) then
        error("-- expected maps to commute");
    new ToricStackDatumMap from {
    	symbol source => D1,
    	symbol target => D2,
    	symbol map => A,
    	symbol cache => new CacheTable}
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
bigPhi = matrix {{2,0},{0,2}}
littlePhi = matrix {{2,0},{0,2}}
A = {bigPhi, littlePhi};
map(D1,D1,A)

-----------------------------------------------------------------------------
---- Contructs from two matrices instead of a list of matrices
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, Matrix, Matrix) := ToricStackDatumMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
    A := {bigPhi, littlePhi};
    map(D2, D1, A)
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
bigPhi = matrix {{2,0},{0,2}}
littlePhi = matrix {{2,0},{0,2}}
map(D1,D1,bigPhi,littlePhi)

-----------------------------------------------------------------------------
---- Contructs either the zero-map or the map given by m*Id.
---- If m != 0 the source and target lattices must be the same.
-----------------------------------------------------------------------------
map(ToricStack, ToricStack, ZZ) := ToricStackDatumMap => opts -> (D2, D1, m) -> (
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
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
map(D1,D1,3)

-----------------------------------------------------------------------------
---- Defines the id map on a ToricStack
-----------------------------------------------------------------------------
ToricStack#id = D -> map(D,D,1)
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
id_(D1)



--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- WELL-DEFINED -----------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

isWellDefined ToricMap := Boolean => f -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys f;
    expectedKeys := set{symbol source, symbol target, symbol matrix, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
	    missing := toList(expectedKeys - K);
	    if #added > 0 then 
	        << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	        << "-- missing keys(s): " << toString missing << endl);
    	return false
	);
    --Check types
    if not instance(f.source, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `source' to be a NormalToricVariety" << endl);
	return false	);
    if not instance(f.target, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `target' to be a NormalToricVariety" << endl);
	return false
	);
    if not instance(f.matrix, Matrix) then (
	if debugLevel > 0 then (
	    << "-- expected `matrix' to be a Matrix" << endl);
	return false
	);
    if ring matrix f =!= ZZ then (
    	if debugLevel > 0 then (
	    << "-- expected `matrix' over the integers" << endl);
	return false
	);	 
    if not instance(f.cache, CacheTable) then (
    	if debugLevel > 0 then (
	    << "-- expected `f.cache' to be a CacheTable" << endl);
    	return false
	);
    --Check mathematical structure
    X := source f;
    Y := target f;
    A := matrix f;
    if rank source A =!= dim X then (
    	if debugLevel > 0 then (
	    << "-- expected number of columns of the matrix to equal the dimension of the source variety"
	    );
	return false
	);
    if rank target A =!= dim Y then (
    	if debugLevel > 0 then (
	    << "-- expected number of rows of the matrix to equal the dimension of the target variety"
	    );
	return false
	);
    V := transpose matrix rays X;
    if not all(max X, sigma -> any(max Y, tau -> (
		normals := outerMatrix outerNormals(Y, tau);
		innerProducts := normals * A * V_sigma;		
		all(flatten entries innerProducts, b -> b <= 0)))) then (
    	if debugLevel > 0 then (
	    << "-- expected image of each maximal cone to be contained in some maximal cone");
	return false
	);
    true
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----------------------------- BASICS -------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

-----------------------------------------------------------------------------
---- Defines source, target, and map of ToricStackDatumMap
-----------------------------------------------------------------------------
source ToricStackDatumMap := ToricStack => f -> f.source
target ToricStackDatumMap := ToricStack => f -> f.target
map ToricStackDatumMap := List => opts -> f -> f.map

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
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
ToricStackDatumMap == ToricStackDatumMap := Boolean => (f1, f2) -> (
    source f1 === source f2 and target f1 === target f2 and map f1 == map f2
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A)

bigPhi2 = matrix {{4,0},{0,4}}
littlePhi2 = matrix {{4,0},{0,4}}
A2 = {bigPhi2, littlePhi2};
f2 = map(D1,D1,A2)

f1 == f1
f2 == f2
f1 == f2

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
rankSource (ToricStackDatumMap) := (f) -> ({rank source (f.source).map, rank target (f.source).map})

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
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
rankTarget (ToricStackDatumMap) := (f) -> ({rank source (f.target).map, rank target (f.target).map})

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A)

rankTarget f1


-----------------------------------------------------------------------------
---- Defines compostion for ToricStackDatumMap
-----------------------------------------------------------------------------
ToricStackDatumMap * ToricStackDatumMap := ToricMapToricStackDatumMap => (f1, f2) -> (
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
D1 = toricStackDatum(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A1)

bigPhi2 = matrix {{4,0},{0,4}}
littlePhi2 = matrix {{4,0},{0,4}}
A2 = {bigPhi2, littlePhi2};
f2 = map(D1,D1,A2)

f2*f1
