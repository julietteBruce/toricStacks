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

ToricStackDatum = new Type of Stack
ToricStackDatum.synonym = "toric stack datum"
ToricStackDatum.GlobalAssignHook = globalAssignFunction
ToricStackDatum.GlobalReleaseHook = globalReleaseFunction
expression ToricStackDatum := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
   (describe D)#0
describe ToricStackDatum := D -> Describe (expression toricStackDatum) (
    expression D.map, expression D.rays, expression D.max)



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
    TypicalValue => ToricStackDatum, 
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
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rayList',
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D.cache.NonStrict = opts.NonStrict;
    D
    )

ToricStackDatumMap = new Type of HashTable
ToricStackDatumMap.synonym = "toric stack datum map"


source ToricStackDatumMap := ToricStackDatum => f -> f.source
target ToricStackDatumMap := ToricStackDatum => f -> f.target
map ToricStackDatumMap := List => opts -> f -> f.map



map(ToricStackDatum, ToricStackDatum, List) := ToricStackDatumMap => opts -> (D2, D1, A) -> (
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

map(ToricStackDatum, ToricStackDatum, Matrix, Matrix) := ToricStackDatumMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
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

map(ToricStackDatum, ToricStackDatum, ZZ) := ToricStackDatumMap => opts -> (D2, D1, m) -> (
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


ToricStackDatum#id = D -> map(D,D,1)
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStackDatum(betaMap, rayList, coneList)
id_(D1)
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

ToricStackDataum#id = D -> map(D,D,{1)


ToricStackDatumMap == ToricStackDatumMap := Boolean => (f, g) -> (
    source f === source g and target f === target g and map f == map g
    )
