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
	NonStrict        => false
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

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
toricStackDatum(betaMap, rayList, coneList)

-----------------------------------------------------------------------------
---- Contructs from NormalToricVariety
-----------------------------------------------------------------------------
toricStackDatum (Matrix, NormalToricVariety) := opts -> (betaMap,X) -> (
    toricStackDatum(betaMap, rays X, max X,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
X = normalToricVariety(rayList,coneList)
toricStackDatum(betaMap,X)

-----------------------------------------------------------------------------
---- Contructs from a Fan
-----------------------------------------------------------------------------
toricStackDatum (Matrix, Fan) := opts -> (betaMap, F) -> (
    rayList := entries transpose rays F;
    coneList := maxCones F;
    toricStackDatum(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

betaMap = matrix {{1,0},{1,2}}
C1 = coneFromVData matrix {{1,0},{0,1}}
F = fan C1
toricStackDatum(betaMap, F)

-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a list of rays and a list of maximal
---- cones) as a toricStackDatum by taking betaMap to be the identiy on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStackDatum (List, List) := opts -> (rayList, coneList) -> (
    dimFanTorus := #(rayList#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStackDatum(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

rayList = {{1,0},{0,1}}
coneList = {{0,1}}
toricStackDatum(rayList, coneList)

-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a NormalToricVariety)
---- as a toricStackDatum by taking betaMap to be the identiy on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStackDatum (NormalToricVariety) := opts -> (X) -> (
    dimFanTorus :=  #((rays X)#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStackDatum(betaMap, rays X, max X,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

rayList = {{1,0},{0,1}}
coneList = {{0,1}}
X = normalToricVariety(rayList,coneList)
toricStackDatum(X)


-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a Fan)
---- as a toricStackDatum by taking betaMap to be the identiy on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStackDatum (Fan) := opts -> (F) -> (
    rayList := entries transpose rays F;
    coneList := maxCones F;
    dimFanTorus := #(rayList#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStackDatum(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

C1 = coneFromVData matrix {{1,0},{0,1}}
F = fan C1
toricStackDatum(betaMap, F)
