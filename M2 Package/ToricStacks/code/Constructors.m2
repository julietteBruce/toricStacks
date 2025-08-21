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
ToricStack.synonym = "toric stack"
ToricStack.GlobalAssignHook = globalAssignFunction
ToricStack.GlobalReleaseHook = globalReleaseFunction
expression ToricStack := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
   (describe D)#0
describe ToricStack := D -> Describe (expression toricStack) (
    expression D.map, expression D.rays, expression D.max)

-----------------------------------------------------------------------------
----- toricStack
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
----------------------------------------------------------------------------- 
toricStack = method (
    TypicalValue => ToricStack, 
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
---- coneList and then call this main version of toricStack. This is
---- for consistentcy and easy of debugging. 
-----------------------------------------------------------------------------
toricStack (Matrix, List, List) := opts -> (betaMap, rayList, coneList) -> (
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
    D.cache.NonStrict = opts.NonStrict;
    D
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
toricStack(betaMap, rayList, coneList)

-----------------------------------------------------------------------------
---- Contructs from NormalToricVariety
-----------------------------------------------------------------------------
toricStack (Matrix, NormalToricVariety) := opts -> (betaMap,X) -> (
    toricStack(betaMap, rays X, max X,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
X = normalToricVariety(rayList,coneList)
toricStack(betaMap,X)

-----------------------------------------------------------------------------
---- Contructs from a Fan
-----------------------------------------------------------------------------
toricStack (Matrix, Fan) := opts -> (betaMap, F) -> (
    rayList := entries transpose rays F;
    coneList := maxCones F;
    toricStack(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

betaMap = matrix {{1,0},{1,2}}
C1 = coneFromVData matrix {{1,0},{0,1}}
F = fan C1
toricStack(betaMap, F)

-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a list of rays and a list of maximal
---- cones) as a toricStack by taking betaMap to be the identiy on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStack (List, List) := opts -> (rayList, coneList) -> (
    dimFanTorus := #(rayList#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStack(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

rayList = {{1,0},{0,1}}
coneList = {{0,1}}
toricStack(rayList, coneList)

-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a NormalToricVariety)
---- as a toricStack by taking betaMap to be the identiy on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStack (NormalToricVariety) := opts -> (X) -> (
    dimFanTorus :=  #((rays X)#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStack(betaMap, rays X, max X,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )

rayList = {{1,0},{0,1}}
coneList = {{0,1}}
X = normalToricVariety(rayList,coneList)
toricStack(X)


-----------------------------------------------------------------------------
---- Realizes a toric variety (given as a Fan)
---- as a toricStack by taking betaMap to be the identity on the
---- lattice of the toric variety.
-----------------------------------------------------------------------------
toricStack (Fan) := opts -> (F) -> (
    rayList := entries transpose rays F;
    coneList := maxCones F;
    dimFanTorus := #(rayList#0);
    betaMap := id_(ZZ^dimFanTorus);
    toricStack(betaMap, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )



-----------------------------------------------------------------------------
----- weightProjectiveStack
-----------------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

weightedProjcetiveStack = method (
    TypicalValue => ToricStack, 
    Options => {
    	CoefficientRing   => QQ,
    	Variable          => getSymbol "x",
	NonStrict        => false
	}
    )

weightedProjcetiveStack (List) := opts -> (betaMap,X) -> (
    
    toricStack(betaMap, rays X, max X,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable,
	NonStrict => opts.NonStrict
	)
    )


C1 = coneFromVData matrix {{1,0},{0,1}}
F = fan C1
toricStack(betaMap, F)

load "./Basics.m2"
