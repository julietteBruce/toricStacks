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
-- Stack  TYPE DECLA
-----------------------------------------------------------------------------

Stack = new Type of MutableHashTable
Stack.synonym = "stack"
Stack.GlobalAssignHook = globalAssignFunction
Stack.GlobalReleaseHook = globalReleaseFunction

-----------------------------------------------------------------------------
-- TORIC STACK TYPE DECLaRATION
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


--------------------------- isWellDefined ------------------------
--------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
--------------------------------------------------------------------
-------------------------------------------------------------------- 
isWellDefined ToricStack := Boolean => D -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys D;
    expectedKeys := set {symbol map, symbol rays, symbol max, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if #added > 0 then 
	    << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	    << "-- missing keys(s): " << toString missing << endl
	    );	 
    	return false
    	);
    -- check toric variety
    X := normalToricVariety(D.rays,D.max);
    if not isWellDefined X then (
	if debugLevel > 0 then 
	    << "--  `rays' and `max' do not give well-defined normal toric variety." << endl;
	return false
	);
    if not instance(D.map, Matrix) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a matrix" << endl;
	return false
	);
     if (ring source D.map != ZZ) or (ring target D.map != ZZ) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a map of ZZ modules" << endl;
	return false
	);
    if not isFreeModule(source D.map) or not isFreeModule(target D.map) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a map of free ZZ modules" << endl;
	return false
	);
	)
