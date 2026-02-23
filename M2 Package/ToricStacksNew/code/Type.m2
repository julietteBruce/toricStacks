--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- CREATE TYPE ------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--- kludge to access parts of the 'Core'
--hasAttribute = value Core#"private dictionary"#"hasAttribute";
--getAttribute = value Core#"private dictionary"#"getAttribute";
--ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";


-----------------------------------------------------------------------------
-- Stack  TYPE DECLARATION
-----------------------------------------------------------------------------

Stack = new Type of MutableHashTable
Stack.synonym = "stack"
Stack.GlobalAssignHook = globalAssignFunction
Stack.GlobalReleaseHook = globalReleaseFunction

-----------------------------------------------------------------------------
-- TORIC STACK TYPE DECLARATION
-----------------------------------------------------------------------------

ToricStack = new Type of Stack
ToricStack.synonym = "toric stack"
ToricStack.GlobalAssignHook = globalAssignFunction
ToricStack.GlobalReleaseHook = globalReleaseFunction
expression ToricStack := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
   (describe D)#0
describe ToricStack := D -> Describe (expression toricStack) (
    expression D.map, expression D.presentation, expression D.rays, expression D.max)


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
    expectedKeys := set {symbol map, symbol presentation, symbol rays, symbol max, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if #added > 0 then 
	    << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	    << "-- missing keys(s): " << toString missing << endl;
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
    -- check B map
    if not instance(D.map, Matrix) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a matrix" << endl;
	return false
	);
     if (not ring source D.map === ZZ) or (not ring target D.map === ZZ) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a map of ZZ modules" << endl;
	return false
	);
    if (not numColumns D.map == #((D.rays)#0)) then (
	    if debugLevel > 0 then
	        << "-- expected source of `map' to be equal to lattice of fan" << endl;
	    return false
	    );
   if not isFreeModule(source D.map) then (
	if debugLevel > 0 then
	    << "-- expected `map' to be a map from a lattice, a free ZZ module." << endl;
	return false
	);
    -- check Q map
    if not instance(D.presentation, Matrix) then (
	if debugLevel > 0 then 
	    << "-- expected `presentation' to be a matrix" << endl;
	return false
	);
    if (not ring source D.presentation === ZZ) or (not ring target D.presentation === ZZ) then (
	if debugLevel > 0 then 
	    << "-- expected `presentation' to be a map of ZZ modules" << endl;
	return false
	);
    -- compatibility of B and Q maps
    if not numRows D.map == numRows D.presentation then (
	if debugLevel > 0 then 
	    << "-- expected `map' and 'presentation' to have same target" << endl;
	return false
	);
    return true
)
