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

-----------------------------------------------------------------------------
-- ToricStackMap TYPE DECLARATION
-----------------------------------------------------------------------------

ToricStackMap = new Type of HashTable
ToricStackMap.synonym = "toric stack map"
-- TODO: Add description and expression functions


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
	if debugLevel > 0 then
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
