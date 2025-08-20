needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

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

map(ToricStackDatum, ToricStackDatum, Matrix, Matrix) := ToricStackDatumMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
    A := {bigPhi, littlePhi};
    map(D2, D1, A)
    )

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

ToricMap * ToricMap := ToricMap => (g, f) -> (
    if target f =!= source g then error "-- expected composable maps";
    new ToricMap from {
    	symbol source => source f,
    	symbol target => target g,
    	symbol matrix => (matrix g) * (matrix f),
    	symbol cache => new CacheTable
	}
    )

ToricMap == ToricMap := Boolean => (f, g) -> (
    source f === source g and target f === target g and matrix f == matrix g
    )
