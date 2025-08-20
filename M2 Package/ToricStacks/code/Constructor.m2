needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

--------------------------- toricStackDatum ------------------------
--------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
--------------------------------------------------------------------
-------------------------------------------------------------------- 
toricStackDatum = method (
    TypicalValue => ToricStackDatum, 
    Options => {
    	CoefficientRing   => QQ,
    	Variable          => getSymbol "x",	  
	}
    )

 
toricStackDatum (Matrix, List, List) := opts -> (betaMap, rayList, coneList) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(coneList, sigma -> sort sigma);
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rayList,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
    )

toricStackDatum (Matrix, NormalToricVariety) := opts -> (betaMap,X) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(max X, sigma -> sort sigma); 
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rays X,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )

toricStackDatum (List, List) := opts -> (rayList, coneList) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(coneList, sigma -> sort sigma);
    dimTorus := #(rayList#0);
    betaMap := id_{ZZ^dimTorus};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rayList,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
    )

toricStackDatum (NormalToricVariety) := opts -> (X) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(max X, sigma -> sort sigma);
    dimTorus := #((rays X)#0);
    betaMap := id_{ZZ^dimTorusX};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rays X,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )

toricStackDatum (Fan) := opts -> (F) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(maxCones F, sigma -> sort sigma);
    dimTorus := rank transpose rays F;
    betaMap := id_{ZZ^dimTorusX};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => entries transpose rays F,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )
