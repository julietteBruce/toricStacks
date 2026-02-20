-- This file has the main toricStack constructor. Mostly following GS
-- a toric stack (strict or possibly non-strict) is specified by a
-- stacky fan which has the following data:
--
-- N = lattice of rank d
-- Sigma = fan in N
-- L  = finitely generated abelian group
-- Beta = morphism N ---> L
--
-- This information is generally encoded in the following way:
-- rayList = list of the rays of Sigma each ray is represented by a list of d integers
-- coneList = list of indices specifying the maximal cones in Sigma
-- Q = presentation of L so Q:Z^s--->Z^t such that coker(Q) = L
-- B = lift of beta i.e. B:N ---> Z^t
--


validateFanMapCompatibility = (B, rayList) -> (
    if numColumns B !=  #(rayList#0) then error "Expect the source of lift of map to have same rank as lattice of fan";
    true
    )

validateStrictness = (B, Q, rayList) -> (
    if not isFreeModule(coker Q) then error "Is Non-Strict: Expected the target of map to be torsion-free";
    d := #(rayList#0);
    if numRows B != d or numColumns B != d then error "Is Non-Strict: Expected target and source to have same rank";
    if rank B != d then error "Is Non-Strict: Expected map to have finite co-kernel";
    true
    )


-----------------------------------------------------------------------------
---- toricStack constructor
-----------------------------------------------------------------------------

toricStack = method(Options => {
    CanonicalizeFan => true,
    CanonicalizeLight => false,
    CanonicalizeMap => true,
    Strict => false,
    CoefficientRing   => QQ,
    Variable          => getSymbol "x"}
);

-- This is the layout of constructors
--- #1 = (B, Q, rayList, coneList)
--- #2A = (B, Q, normalToricVariety)
--- #2B = (B, Q, Fan)
--- #3 = (beta, rayList, coneList)
--- #3A = (beta, normalToricVariety)
--- #3B = (beta, Fan)
--- #4 = (rayList, coneList)
--- #4A = (normalToricVariety)
--- #4B = (Fan)

-- Constructor #1
toricStack(Matrix, Matrix, List, List) := opts -> (B, Q, rayList, coneList) -> (
    (rayList', coneList') := fanDataFromAnything(rayList, coneList, opts);
    (B', Q') := mapDataFromAnything(B, Q, opts);
    buildToricStack(B', Q', rayList', coneList', opts)
);

toricStack(Matrix, Matrix, List, List) := opts -> (B, Q, rayList, coneList) -> (
    (rayList', coneList') := fanData(rayList, coneList,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    --
    (B', Q') := mapData(B, Q,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    --
    validateFanMapCompatibility(B', rayList');
    if opts.Strict == true then validateStrictness(B', Q', rayList');
    --
    D := new ToricStack from {
	symbol map => B',
	symbol presentation => Q',
	symbol rays => rayList',
	symbol max => coneList',
	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
)

-- Constructor #2A
toricStack(Matrix, Matrix, NormalToricVariety) := opts -> (B, Q, X) -> (
    (rayList', coneList') := fanData(X,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    (B', Q') := mapData(B, Q,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #2B
toricStack(Matrix, Matrix, Fan) := opts -> (B, Q, F) -> (
    (rayList', coneList') := fanData(F,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    (B', Q') := mapData(B, Q,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #3
toricStack(Matrix, List, List) := opts -> (beta, rayList, coneList) -> (
    (rayList', coneList') := fanData(rayList, coneList,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    (B', Q') := mapData(beta,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #3A
toricStack(Matrix, NormalToricVariety) := opts -> (beta, X) -> (
    (rayList', coneList') := fanData(X,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    (B', Q') := mapData(beta,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #3B
toricStack(Matrix, Fan) := opts -> (beta, F) -> (
    (rayList', coneList') := fanData(F,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    (B', Q') := mapData(beta,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #4
toricStack(List, List) := opts -> (rayList, coneList) -> (
    (rayList', coneList') := fanData(rayList, coneList,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    d := #(rayList'#0);
    B := id_(ZZ^d);
    (B', Q') := mapData(B,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #4A
toricStack(NormalToricVariety) := opts -> (X) -> (
    (rayList', coneList') := fanData(X,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    d := #(rayList'#0);
    B := id_(ZZ^d);
    (B', Q') := mapData(B,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )

-- Constructor #4B
toricStack(Fan) := opts -> (F) -> (
    (rayList', coneList') := fanData(F,
	CanonicalizeFan => opts.CanonicalizeFan,
	CanonicalizeLight => opts.CanonicalizeLight
	);
    d := #(rayList'#0);
    B := id_(ZZ^d);
    (B', Q') := mapData(B,
	CanonicalizeMap => opts.CanonicalizeMap
	);
    toricStack(B', Q', rayList', coneList', opts,
	CanonicalizeFan => false,
	CanonicalizeMap => false
	)
    )


