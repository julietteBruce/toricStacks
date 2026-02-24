weightedProjectiveStack = method (
    TypicalValue => ToricStack, 
    Options => {
    	CoefficientRing   => QQ,
    	Variable          => getSymbol "x"
	}
    );

weightedProjectiveStack (List) := opts -> (weights) -> (
    if #weights <= 1 then error "Need at least 2 weights.";
    if any(weights, w -> not instance(w,ZZ)) then error "All weights must be integers.";
    if any(weights, w -> w <= 0) then error "All weights must be positive integers.";
    --
    n := #weights;
    --
    rayList := entries id_(ZZ^n);
    coneList := apply(n, i->({i}));
    --
    B := id_(ZZ^n);
    Q := matrix apply(weights, w -> {w});
    --
    toricStack(B,Q, rayList, coneList,
	CoefficientRing => opts.CoefficientRing,
	Variable => opts.Variable
	)
    )

