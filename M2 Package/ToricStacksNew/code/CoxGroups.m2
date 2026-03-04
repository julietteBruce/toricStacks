
-----------------------------------------------------------------------------
-- Computes the Cox Group of a toric stack, returned as a DiagonalizableGroup
-----------------------------------------------------------------------------
coxGroup = method(Options => {});
coxGroup(Matrix, Matrix) := opts -> (B, Q) -> (
    validateMapData(B,Q);
    --
    phi := transpose (B|Q);
    --
    diagonalizableGroup(phi)
    )-

coxGroup(Matrix) := opts -> (B) -> (
    if not (ring B === ZZ) then error "Expected a ZZ-linear map or matrix";
    if not isFreeModule(source B) then error "Expected the source to be a free ZZ-module";
    if isFreeModule(target B) then (
	r := numRows B;
	Q := map(ZZ^r,ZZ^0,0);
	coxGroup(B,Q)
	)
    else (
	L := target B;
	P := coverMap L;
	Q := matrix presentation L;
	coxGroup(B//P,Q)
	)
    )

coxGroup(ToricStack) := opts -> (D) -> (
    if D.cache#?CoxGroup then return D.cache#CoxGroup; 
    result := coxGroup(D.map,D.presentation);
    D.cache#Strict = result;
    --
    result
    )


