--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- BASIC FUNCTIONS --------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

----------------------------------------------------------------------------
--- These are basic functions that basically allow one to call the keys
--- of ToricStack as function.
-----------------------------------------------------------------------------
map ToricStack := Matrix => opts -> D -> D.map
rays ToricStack := List => {} >> o -> D -> D.rays
max  ToricStack := List => D -> D.max
fan ToricStack := Fan => D -> fan(D.rays, D.max)
presentation ToricStack := Matrix =>  D -> D.presentation


----------------------------------------------------------------------------
---- Tests whether a toric stack is strict.
-----------------------------------------------------------------------------
isStrict = method()
isStrict(ToricStack) := Boolean => D -> (
    if D.cache#?Strict then return D.cache#Strict;   -- return cached result
    ---
    B := D.map;
    Q := D.presentation;
    rayList := D.rays;
    --
    result := (
	if not isFreeModule(coker Q) then false
	else if rank ((B|Q)**QQ) != numRows (B|Q) then false
	else true
	);
    D.cache#Strict = result;
    --
   result
)


-*
----------------------------------------------------------------------------
---- Cartesian Product 
-----------------------------------------------------------------------------
cartesianProduct NormalToricVariety := X -> NormalToricVariety.cartesianProduct (1 : X)
NormalToricVariety.cartesianProduct = args -> (
    rayList := entries transpose directSum apply(args, X -> transpose matrix rays X);
    betaSum := directSum apply(args, X -> map X);
    m := # rays args#0;
    coneList := max args#0;
    for i from 1 to #args - 1 do (
	X := args#i;
	cones := apply (max X, sigma -> apply (sigma, j -> j + m));
	m = m + #rays X;
	coneList = flatten table(coneList, cones, (sigma, tau) -> sigma | tau);
	);
    normalToricVariety (rayList, coneList, betaSum,
	CoefficientRing => coefficientRing ring args#0,
	--Variable => opts.Variable,
	NonStrict => any(args, X -> (isStrict X)==false)
	)
    )

ToricStack ** ToricStack := ToricStack => (X,Y) -> (
    cartesianProduct (X,Y))

ToricStack ^** ZZ := ToricStack => (X, n) ->  (
    if n <= 0 then error "-- expected a positive integer";
    cartesianProduct (n : X)
    )
*-

----------------------------------------------------------------------------
---- dimension: dim [U/G] = dim U - dim G
-----------------------------------------------------------------------------
dim(ToricStack) := D -> (
    if D.cache#?dim then return D.cache#dim;
    dimU := #((D.rays)#0);
    dimG := dim coxGroup(D);
    dimD := dimU - dimG;
    D.cache#dim = dimD;
    dimD
    )






