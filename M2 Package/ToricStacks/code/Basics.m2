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
---- Returns the fan of a toric stack.
-----------------------------------------------------------------------------
fan ToricStack := Fan => D -> (
    rayMat := (transpose matrix D.rays)**QQ;
    fan(rayMat, D.max)
    )

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


----------------------------------------------------------------------------
---- Cartesian Product 
-----------------------------------------------------------------------------
components ToricStack := List => D -> if D.cache.?components then D.cache.components else {D}

cartesianProduct ToricStack := D -> ToricStack.cartesianProduct (1 : D)

ToricStack.cartesianProduct = args -> (
    rayList := entries transpose directSum apply(args, D -> transpose matrix(D.rays));
    betaSum := directSum apply(args, D -> D.map);
    presentationSum := directSum apply(args, D -> D.presentation);
    --
    m := #((args#0).rays);
    coneList := (args#0).max;
    --
    for i from 1 to #args - 1 do (
        X := args#i;
        cones := apply(X.max, sigma -> apply(sigma, j -> j + m));
        m = m + #(X.rays);
        coneList = flatten table(coneList, cones, (sigma, tau) -> sigma | tau);
    );
    --
    toricStack(betaSum, presentationSum, rayList, coneList)
    )

ToricStack ** ToricStack := ToricStack => (D1,D2) -> (
    cartesianProduct (D1,D2)
    )

ToricStack ^** ZZ := ToricStack => (D, n) -> (
    if n <= 0 then error "-- expected a positive integer";
    cartesianProduct (n : D)
    )


----------------------------------------------------------------------------
---- Some extra tools to work with Toric Varieties
-----------------------------------------------------------------------------

--- This lists all the maximal cones of Sigma, but as cones instead of lists of indices. Somehow this is not a current method of Fan.
maxFacesAsCones = method()
maxFacesAsCones(Fan) := List => (Sigma) -> (
    V := entries transpose rays Sigma;
    (for maxCone in maxCones(Sigma) list (V_maxCone)) / transpose / matrix / coneFromVData
)

--- This checks if a map phi: Sigma1 --> Sigma2 sends cones to cones. This is a necessary condition for phi to be a morphism of toric varieties.
mapsConestoCones = method()
mapsConestoCones(Fan, Fan, Matrix) := Boolean => (Sigma2, Sigma1, phi) -> (
    all(apply(maxFacesAsCones(Sigma1), sigma -> (
        imagePhi := affineImage(phi, sigma);
        any(apply(maxFacesAsCones(Sigma2), tau -> contains(tau, imagePhi)), bool -> bool)
            )), bool -> bool)
    )

----------------------------------------------------------------------------
---- Saturation and Direct Complement
-----------------------------------------------------------------------------

-- This takes in a map of ZZ-modules (abelian groups) and tries to compute the saturation of the image. In the case that this is an inclusion, its the saturation of the subgroup A inside of B.
-- See Definition 2.20
saturation = method()
saturation(Matrix) := Module => M -> (
    if not ring source M === ZZ or not ring target M === ZZ then (
        error "expected a matrix between ZZ-modules"
        );
    (D,P,Q) := smithNormalForm M;    -- M2 returns (D,P,Q) with D = P*M*Q
    n := numrows M;
    r := rank M;
    E := matrix table(n, r, (i,j) -> if i == j then 1 else 0);
    Pinv := inverse P;               -- P is unimodular so inverse exists over ZZ
    image(Pinv * E)            -- columns generate the saturation of im M
)
isSaturated = method()
isSaturated(Matrix) := Boolean => M -> (
    M == gens saturation(M)
)

-- this takes a direct complement of a saturated submodule M of ZZ^n. It returns a matrix whose columns generate a direct complement. This will be used for the canonical stack.
directComplement = method()
directComplement(Matrix) := Matrix => M -> (
    if not isSaturated M then error("not saturated");
    (D,P,Q) := smithNormalForm(M); -- M2 returns (D,P,Q) with D = P*M*Q
    p := numrows P;
    q := numrows Q;
    if p-q == 0 then return ZZ^0 else (
        L := inverse(P); -- this expresses generators of L = M + M', where the first q columns generate M and the last p-q columns generate M'
        return L_{q..p-1}
    )
)
directComplement(Module) := Matrix => M -> directComplement(gens M)

-- This checks if a cone of a non-strick stacky fan is unstable using the second equivalent condition listed in Definition 6.2 of Geraschenko and Satriano. This says:
-- the relative interior of the image of tau in N/N_tor contains 0
isUnstable = method()
isUnstable(ToricStack, Cone) := Boolean => (D, tau) -> (
    (Sigma, beta) := (fan D, D.map);
    if not isStrict(D) then (
        beta = beta**QQ; -- restrict beta to free part to get rid of torsion.
    );
    zeroVector := map(ZZ^1, ZZ^(ambDim Sigma), 0);
    inInterior(zeroVector, affineImage(map D, tau))
)

unstableCones = method()
unstableCones(ToricStack) := List => D -> (
    (beta, Sigma) := (map D, fan D);
    select(maxFacesAsCones Sigma, tau -> isUnstable(D, tau))
)
-* Do we still need this? This was a clever way to find the coker map quickly....
cokerMap := (A) -> (
    (prune coker A).cache.pruningMap
    )
*-

getHilbRays = method()
getHilbRays(Cone) := List => sigma -> (
    hilbBasis := entries ((normaliz(transpose rays sigma, "integral_closure"))#"gen") ;
    hilbRays := apply(hilbBasis, b -> coneFromVData transpose matrix{b});
    hilbRays
)


