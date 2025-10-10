--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- BASIC FUNCTIONS --------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
map ToricStack := Matrix => opts -> D -> D.map
rays ToricStack := List => {} >> o -> D -> D.rays
max  ToricStack := List => D -> D.max
fan ToricStack := Fan => D -> fan(D.rays, D.max)

isStrict = method()
isStrict ToricStack := Boolean => D -> (
    if isMember(NonStrict, keys D.cache) then (not D.cache.NonStrict) else (
         isFreeModule(target D.map) and rank((coker D.map) ** QQ) == 0
    )
)


-- The following is an attempt to implement Definition 2.20 in Geraschenko and Satriano. I think this will be needed later to compute unstable cones and compute direct complements of lattices and whatnot.

-- This takes in a map of ZZ-modules (abelian groups) and tries to compute the saturation of the image. In the case that this is an inclusion, its the saturation of the subgroup A inside of B.
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
    if p-q == 0 then return image matrix{{0}} else (
        L := inverse(P); -- this expresses generators of L = M + M', where the first q columns generate M and the last p-q columns generate M'
        return L_{q..p-1}
    )
)
directComplement(Module) := Matrix => M -> directComplement(gens M)

cokerMap := (A) -> (
    (prune coker A).cache.pruningMap
    )

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

-*
fanGensFromGeneralizedFan = method()
fanGensFromGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fan(rayList,coneList);
    L := cokerMap linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    {rayList', maxCones F}
    )

toricVarietyGeneralizedFan = method()
toricVarietyGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fanGensFromGeneralizedFan(rayList, coneList);
    normalToricVariety(F#0,F#1)
    )

rayList = {{0,0,1},{0,0,-1},{0,1,0},{1,0,0},{1,1,0}}
coneList = {{0,1,2,3},{0,1,3,4},{0,1,2,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)


rayList = {{1,1,1},{-1,-1,-1},{1,-1,0},{1,0,-1},{0,1,-1}}
coneList = {{0,1,2,3},{0,1,2,4},{0,1,3,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)
*-
