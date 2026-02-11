-*
This file contains code to implement the canonical stack construction from Section 5 of Gareschencko and Satriano. 

Given any non-strict toric state, there is a canonical smooth nonstrict toric stack of which it is a good moduli space. 
Intuitively, the canonical stack can be regarded as a canonical stacky resolution of singularities.

-- The canonical stack only depends on the stack and its torus action, not on the stacky fan.
-- Over any non-strict toric stack, the canonical stack is isomorphic to it over its smooth locus.
*-


canonicalStack = method(
    TypicalValue => ToricStack,
    Options => {

    }
)
canonicalStack(ToricStack) := ToricStack => opts -> D -> (
    if isMember(CanonicalStack, keys D.cache) then return (D.cache)#CanonicalStack else (
        M := saturation matrix rays D;
        M' := directComplement M;
        L' := directSum(image matrix rays D, M'); --- this is the lattice that the fan of the canonical stack will lie inside

        canonicalRayList := entries gens L';
        canonicalConeList := for C in max D list (
            coneC := coneFromVData transpose matrix apply(C, idx -> D.rays_idx);
            conePositions := positions(canonicalRayList, ei -> contains(coneC, D.map * transpose matrix{ei}));
            if conePositions == {} then error "Every ray of fan should contain contain the image of a standard basis vector under the map provided.";
            conePositions
        );
        -- this might be expensive to compute, so maybe we want to cache phi.
        Phi := directSum(transpose matrix for ray in D.rays list ray // gcd(ray), id_(M'));
        -- In the notation of section 5, this is the matrix given by u_p's, together with the identity on M'

        canonicalD := toricStack(D.map*Phi, canonicalRayList, canonicalConeList, opts);
        D.cache#CanonicalStack = canonicalD;
        canonicalD.cache#CanonicalPhi = Phi;
        canonicalD.cache#InputStack = D;
        canonicalD
    )
)


canonicalStackMap = method(
    TypicalValue => ToricStackMap,
    Options => {

    }
)
canonicalStackMap(ToricStack) := ToricStackMap => opts -> D -> (
    canonicalD := canonicalStack(D, opts);
    map(D, canonicalD, canonicalD.cache#CanonicalPhi, id_(target D.map), opts) 
)

-*
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D = toricStack(betaMap, rayList, coneList)
canonicalStack(D)
canonicalStackMap(D)
*-