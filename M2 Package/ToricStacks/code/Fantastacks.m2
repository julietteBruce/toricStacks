Fantastack = new Type of ToricStack
Fantastack.synonym = "fantastack"
Fantastack.GlobalAssignHook = globalAssignFunction
Fantastack.GlobalReleaseHook = globalReleaseFunction


fantastack = method(
    TypicalValue => Fantastack, 
    Options => {
        CoefficientRing   => QQ,
        Variable          => getSymbol "x",
        NonStrict         => false
    }   
)
-- isWellDefined is inherited from ToricStack!

max Fantastack := List => D -> D.max
rays Fantastack := List => {} >> o -> D -> D.rays
fan Fantastack := Fan => D -> fan(D.rays, D.max) -- should save this, it takes awhile to make a fan.

---------------------------------------------------------------------------
-*
Explanation: A fantastack is a broad class of smooth toric stacks which are easy to handle because the fan on L is completely induced by a fan on N. All of the following is implemented following Section 4 of Geraschenko and Satriano's "Toric Stacks I: The Theory of Stacky Fans". The input is a fan Sigma on a lattice N, and map beta: Z^n --> N such that:
- every ray of Sigma contains some beta(e_i) 
- every beta(e_i) is in the support of Sigma
The toric stack is then (\hat{Sigma}, beta), where \hat{Sigam} is induced by Sigma and beta: the cones exactly \hat{sigma} = {e_i | beta(e_i) in sigma}.

Input: a matrix beta, a list of rays of Sigma, and list of cones of Sigma.
Output: a Fantastack D.

The inputFan is also saved in the cache under the symbol InputFan.
*-
-----------------------------------------------------------------------------
fantastack(Matrix, List, List) := Fantastack => opts -> (beta, rayList, coneList) -> (
    E := basis source(beta);

    fantastackRayList := entries E;
    fantastackConeList := for C in coneList list (
        coneC := coneFromVData transpose matrix apply(C, idx -> rayList_idx);
        conePositions := positions(fantastackRayList, ei -> contains(coneC, beta * transpose matrix{ei}));
        if conePositions == {} then error "Every ray of fan should contain contain the image of a standard basis vector under the map provided.";
        conePositions
    );
    D := new Fantastack from toricStack(beta, fantastackRayList, fantastackConeList, opts);
    D.cache#InputFan = fan(rayList, coneList);

    assert(isWellDefined D);
    assert(isStrict D);
    D
)
fantastack(Matrix, Fan) := Fantastack => opts -> (beta, Sigma) -> fantastack(beta, entries transpose rays Sigma, maxCones Sigma, opts)




-* 
If you do not provide a beta map, there is a canonical choice: the map that sends each standard basis vector to the primitive generator of the ray it is mapped to. This is the canonicalStack.
*-

-- this is the canonical stack of a fan
-- The canonical stack only depends on the stack and its torus action, not on the stacky fan.
-- Over any non-strict toric stack, the canonical stack is isomorphic to it over its smooth locus.

---------------------------------------------------------------------------
-*
Explanation: If you do not provide a beta map (only a Fan in N), there is a canonical choice for beta: the map that sends each standard basis vector to the primitive generator of the ray it is mapped to. This is the canonicalStack.

Input: a list of rays of Sigma, and list of cones of Sigma.
Output: a Fantastack D.

*-
-----------------------------------------------------------------------------
canonicalStack = method(
    TypicalValue => Fantastack, 
    Options => {
        CoefficientRing   => QQ,
        Variable          => getSymbol "x",
        NonStrict         => false
    }   
)
canonicalStack(List, List) := Fantastack => opts -> (rayList, coneList) -> fantastack(transpose matrix for ray in rayList list ray // gcd(ray), rayList, coneList, opts)
canonicalStack(Fan) := Fantastack => opts -> Sigma -> canonicalStack(entries transpose rays Sigma, maxCones Sigma, opts)
canonicalStack(NormalToricVariety) := Fantastack => opts -> X -> canonicalStack(fan X)
canonicalStack(Fantastack) := Fantastack => opts -> D -> (
    Sigma := fan D;
    canonicalStack(Sigma, opts)
)
fantastack(List, List) := Fantastack => opts -> (rayList, coneList) -> canonicalStack(rayList, coneList, opts)
fantastack(Fan) := Fantastack => opts -> Sigma -> canonicalStack(Sigma, opts)

--- TODO: ADD canonicalStack(ToricStack) 
-*canonicalStack(ToricStack) := Fantastack => opts -> D -> (

)*-

-- TODO: Implement == of toricStacks using Theorem B.3 
-*
canonicalStackMorphism = method()
canonicalStackMorphism(Fantastack) = ToricStackMap => opts -> (D) -> (
    canonicalD := canonicalStack(D, opts);
    N := target map canonicalD;
    toricStackMap(moduliSpace(canonicalD),canonicalD,,map(N,N, 1), opts)
) *-


---------------------------------------------------------------------------
-*
Explanation: A smooth toric stack D is a fantastack iff it has a toric variety X as a good moduli space and the morphism D --> X restricts to an isomorphism of tori. In particular, the fantastack D has the toric variety constructed from the input fan (Sigma on N) as a good moduli space, of which there may be many.

Input: a Fantastack D
Output: the toric variety constructed from the input fan, a good moduli space of D.
*-
-----------------------------------------------------------------------------
moduliSpace = method()
moduliSpace(Fantastack) := NormalToricVariety => D -> (
    -- If we didn't save the input fan, we would need to compute some invariantRing stuff. 
    Sigma := D.cache#InputFan;
    rayList := rays Sigma;
    normalToricVariety( entries transpose rays Sigma, maxCones Sigma)
) -- TODO: should return the toricStackMap


ring Fantastack := Ring => D -> ring moduliSpace D
-- get the irrelevant ideal of the moduliSpace, as defined in Remark 4.2
ideal Fantastack := Ideal => D -> ideal moduliSpace D


------ TODO: I should actually just allow isWellDefined to be inherited from ToricStack, along with some extra checks.
-*
isWellDefined Fantastack := Boolean => D -> (
    betaImages := apply(D.rays, r -> D.map * transpose matrix{r});
    all({
        isWellDefined fan D,
        all(betaImages, b -> any(maxFacesAsCones(D.cache.InputFan), face -> contains(face, coneFromVData b)))-- this is checking that the image of each ray is actually contained in the imageFan.
    })
) *-

isSmooth(Fantastack) := Boolean => D -> true -- all fantastacks are smooth


-- TODO: A function that returns the unstable cones of a fantastack (see Definition 6.2)
-*
unstableCones = method()
unstableCones(Fantastack) := List => D -> (
    (beta, Sigma) := (map D, fan D);
*-