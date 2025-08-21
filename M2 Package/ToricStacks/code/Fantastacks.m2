--- We can come later and separate this out into separate files.
load "./Constructors.m2"
topLevelMode = Standard

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
-- need to check isWellDefined....

max Fantastack := List => D -> D.max
rays Fantastack := List => {} >> o -> D -> D.rays
fan Fantastack := Fan => D -> fan(D.rays, D.max)

-- TODO: I want to pass along the options to toricStack

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
    D.cache#inputFan = fan(rayList, coneList);

    assert(isWellDefined D);
    D
)
fantastack(Matrix, Fan) := Fantastack => opts -> (beta, Sigma) -> fantastack(beta, entries transpose rays Sigma, maxCones Sigma, opts)

-- this is the canonical stack of a fan
-- The canonical stack only depends on the stack and its torus action, not on the stacky fan.
-- Over any non-strict toric stack, the canonical stack is isomorphic to it over its smooth locus.
fantastack(List, List) := Fantastack => opts -> (rayList, coneList) -> fantastack(transpose matrix for ray in rayList list ray // gcd(ray), rayList, coneList, opts)

fantastack(Fan) := Fantastack => opts -> Sigma -> fantastack(entries transpose rays Sigma, maxCones Sigma, opts)


canonicalStack = method(
    TypicalValue => Fantastack, 
    Options => {
        CoefficientRing   => QQ,
        Variable          => getSymbol "x",
        NonStrict         => false
    }   
)
canonicalStack(List, List) := Fantastack => opts -> (rayList, coneList) -> fantastack(rayList, coneList, opts)
canonicalStack(Fan) := Fantastack => opts -> Sigma -> fantastack(Sigma, opts)
--- TODO: ADD canonicalStack(ToricStack)



-- If we didn't save the input fan, we would need to compute some invariantRing stuff. 
moduliSpace = method()
moduliSpace(Fantastack) := NormalToricVariety => D -> (
    Sigma := D.cache#inputFan;
    rayList := rays Sigma;
    normalToricVariety( entries transpose rays Sigma, maxCones Sigma)
)


ring Fantastack := Ring => D -> ring moduliSpace D
-- get the irrelevant ideal of the moduliSpace, as defined in Remark 4.2
ideal Fantastack := Ideal => D -> ideal moduliSpace D


isWellDefined Fantastack := Boolean => D -> (
    betaImages := apply(D.rays, r -> D.map * transpose matrix{r});
    all({
        isWellDefined fan D,
        all(betaImages, b -> any(maxFacesAsCones(D.cache.inputFan), face -> contains(face, coneFromVData b)))-- this is checking that the image of each ray is actually contained in the imageFan.
    })
)

----- BASIC FUNCTIONS
-*
fan(List, List) := Fan => (V,F) -> (
    fan(apply(F, C -> transpose matrix apply(C, idx -> V_idx)) / coneFromVData)
)


maxFacesAsCones = method()
maxFacesAsCones(Fan) := List => (Sigma) -> (
    V := entries transpose rays Sigma;
    (for maxCone in maxCones(Sigma) list (V_maxCone)) / transpose / matrix / coneFromVData
)
*-

toricIdeal = method()
toricIdeal(Matrix,Ring) := (A,R) -> (
    m := product gens R;
    saturate(sub(toBinomial(transpose(syz(A)),R),R),m)
    )
toricIdeal(Matrix) := A -> (
    numcol := numColumns(A);
    p := local p;
    R := QQ[p_0..p_(numcol-1)];
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety, Ring) := Ideal => (X,R) -> toricIdeal(transpose matrix rays X, R)
toricIdeal(NormalToricVariety) := X -> toricIdeal(transpose matrix rays X, ring X)


toBinomial = method()
toBinomial(Matrix,Ring) := (M,S) -> (
     toBinom := (b) -> (
       pos := 1_S;
       neg := 1_S;
       scan(#b, i -> if b_i > 0 then pos = pos*S_i^(b_i)
                   else if b_i < 0 then neg = neg*S_i^(-b_i));
       pos - neg);
     ideal apply(entries M, toBinom)
     )