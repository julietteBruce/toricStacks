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
fantastack(List, List) := Fantastack => opts -> (rayList, coneList) -> toricStack(transpose matrix for ray in rayList list ray // gcd(ray), entries basis source(beta), coneList, opts)
fantastack(Fan) := Fantastack => opts -> Sigma -> fantastack(entries transpose rays Sigma, maxCones Sigma, opts)


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


