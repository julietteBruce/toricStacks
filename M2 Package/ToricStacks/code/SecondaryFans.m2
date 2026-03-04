needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

cokerMap := (A) -> (
    --(prune coker A).cache.pruningMap
    transpose gens ker transpose A
    )

concatenateMatrices := (L) -> (
    if #L === 1 then (return L#0);
    A := L#0;
    apply(1..#L-1,i->(A = A|L#i));
    A
    )


galeDual = method()
galeDual (Matrix) := (A) -> (
    transpose mingens ker A
    )

galeDual (List) := (rayList) -> (
    galeDual(transpose (matrix rayList))
    )

fanGensFromGeneralizedFan = method()
fanGensFromGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fan(rayList,coneList);
    L := cokerMap linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    {rayList', maxCones F}
    )

fanGensFromGeneralizedFan (Matrix, List) := (rayMatrix, coneList) -> (
    F := fan(rayMatrix,coneList);
    L := cokerMap linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    {rayList', maxCones F}
    )

toricVarietyGeneralizedFan = method()
toricVarietyGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fanGensFromGeneralizedFan(rayList, coneList);
    normalToricVariety(F#0,F#1)
    )
-*
rayList = {{0,0,1},{0,0,-1},{0,1,0},{1,0,0},{1,1,0}}
coneList = {{0,1,2,3},{0,1,3,4},{0,1,2,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)


rayList = {{1,1,1},{-1,-1,-1},{1,-1,0},{1,0,-1},{0,1,-1}}
coneList = {{0,1,2,3},{0,1,2,4},{0,1,3,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)
*-


SecondaryFan = new Type of Fan
SecondaryFan.synonym = "secondary fan"
SecondaryFan.GlobalAssignHook = globalAssignFunction
SecondaryFan.GlobalReleaseHook = globalReleaseFunction

SecondaryFan#(symbol inputRays) = null

secondaryFan = method(
    TypicalValue => SecondaryFan, 
    Options => {
        gkzGenFans   => false,
	gkzStacks => false,
    }   
)

secondaryFan(Matrix) := SecondaryFan => opts -> (rayInputMatrix) -> (
    -- Find the gale dual of our input rays.New rays are the cols still.
    galeRayMatrix := galeDual(rayInputMatrix);
    -- compute the secondary fan via ccr
    F := new SecondaryFan from ccRefinement(galeRayMatrix);
    F#(symbol inputRays) = rayInputMatrix;
     F.cache.galeDualMatrix = galeRayMatrix;
    if opts.gkzGenFans == true or opts.gkzStacks == true then (
	n := numcols rayInputMatrix;
	-- a splitting of the galeMatrix for our inputRays,
	-- will be used to lift from the secondary fan to ZZ^(Rays)
	splitGaleMatrix := id_(target F.cache.galeDualMatrix)//(F.cache.galeDualMatrix);
	-- non-empty faces of secondary fan (empty face would cause errors)
	faceSecFan := delete({},flatten values faces F);
	-- temp mutable hashes
	gkzHash := new MutableHashTable;
	stackHash := new MutableHashTable;
	apply(sort faceSecFan, f -> (
		-- get cone Gamma in secondary fan from list of rays f
		gammaMatrix := (rays F)_(f);
		m := numcols gammaMatrix;
		-- find point in the relative interior of Gamma and lift
		-- from secondary fan to ZZ^(Rays) with choosen splitting
		allOnesMatrix := matrix toList (m:{1});
		ptInRelInt :=  gammaMatrix*allOnesMatrix;
		ptLift := splitGaleMatrix*ptInRelInt;
		-- compute the gkz fan for Gamma 
		gammaFan' := regularSubdivision(rayInputMatrix, transpose ptLift);
		-- sorting for some semi-consistency
		gammaFan := apply(sort gammaFan', i -> sort i);
		irrRays := sort toList set(0..n-1) - set flatten gammaFan;
		gkzHash#f = (gammaFan,irrRays);
		-- this subroutine will compute the stacks if desired once implemented. 
		if opts.gkzStacks == true then (
		    stackHash#f = "This is where the stack will go";
		    );
		));
	F.cache.gkzGenFans = new HashTable from gkzHash;
	if opts.gkzStacks == true then (F.cache.gkzStacks = new HashTable from stackHash);
	);
    F
    )

gkzGeneralizedFan = method()
gkzGeneralizedFan (Matrix, Matrix, Matrix) := (rayInputMatrix, gammRayMatrix, galeDualMatrix) -> (
    n := numcols rayInputMatrix;
    m := numcols gammRayMatrix;
    -- find point in the relative interior of Gamma and lift
    -- from secondary fan to ZZ^(Rays) with choosen splitting
    allOnesMatrix := matrix toList (m:{1});
    ptInRelInt :=  gammRayMatrix*allOnesMatrix;
    splitGaleMatrix := id_(target galeDualMatrix)//(galeDualMatrix);
    ptLift := splitGaleMatrix*ptInRelInt;
    -- compute the gkz fan for Gamma 
    gammaFan' := regularSubdivision(rayInputMatrix, transpose ptLift);
    -- sorting for some semi-consistency
    gammaFan := apply(sort gammaFan', i -> sort i);
    irrRays := sort toList set(0..n-1) - set flatten gammaFan;
    (gammaFan,irrRays)
    )

gkzGeneralizedFan (Matrix, Matrix) := (rayInputMatrix, gammRayMatrix) -> (
    galeDualMatrix := galeDual(rayInputMatrix);
    gkzGeneralizedFan(rayInputMatrix, gammRayMatrix, galeDualMatrix)
    )
    
tildeL = method()
tildeL (Matrix, Sequence) := (rayInputMatrix, gkzGenFanData) ->(
    gkzGenFanGamma := gkzGenFanData#0;
    irrRaysList := gkzGenFanData#1;
    -- set-up
    gkzGammaAsFan := fan(rayInputMatrix, gkzGenFanGamma); -- CAN WE AVOID THIS
    tildeN := source rayInputMatrix;
    -- map from N to N_Gamma
    fGamma := cokerMap linealitySpace gkzGammaAsFan;
    -- map from tildeN to N_gamma
    FGamma := fGamma*rayInputMatrix;
    nonemptyCones := delete({},flatten values faces gkzGammaAsFan);
    -- the irrelevant rays contribute the same to each lambda space
    -- we will convert the indices to rays in tildeN later
    irrIndices := toList set(0..numcols rayInputMatrix-1) - ((set flatten maxCones gkzGammaAsFan) + (set irrRaysList));
    -- computing the Lambda_tau space for each face tau of gamma
    -- then compute the kernel of fTildeGamma restricted to Lambda_tau
    lambdaKernels := apply(nonemptyCones, tau->(
	    -- these lines find the indices of the rays that span Lambda_tau
	    tauRayIndices := flatten tau;
	    tauLambdaIndices := unique(tauRayIndices | irrIndices);
	    -- convert the indices of the rays spanning Lambda_tau to vectors in tildeN
	    tayLambdaVectors := apply(tauLambdaIndices, i->(matrix( (id_(tildeN))_i)));
	    -- convert into a matrix whose columns are the vectors spanning Lambda_tau
	    -- i.e. the image is the Lambda_tau subspace of tildeN
	    tauLambdaMatrix := concatenateMatrices(tayLambdaVectors);
	    -- To comptue the kernel of FGamma we use the fact that
	    -- g(ker(f*g) = ker(f|_img(g)))
	    -- so we compute kernel of FGamma*tauLambdaMatrix and then go back
	    kerFGammaResTau := ker(FGamma*tauLambdaMatrix);
	    tauLambdaMatrix*(gens kerFGammaResTau)
	    ));
    -- concatenate all the lambdaKernels to find tildeL
    concatenateMatrices lambdaKernels
    )

tildeL (Matrix, Matrix) := (rayInputMatrix, gammaRayMatrix) -> (
        gzkGenFanData := gkzGeneralizedFan(rayInputMatrix);
	tildeL(rayInputMatrix, gzkGenFanData)
	)
   

bettaGamma = method()
bettaGamma (Matrix, Sequence) := (rayInputMatrix, gzkGenFanData) ->(
    -- Construct the tilde-L subspace and the cokernel map called tildeFGamma
    tildeLSubspace := tildeL(rayInputMatrix,gzkGenFanData);
    tildeFGamma := cokerMap tildeLSubspace;
    -- We now have the short exact sequences
    --
    --- 0----> tildeL_Gamma ---> tildeN ---- tildeFGamma --->> tildeN_Gamma ----> 0
    ---                            |
    ---                            |
    ---                            | tilde(f)
    ---                            |
    ---                            v
    --- 0----> L_Gamma -------->   N ------>   fGamma ------>> N_Gamma ---------> 0
    --
    -- We want beta_Gamma : tildeN_Gamma ----> N_Gamma making this commute
    -- Since tildeFGamma is surjective  we find it my taking a splitting of
    -- tildeFGamma and composing down
    splitTildeFGamma := id_(target tildeFGamma)//tildeFGamma;
    gkzGenFanGamma := gzkGenFanData#0;
    fGamma := cokerMap linealitySpace gkzGenFanGamma;
    fGamma*rayInputMatrix*splitTildeFGamma
    )

bettaGamma (Matrix, Matrix) := (rayInputMatrix, gammaRayMatrix) -> (
     gzkGenFanData := gkzGeneralizedFan(rayInputMatrix, gammaRayMatrix);
     bettaGamma(rayInputMatrix, gzkGenFanData)
    )

gkzStack = method()
gkzStack (Matrix, Matrix) := (rayInputMatrix, gammaRayMatrix) -> (
    (gkzGenFanGamma,irrRaysList) := gkzGeneralizedFan(rayInputMatrix, gammaRayMatrix);
    --
    coarseGamma := toricStack(fanGensFromGeneralizedFan(rayInputMatrix,gkzGenFanGamma));
    --
    beta := bettaGamma(rayInputMatrix, gkzGenFanGamma);
    preimageCones := apply(0..dim gkzGenFanGamma,d->(
	    apply(facesAsCones(d,gkzGenFanGamma),C->(
		    affinePreimage(beta,C)
		    ));
	    ));
    preimageFan := fan preimageCones;
    stackGamma := toricStack(beta, rays preimageFan, maxCones preimageFan);
    goodMorphism := map(coarseGamma, stackGamma, beta, coarseGamma.map);
    {stackGamma, coarseGamma, goodMorphism}
    )
    
    
-*
rayInputMatrix =  transpose matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
gammaRayMatrix = matrix {{1},{1}}
gkzStack(rayInputMatrix, gammaRayMatrix)


rayInputMatrix =  transpose matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
F1 = secondaryFan(rayInputMatrix)
F2 = secondaryFan(rayInputMatrix,gkzGenFans => true)
F2.cache.gkzGenFans
F3 = secondaryFan(rayInputMatrix,gkzStacks => true)
F3.cache.gkzStacks


secondaryFan(List) := SecondaryFan => opts -> (rayInputList) -> (
    rayInputMatrix := transpose matrix rayInputList;
    secondaryFan(rayInputMatrix,opts)
    )

rayInputList = {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
G1 = secondaryFan(rayInputList)
G2 = secondaryFan(rayInputList,gkzGenFans => true)
G2.cache.gkzGenFans
G3 = secondaryFan(rayInputList,gkzStacks => true)
G3.cache.gkzStacks

secondaryFan(NormalToricVariety) := SecondaryFan => opts -> (X) -> (
    rayInputMatrix := transpose matrix rays X;
    secondaryFan(rayInputMatrix,opts)
    )

rayList = {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}};
coneList = {{0,1,4},{0,2,4},{2,3,4}}
X = normalToricVariety(rayList,coneList)
H1 = secondaryFan(X)
H2 = secondaryFan(X,gkzGenFans => true)
H2.cache.gkzGenFans
H3 = secondaryFan(X,gkzStacks => true)
H3.cache.gkzStacks


rayInputList =  matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
secondaryFan(rayInputMat)
G = secondaryFan(rayInputMat,gkzGenFans => true)
G.cache.gkzGenFans
H = secondaryFan(rayInputMat,gkzStacks => true)
H.cache.gkzStacks


*-
