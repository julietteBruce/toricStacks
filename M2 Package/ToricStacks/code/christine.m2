loadPackage "NormalToricVarieties"
loadPackage "Polyhedra"
-- This consruction only depends on Cox(S) not the toric variety
-- Example 2.11 does not give an X just the rays.

------------------------------------------------------------------------------------
----------- SET UP
------------------------------------------------------------------------------------
--rayMat = matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}}
-- I am working with the transpose of Christine to follow M2/Polyhedra convetntions
rayMatrix = matrix {{1, 1, 1, 1, 1}, {0, 1, 0, 0, 1}, {0, 0, 1, 2, 2}} -- tilde(f)


------------------------------------------------------------------------------------
----------- SECONDARY FAN
------------------------------------------------------------------------------------
-- Compute the Gale Dual of rayMatrix. Christine picked a different basis
--- than M2 so we work with hers directly. 
-- galeMatrix = transpose mingens ker rayMatrix -- M2 galeDual
galeMatrix = matrix{{1, 0, -2, 1, 0}, {-1, 1, 0, 1, -1}} -- G(tilde(f))

-- Compute the secondary fan via coarsest common refinement
secondaryFan = ccRefinement (galeMatrix)

-- Note M2 gives a different ordering to the rays of secondaryFan
-- so we define our cones by hand to match the document
rays secondaryFan
-- rays
psi0 = matrix {{1}, {-1}}
psi1 = matrix {{0}, {1}}
psi2 = matrix {{-2}, {0}}
psi3 = matrix {{1}, {1}}
psi4 = matrix {{0}, {-1}}
-- 2D cones
gamma03 = matrix {{1, 1}, {-1, 1}}
gamma13 = matrix {{0, 1}, {1, 1}}
gamma12 = matrix {{0, -1}, {1, 0}}
gamma24 = matrix {{-1, 0}, {0, -1}}
gamma04 = matrix {{1, 0}, {-1, -1}}


------------------------------------------------------------------------------------
----------- WEIGHT VECTORS
------------------------------------------------------------------------------------
-- Define the point in the relative interior
-- rays
w0 = matrix {{1}, {-1}}
w1 = matrix {{0}, {1}}
w2 = matrix {{-2}, {0}}
w3 = matrix {{1}, {1}}
w4 = matrix {{0}, {-1}}
-- 2D Cones (multplying by the all ones vector is the fast way to add the columns)
w03 = gamma03*(matrix{{1},{1}})
w13 = gamma13*(matrix{{1},{1}})
w12 = gamma12*(matrix{{1},{1}})
w24 = gamma24*(matrix{{1},{1}})
w04 = gamma04*(matrix{{1},{1}})

-- Get a spliting of the galeMatrix 
splittingGaleMatrix = id_(target galeMatrix)//galeMatrix
-- Lift each point to tilde(N)
-- rays
liftW0 = splittingGaleMatrix*w0
liftW1 = splittingGaleMatrix*w1
liftW2 = splittingGaleMatrix*w2
liftW3 = splittingGaleMatrix*w3
liftW4 = splittingGaleMatrix*w4
-- 2D cones
liftW03 = splittingGaleMatrix*w03
liftW13 = splittingGaleMatrix*w13
liftW12 = splittingGaleMatrix*w12
liftW24 = splittingGaleMatrix*w24
liftW04 = splittingGaleMatrix*w04


------------------------------------------------------------------------------------
----------- GKZ GENERALIZED FANS
------------------------------------------------------------------------------------
-- We use the lifts to define weights and apply regularSubdiviaon to compute the GKZ
-- generalized fans for each of the chambers in our secondary fan. We also compute
-- the irrelevant rays which is just the rays not appearing in the subdivions
-- and the lineality space for each fan
--
-- rays
gkzRays0 = regularSubdivision(rayMatrix, transpose liftW0)
irrRays0 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays0
gkzGenFan0 = fan(rayMatrix,gkzRays0)
gkzLineality0 = linealitySpace(gkzGenFan0)
--
gkzRays1 = regularSubdivision(rayMatrix, transpose liftW1)
irrRays1 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays1
gkzGenFan1 = fan(rayMatrix,gkzRays1)
gkzLineality1 = linealitySpace(gkzGenFan1)
--
gkzRays2 = regularSubdivision(rayMatrix, transpose liftW2)
irrRays2 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays2
gkzGenFan2 = fan(rayMatrix,gkzRays2)
gkzLineality2 = linealitySpace(gkzGenFan2)
--
gkzRays3 = regularSubdivision(rayMatrix, transpose liftW3)
irrRays3 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays3
gkzGenFan3 = fan(rayMatrix,gkzRays3)
gkzLineality3 = linealitySpace(gkzGenFan3)
--
gkzRays4 = regularSubdivision(rayMatrix, transpose liftW4)
irrRays4 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays4
gkzGenFan4 = fan(rayMatrix,gkzRays4)
gkzLineality4 = linealitySpace(gkzGenFan4)
--
-- 2D Cones
gkzRays03 = regularSubdivision(rayMatrix, transpose liftW03)
irrRays03 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays03
gkzGenFan03 = fan(rayMatrix,gkzRays03)
gkzLineality03 = linealitySpace(gkzGenFan03)
--
gkzRays13 = regularSubdivision(rayMatrix, transpose liftW13)
irrRays13 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays13
gkzGenFan13 = fan(rayMatrix,gkzRays13)
gkzLineality13 = linealitySpace(gkzGenFan13)
--
gkzRays12 = regularSubdivision(rayMatrix, transpose liftW12)
irrRays12 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays12
gkzGenFan12 = fan(rayMatrix,gkzRays12)
gkzLineality12 = linealitySpace(gkzGenFan12)
--
gkzRays24 = regularSubdivision(rayMatrix, transpose liftW24)
irrRays24 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays24
gkzGenFan24 = fan(rayMatrix,gkzRays24)
gkzLineality24 = linealitySpace(gkzGenFan24)
--
gkzRays04 = regularSubdivision(rayMatrix, transpose liftW04)
irrRays04 =  toList set(0..numcols rayMatrix-1) - set flatten gkzRays04
gkzGenFan04 = fan(rayMatrix,gkzRays04)
gkzLineality04 = linealitySpace(gkzGenFan04)
--

------------------------------------------------------------------------------------
----------- Tilde L SUBSPACES
------------------------------------------------------------------------------------
-- Here we compute the Tilde L Subspace as defined in the note
-- This is annoying an complicated but mostly bookkeeping so here's some code
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

tildeL = method()
tildeL (Matrix, Fan, List) := (rayMatrix,gamma,irrRays) ->(
    -- set-up
    tildeN := source rayMatrix;
    -- map from N to N_Gamma
    fGamma := cokerMap linealitySpace gamma;
    -- map from tildeN to N_gamma
    FGamma := fGamma*rayMatrix;
    nonemptyCones := delete({},flatten values faces gamma);
    -- the irrelevant rays contribute the same to each lambda space
    -- we will convert the indices to rays in tildeN later
    irrIndices := toList set(0..numcols rayMatrix-1) - ((set flatten maxCones gamma) + (set irrRays));
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
-- rays
L0 = tildeL(rayMatrix,gkzGenFan0,irrRays0)
L1 = tildeL(rayMatrix,gkzGenFan1,irrRays1)
L2 = tildeL(rayMatrix,gkzGenFan2,irrRays2)
L3 = tildeL(rayMatrix,gkzGenFan3,irrRays3)
L4 = tildeL(rayMatrix,gkzGenFan4,irrRays4)
-- 2D Cones
L03 = tildeL(rayMatrix,gkzGenFan03,irrRays03)
L13 = tildeL(rayMatrix,gkzGenFan13,irrRays13)
L12 = tildeL(rayMatrix,gkzGenFan12,irrRays12)
L24 = tildeL(rayMatrix,gkzGenFan24,irrRays24)
L04 = tildeL(rayMatrix,gkzGenFan04,irrRays04)



------------------------------------------------------------------------------------
----------- COMPUTE BETA_GAMMA
------------------------------------------------------------------------------------

bettaGamma = method()
bettaGamma (Matrix, Fan, List) := (rayMatrix,gamma,irrRays) ->(
    )




gkzGF3 = fan(rayMatrix,gkz3)
lin3 = linealitySpace gkzGF3
fGamma3 = cokerMap(lin3)
F3 = fGamma3*rayMatrix

lambda3 = apply(delete({},flatten values faces gkzGF3), tau->(
	raysTau = unique flatten tau;
	lambdaIrrList = toList set(0..numcols rayMatrix-1) - ((set flatten gkz3) + (set raysTau));
	lambdaTau = apply(unique(raysTau|lambdaIrrList),i->(matrix(id_(source rayMatrix))_i));
--	lambdaTauMatrix = matrix(lambdaTau#0);
--	apply(1..#lambdaTau-1,i->lambdaTauMatrix=lambdaTauMatrix|(matrix lambdaTau#i));
	lambdaTauMatrix = concatenateMatrices(lambdaTau);
--	{tau, concatenateMatrices(lambdaTau)};
	kerTau = ker(F3*lambdaTauMatrix);
--	{tau,lambdaTauMatrix*(gens kerTau)};
	lambdaTauMatrix*(gens kerTau)
	))
lambdaSpace3 = concatenateMatrices(lambda3)
lambdaCoker3 = cokerMap(lambdaSpace3)
lambdaCokerSplit = id_(target lambdaCoker3)//lambdaCoker3
beta3 = F3*lambdaCokerSplit
beta3*E == rayMatrix







----OLD
galeMat = matrix{{1,0,-2,1,0},{-1,1,0,1,-1}} --gives gail dual 
ccRefinement (galeMat)

intPoint = matrix{{1},{1}} -- point in the choosen cone between Sigma_I and Sigma_II in Christine

liftIntPoint = transpose matrix {{1,0,0,0,-2}} -- I manually picked this but just pick a section of galeMat
aMat * liftIntPoint == intPoint -- check my lift works

weights = transpose liftIntPoint -- this is the weight

regularSubdivision(transpose rayMat, weights) -- this agrees with Christine's picture!!!!!!!!!!


galeDual = method()
galeDual (Matrix) := (A) -> (
    transpose mingens ker A
    )

galeDual (List) := (rayList) -> (
    galeDual(transpose (matrix rayList))
    )

secondaryFan = method()
secondaryFan (List) := (rayList) ->(
    ccRefinement(galeDual(rayList))
    )

secondaryFan (NormalToricVariety) := (X) ->(
	secondaryFan(rays X)
	)

generalizedFanFromGKZChamber = method()
generalizedFanFromGKZChamber (List,List) := (rayList, gammaList) ->(
    A := galeDual(rayList);
    relIntPt := transpose matrix {sum(gammaList)};
    splitA := id_(target A)//A;
    liftPt := splitA*relIntPt;
    Fgamma := regularSubdivision(transpose rayMat, transpose liftPt);
    Igamma := toList set(0..#rayList-1) - set flatten Fgamma;
    {Fgamma,Igamma}
    )

fullGKZ = method()
generalizedFanFromGKZChamber (List) := (rayList) ->(
    secFan = secondaryFan(rayList);
    A := galeDual(rayList);
    splitA := id_(target A)//A;
    ---
    apply(flatten values faces secFan, k->(
	    
	    )


	    rays secFan)_k)
    )

generalizedFanFromGKZChamber(rayList,gammaList)
galeDual(transpose rayMat)
galeDual(entries rayMat)
secondaryFan(entries rayMat)

X = smoothFanoToricVariety(4,50)
rX = rays X
picardGroup(X)
secondaryFan(X)
rgX = galeDual(rX)
ccRefinement gX
