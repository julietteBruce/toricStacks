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
--
-- rays
gkz0 = regularSubdivision(rayMatrix, transpose liftW0)
irrRays0 =  toList set(0..numcols rayMatrix-1) - set flatten gkz0
--
gkz1 = regularSubdivision(rayMatrix, transpose liftW1)
irrRays1 =  toList set(0..numcols rayMatrix-1) - set flatten gkz1
--
gkz2 = regularSubdivision(rayMatrix, transpose liftW2)
irrRays2 =  toList set(0..numcols rayMatrix-1) - set flatten gkz2
--
gkz3 = regularSubdivision(rayMatrix, transpose liftW3)
irrRays3 =  toList set(0..numcols rayMatrix-1) - set flatten gkz3
--
gkz4 = regularSubdivision(rayMatrix, transpose liftW4)
irrRays4 =  toList set(0..numcols rayMatrix-1) - set flatten gkz4
--
-- 2D Cones
gkz03 = regularSubdivision(rayMatrix, transpose liftW03)
irrRays03 =  toList set(0..numcols rayMatrix-1) - set flatten gkz03
--
gkz13 = regularSubdivision(rayMatrix, transpose liftW13)
irrRays13 =  toList set(0..numcols rayMatrix-1) - set flatten gkz13
--
gkz12 = regularSubdivision(rayMatrix, transpose liftW12)
irrRays12 =  toList set(0..numcols rayMatrix-1) - set flatten gkz12
--
gkz24 = regularSubdivision(rayMatrix, transpose liftW24)
irrRays24 =  toList set(0..numcols rayMatrix-1) - set flatten gkz24
--
gkz04 = regularSubdivision(rayMatrix, transpose liftW04)
irrRays04 =  toList set(0..numcols rayMatrix-1) - set flatten gkz04
--

cokerMap := (A) -> (
    (prune coker A).cache.pruningMap
    )

concatenateMatrices := (L) -> (
    if #L === 1 then (return L#0);
    A := L#0;
    apply(1..#L-1,i->(A = A|L#i));
    A
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
lambdaSpace3 = concatenateMatrices(lambda3)**QQ
lambdaCoker3 = cokerMap(lambdaSpace3)
lambdaCokerSplit = id_(target E)//E
beta3 = F3*lambdaCokerSplit




E = transpose matrix {{0, 0, 0, -2}, {1, 0, 0, 1}, {0, 1, 0, 2}, {0, 0, 1, 0}, {0, 0, 0, 0}}










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
