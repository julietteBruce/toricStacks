loadPackage "NormalToricVarieties"
loadPackage "Polyhedra"
-- This consruction only depends on Cox(S) not the toric variety
-- Example 2.11 does not give an X just the rays.
rayMat = matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 

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
