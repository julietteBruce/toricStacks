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

weights = tranpose liftIntPoint -- this is the weight

regularSubdivision(transpose rayMat, transpose liftIntPoint) -- this agrees with Christine's picture!!!!!!!!!!
