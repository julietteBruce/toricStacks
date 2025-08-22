loadPackage "NormalToricVarieties"

-- This consruction only depends on Cox(S) not the toric variety
-- Example 2.11 does not give an X just the rays.
rayMat = matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}}
maxCon = {{0,1,2},{1,2,3},{2,3,4},{3,4,5},{4,5,0}}
X = normalToricVariety(entries rayMat, maxCon)
