needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- BASIC FUNCTIONS --------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
map ToricStack := Matrix => D -> D.map
rays ToricStack := List => {} >> o -> D -> D.rays
max  ToricStack := List => D -> D.max
fan ToricStack := Fan => D -> (
    fanRaysMatrix := (transpose matrix D.rays)**QQ;
    fan(fanRaysMatrix, D.max)
    )

fan(List, List) := Fan => (V,F) -> (
    fan(apply(F, C -> transpose matrix apply(C, idx -> V_idx)) / coneFromVData)
)

cokerMap := (A) -> (
    (prune coker A).cache.pruningMap
    )

fanGensFromGeneralizedFan = method ()
fanGensFromGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fan(rayList,coneList);
    L := cokerMap linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    {rayList', maxCones F}
    )

toricVarietyGeneralizedFan = method ()
toricVarietyGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fanGensFromGeneralizedFan(rayList, coneList);
    normalToricVariety(F#0,F#1)
    )

rayList = {{0,0,1},{0,0,-1},{0,1,0},{1,0,0},{1,1,0}}
coneList = {{0,1,2,3},{0,1,3,4},{0,1,2,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)


rayList = {{1,1,1},{-1,-1,-1},{1,-1,0},{1,0,-1},{0,1,-1}}
coneList = {{0,1,2,3},{0,1,2,4},{0,1,3,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)
