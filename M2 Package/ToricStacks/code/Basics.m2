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

projectionMapSES := (A) -> (
    transpose syz transpose A
    )

fanGensFromGeneralizedFan = method ()
fanGensFromGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fan(rayList,coneList);
    L := dual gens kernel dual linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    {rayList', maxCones F}
    )

toricVarietyGeneralizedFan = method ()
toricVarietyGeneralizedFan (List, List) := (rayList, coneList) -> (
    F := fan(rayList,coneList);
    L := dual gens kernel dual linealitySpace F;
    rayList' := entries transpose (L*(rays F));
    normalToricVariety(rayList', maxCones F)
    )

rayList = {{0,0,1},{0,0,-1},{0,1,0},{1,0,0},{1,1,0}}
coneList = {{0,1,2,3},{0,1,3,4},{0,1,2,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)


rayList = {{1,1,1},{-1,-1,-1},{1,-1,0},{1,0,-1},{0,1,-1}}
coneList = {{0,1,2,3},{0,1,2,4},{0,1,3,4}}
toricVarietyFromGeneralizedFan(rayList,coneList)
