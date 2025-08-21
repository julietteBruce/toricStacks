needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- BASIC FUNCTIONS --------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
map ToricStackDatum := Matrix => D -> D.map
rays ToricStackDatum := List => {} >> o -> D -> D.rays
max  ToricStackDatum := List => D -> D.max
fan ToricStackDatum := Fan => D -> (
    fanRaysMatrix := (transpose matrix D.rays)**QQ;
    fan(fanRaysMatrix, D.max)
    )

fan(List, List) := Fan => (V,F) -> (
    fan(apply(F, C -> transpose matrix apply(C, idx -> V_idx)) / coneFromVData)
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
