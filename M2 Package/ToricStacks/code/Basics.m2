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
