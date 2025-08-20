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


fanFromGeneralizedFan = method ()
fanFromGeneralizedFan (List, List) := opts -> (rayList, coneList) -> (
    
    )


betaMap = matrix {{1,0},{1,2}}
C1 = coneFromVData matrix {{1,0},{0,1}}
F = fan C1
toricStackDatum(betaMap, F)


linearTransform = method()
linearTransform(Fan, Matrix) := (F, A) -> (
   newRays = rays F;
   newRays = A * newRays
   newLineality = linealitySpace F;
   check = kernel A;
   check = newLineality | (gens check);
   if(rank check != rank newLineality) then << "Warning: Output fan may not be well defined. Check with 'isWellDefined'" << endl;
   newLineality = A * newLineality;
   newLineality = mingens image newLineality;
   goodNewRays = makeRaysUniqueAndPrimitive(newRays, newLineality);
   result := new HashTable from {
      rays => newRays,
      computedLinealityBasis => newLineality,
      generatingObjects => maxCones F
   };
   internalFanConstructor result
)
