--------------------------------------------------------------------
----- isListOfIntegers
--------------------------------------------------------------------
TEST ///
   assert (isListOfIntegers({1,2,3}) == true)
   assert (isListOfIntegers({1,1/2,"x"} == false)
   assert(isListOfIntegers("hello") == false);
   assert(isListOfIntegers(19) == false);
///

--------------------------------------------------------------------
----- primitiveRay
--------------------------------------------------------------------
TEST ///
   assert (primitiveRay({2,6,8}) == {1,3,4})
   assert (primitiveRay(vector {-2,0,-2,4}) == (vector {-1,0,-1,2}))
///


--------------------------------------------------------------------
----- canonicalizeFan
--------------------------------------------------------------------
TEST ///
   -- We will use the standard fan for P^2 as the test case.
   expectedRays = {{-1,-1}, {0,1}, {1,0}};
   expectedCones = {{0,1}, {0,2}, {1,2}};
   --
   -- Does not mess us something already in correct form.
   (newRays, newCones) = canonicalizeFan(expectedRays, expectedCones);
   assert(newRays == expectedRays);
   assert(newCones == expectedCones);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   expectedRays = {{-1,-1}, {0,1}, {1,0}};
   expectedCones = {{0,1}, {0,2}, {1,2}};
   --
   -- Ray List has non-primitive ray, unused duplicate rays, and rays out of order
   rayList = {{-2,-2}, {1,0}, {0,1}, {0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   (newRays, newCones) = canonicalizeFan(rayList, coneList);
   assert(newRays == expectedRays);
   assert(newCones == expectedCones);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   expectedRays = {{-1,-1}, {0,1}, {1,0}};
   expectedCones = {{0,1}, {0,2}, {1,2}};
   --
   -- Ray List has non-primitive rays, used duplicate rays, and rays out of order
   rayList = {{-5,-5}, {3,0}, {0,1}, {0,1}};
   coneList = {{0,1}, {0,2}, {1,3}};
   --
   (newRays, newCones) = canonicalizeFan(rayList, coneList);
   assert(newRays == expectedRays);
   assert(newCones == expectedCones);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   expectedRays = {{-1,-1}, {0,1}, {1,0}};
   expectedCones = {{0,1}, {0,2}, {1,2}};
   --
   -- Duplicated cones out of order
   rayList = {{-1,-1}, {0,1}, {1,0}};
   coneList = {{0,1}, {0,2}, {1,2}, {0,1}, {1,2}};
   --
   (newRays, newCones) = canonicalizeFan(rayList, coneList);
   assert(newRays == expectedRays);
   assert(newCones == expectedCones);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   expectedCones = {{0,1}, {0,2}, {1,2}};
   --
   rayList = {{-3,-3}, {0,1}, {1,0}};
   coneList = {{1,0}, {2,0}, {2,1}, {0,1}};
   --
   (newRays, newCones) = canonicalizeFan(rayList, coneList, CanonicalizeLight => true);
   -- Ray are unchanged
   assert(newRays == rayList);
   -- Cones are sorted and unduplicated
   assert(newCones == expectedCones);
///

TEST ///
  rayList = {{-3,-3},{0,5},{2,0}};
  coneList = {{1,0},{2,0},{2,1},{1,0}};
  (newRays, newCones) = canonicalizeFan(rayList, coneList, CanonicalizeLight => true);
  -- Rays untouched (not made primitive)
  assert(newRays == {{-3,-3},{0,5},{2,0}});
  -- Cones sorted within and deduplicated
  assert(newCones == {{0,1},{0,2},{1,2}});
///


--------------------------------------------------------------------
----- validateFanData
--------------------------------------------------------------------
TEST ///
  -- We will use the standard fan for P^2 as the test case.
  rayList = {{-1,-1},{0,1},{1,0}};
  coneList = {{0,1},{0,2},{1,2}};
  assert(validateFanData(rayList, coneList) == true);
///

TEST ///
  -- rays are P^2 but index 3 is out of bounds
  rayList = {{-1,-1},{0,1},{1,0}};
  coneList = {{0,1},{0,3},{1,2}};  
  assert(try (validateFanData(rayList, coneList); false) else true);
///

TEST ///
  -- Rays are of different lengths
  rayList = {{-1,-1},{0,1,0},{1,0}};  
  coneList = {{0,1},{0,2},{1,2}};
  assert(try (validateFanData(rayList, coneList); false) else true);
///



--------------------------------------------------------------------
----- validateFanData
--------------------------------------------------------------------
TEST ///
   -- We will use the standard fan for P^2 as the test case.
  rayList = {{-1,-1},{0,1},{1,0}};
  coneList = {{0,1},{0,2},{1,2}};
  (newRays, newCones) = fanData(rayList, coneList);
  assert(newRays == rayList);
  assert(newCones == coneList);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   -- cones and rays not in canonical form.
  rayList = {{-2,-2},{0,1},{1,0}};
  coneList = {{0,2},{0,1},{1,2}};
  --
  expectRays = {{-1,-1},{0,1},{1,0}};
  expectedCones = {{0,1},{0,2},{1,2}};
  (newRays, newCones) = fanData(rayList, coneList);
  assert(newRays == expectRays);
  assert(newCones == expectedCones);
///

TEST ///
   -- We will use the standard fan for P^2 as the test case.
   -- cones and rays not in canonical form.
  rayList = {{-2,-2},{0,1},{1,0}};
  coneList = {{0,2},{0,1},{1,2}};
  --
  (newRays, newCones) = fanData(rayList, coneList, CanonicalizeFan => false);
  assert(newRays == rayList);
  assert(newCones == coneList);
///

TEST ///
  needsPackage "NormalToricVarieties";
  X = toricProjectiveSpace 2;
  (newRays, newCones) = fanData(X);
  --
  expectRays = {{-1,-1},{0,1},{1,0}};
  expectedCones = {{0,1},{0,2},{1,2}};
  assert(newRays == expectRays);
  assert(newCones == expectedCones);
///

TEST ///
  needsPackage "Polyhedra";
  F = normalFan convexHull matrix {{0,1,0},{0,0,1}};
  (newRays, newCones) = fanData(F);
  --
  expectRays = {{-1,-1},{0,1},{1,0}};
  expectedCones = {{0,1},{0,2},{1,2}};
  assert(newRays == expectRays);
  assert(newCones == expectedCones);
///



--------------------------------------------------------------------
----- reduceByDiagonal
--------------------------------------------------------------------
TEST ///
  -- Trivial diagonal 
  B = matrix {{2,3},{4,5}};
  D = map(ZZ^2, ZZ^2, 0);  -- no diagonal entries
  assert(reduceByDiagonal(B, D) == B);
///

TEST ///
  B = matrix {{2,3},{4,6}};
  D = matrix {{2,11},{13,4}};
  result = reduceByDiagonal(B, D);
  assert(result == matrix {{0,1},{0,2}});
///

TEST ///
  -- Zero diagonal entry means no reduction on that row
  D = matrix {{2,0},{0,0}};
  B = matrix {{5,9},{7,3}};
  result = reduceByDiagonal(B, D);
  assert(result == matrix {{1,1},{7,3}});
///
