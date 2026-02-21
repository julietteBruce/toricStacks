-- This is the layout of constructors
--- #1 = (B, Q, rayList, coneList)
--- #2A = (B, Q, normalToricVariety)
--- #2B = (B, Q, Fan)
--- #3 = (beta, rayList, coneList)
--- #3A = (beta, normalToricVariety)
--- #3B = (beta, Fan)
--- #4 = (rayList, coneList)
--- #4A = (normalToricVariety)
--- #4B = (Fan)

--------------------------------------------------------------------
----- #1 = (B, Q, rayList, coneList)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   rayList = {{-1,-1}, {0,1}, {1,0}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{1, 0}, {0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,rayList,coneList);
   assert(isWellDefined(D) == true)
///

--- Stacky: Strict: P(1,1,2)
TEST ///
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,rayList,coneList);
   assert(isWellDefined(D) == true)
///

--- Stacky: Non-Strict P(2,2,4)
TEST ///
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}};
   Q = matrix {{0}, {0}, {2}};
   D = toricStack(B,Q,rayList,coneList);
   assert(isWellDefined(D) == true)
///

--------------------------------------------------------------------
----- #2A = (B, Q, normalToricVariety) 
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties";
   X = toricProjectiveSpace 2;
   --
   B = matrix {{1, 0}, {0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,X);
   assert(isWellDefined(D) == true)
///


--- Stacky: Strict: P(1,1,2)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,X);
   assert(isWellDefined(D) == true)
///


--- Stacky: Non-Strict P(2,2,4)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}};
   Q = matrix {{0}, {0}, {2}};
   D = toricStack(B,Q,X);
   assert(isWellDefined(D) == true)
///

--------------------------------------------------------------------
----- #2B = (B, Q, Fan)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties"
   X = toricProjectiveSpace 2;
   F = fan X;
   --
   B = matrix {{1, 0}, {0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,F);
   assert(isWellDefined(D) == true)
///

--- Stacky: Strict: P(1,1,2)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   F = fan X;
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   Q = map(ZZ^2, ZZ^0, 0);
   D = toricStack(B,Q,F);
   assert(isWellDefined(D) == true)
///

--- Stacky: Non-Strict P(2,2,4)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   F = fan X;
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}};
   Q = matrix {{0}, {0}, {2}};
   D = toricStack(B,Q,F);
   assert(isWellDefined(D) == true)
///
--------------------------------------------------------------------
----- #3 = (beta, rayList, coneList)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   rayList = {{-1,-1}, {0,1}, {1,0}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{1, 0}, {0, 1}};
   D = toricStack(B,rayList,coneList);
   assert(isWellDefined(D) == true)
///


--- Stacky: Strict: P(1,1,2)
TEST ///
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   D = toricStack(B,rayList,coneList);
   assert(isWellDefined(D) == true)
///

--- Stacky: Non-Strict P(2,2,4)
TEST ///
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}};
   Q = matrix {{0}, {0}, {2}};
   D = toricStack(B,Q,F);
   assert(isWellDefined(D) == true)
///
--------------------------------------------------------------------
----- #3A = (beta, normalToricVariety)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties"
   X = toricProjectiveSpace 2;
   --
   B = matrix {{1, 0}, {0, 1}};
   D = toricStack(B,X);
   assert(isWellDefined(D) == true)
///

--- Stacky: Strict: P(1,1,2)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   D = toricStack(B,X);
   assert(isWellDefined(D) == true)
///

--- Stacky: Non-Strict P(2,2,4)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   --
   N = ZZ^3;
   L = coker matrix {{0, 0, 0}, {0, 0, 0}, {0, 0, 2}};
   beta = map(L,N, matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}})
   --
   D = toricStack(beta,X);
   assert(isWellDefined(D) == true)
///

--------------------------------------------------------------------
----- #3B = (beta, Fan)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties"
   X = toricProjectiveSpace 2;
   F = fan X
   --
   N = ZZ^3;
   L = coker matrix {{0, 0, 0}, {0, 0, 0}, {0, 0, 2}};
   beta = map(L,N, matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}})
   --
   D = toricStack(beta,F);
   assert(isWellDefined(D) == true)
///


--- Stacky: Strict: P(1,1,2)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   F = fan X;
   --
   B = matrix {{-1, 1, 0}, {-2, 0, 1}};
   D = toricStack(B,F);
   assert(isWellDefined(D) == true)
///

--- Stacky: Non-Strict P(2,2,4)
TEST ///
   needsPackage "NormalToricVarieties";
   rayList = {{1,0,0}, {0,1,0}, {0,0,1}};
   coneList = {{0,1}, {0,2}, {1,2}};
   X = normalToricVariety(rayList,coneList);
   F = fan X;
   --
   N = ZZ^3;
   L = coker matrix {{0, 0, 0}, {0, 0, 0}, {0, 0, 2}};
   beta = map(L,N, matrix {{-1, 1, 0}, {-2, 0, 1}, {1,0,0}})
   --
   D = toricStack(beta,F);
   assert(isWellDefined(D) == true)
///

--------------------------------------------------------------------
----- #4 = (rayList, coneList)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   rayList = {{-1,-1}, {0,1}, {1,0}};
   coneList = {{0,1}, {0,2}, {1,2}};
   --
   D = toricStack(rayList,coneList);
   assert(isWellDefined(D) == true)
///



--------------------------------------------------------------------
----- #4A = (normalToricVariety)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties"
   X = toricProjectiveSpace 2;
   --
   D = toricStack(X);
   assert(isWellDefined(D) == true)
///



--------------------------------------------------------------------
----- #4B = (Fan)
--------------------------------------------------------------------

--- Non-Stacky: P^2
TEST ///
   needsPackage "NormalToricVarieties"
   X = toricProjectiveSpace 2;
   F = fan X
   --
   D = toricStack(F);
   assert(isWellDefined(D) == true)
///
