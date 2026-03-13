
--------------------------------------------------------------------
--------------------------------------------------------------------
----- Tests for Map
--------------------------------------------------------------------
--------------------------------------------------------------------

----Test 1
TEST ///
    betaMap = matrix {{1,0},{1,2}}
    rayList = {{1,0},{0,1}}
    coneList = {{0,1}}
    D1 = toricStack(betaMap, rayList, coneList)
    bigPhi = matrix {{2,0},{0,2}}
    littlePhi = matrix {{2,0},{0,2}}
    A = {bigPhi, littlePhi};
    map(D1,D1,A)
///

---- Test 2
TEST ///
    betaMap = matrix {{1,0},{1,2}}
    rayList = {{1,0},{0,1}}
    coneList = {{0,1}}
    D1 = toricStack(betaMap, rayList, coneList)
    bigPhi = matrix {{2,0},{0,2}}
    littlePhi = matrix {{2,0},{0,2}}
    map(D1,D1,bigPhi,littlePhi)
///

TEST ///
    betaMap = matrix {{1,0},{1,2}}
    rayList = {{1,0},{0,1}}
    coneList = {{0,1}}
    D1 = toricStack(betaMap, rayList, coneList)
    map(D1,D1,3)
///

TEST ///
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
id_(D1)
///

TEST ///
betaMap = matrix {{1,0},{1,2}}
rayList = {{1,0},{0,1}}
coneList = {{0,1}}
D1 = toricStack(betaMap, rayList, coneList)
bigPhi1 = matrix {{2,0},{0,2}}
littlePhi1 = matrix {{2,0},{0,2}}
A1 = {bigPhi1, littlePhi1};
f1 = map(D1,D1,A)

bigPhi2 = matrix {{4,0},{0,4}}
littlePhi2 = matrix {{4,0},{0,4}}
A2 = {bigPhi2, littlePhi2};
f2 = map(D1,D1,A2)

f1 == f1
f2 == f2
f1 == f2
///
