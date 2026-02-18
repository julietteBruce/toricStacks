--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #1: A^2 (affine plane) as a toric stack
----- TESTING: toricStack(betaMap, rayList, coneList) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    beta = matrix {{1,0},{0,1}};
    rayList = {{1,0},{0,1}};
    coneList = {{0,1}};
    X = toricStack(betaMap, rayList, coneList);
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1}})
    assert (max X == {{0,1}}))
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 2)
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #2:A^1 with doubled origin
----- TESTING: toricStack(betaMap, rayList, coneList) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    beta = matrix {{1,1}};
    rayList = {{1,0},{0,1};
    coneList = {{0},{1}};
    X = toricStack(betaMap, rayList, coneList);
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,-1}})
    assert (max X == {{0,1},{1,2},{0,2}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 1)
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #3:A^1/mu_2
----- TESTING: toricStack(betaMap, rayList, coneList) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    beta = matrix {{2}};
    rayList = {{1}};
    coneList = {{0}};
    X = toricStack(betaMap, rayList, coneList);
    -----
    assert isWellDefined X
    assert (map X == matrix {{2}})
    assert (rays X == {{1}})
    assert (max X == {{0}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 1)
///


--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #4:AS Example 2.7
----- TESTING: toricStack(betaMap, normalToricVariety) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    betaMap = matrix {{1,0},{1,2}};
    rayList = {{1,0},{0,1}};
    coneList = {{0,1}};
    X' = normalToricVariety(rayList,coneList);
    X = toricStack(betaMap,X');
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{1,2}}
    assert (rays X == {{1,0},{0,1}})
    assert (max X == {{0,1}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 2)
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #5:AS Example 2.7
----- TESTING: toricStack(betaMap, Fan) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    betaMap = matrix {{1,0},{1,2}}
    C1 = coneFromVData matrix {{1,0},{0,1}}
    F = fan C1
    X = toricStack(betaMap, F)
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{1,2}}
    assert (rays X == {{1,0},{0,1}})
    assert (max X == {{0,1}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 2)
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #6: P^2 (projective plane) as a toric stack
----- TESTING: toricStack(rayList, coneList) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    rayList = {{1,0},{0,1},{-1,-1}};
    coneList = {{0,1},{1,2},{0,2}};
    X = toricStack(rayList, coneList);
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,-1}})
    assert (max X == {{0,1},{1,2},{0,2}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 2)
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #7: H_2 (2nd Hirzebruch surface) as a toric stack
----- TESTING: toricStack(normalToricVariety) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    X' = hirzebruch 2;
    X = toricStack(X')
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
    assert (max X == {{0,1},{1,2},{2,3},{3,0}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert( dim X == dim X')
///

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #8: H_2 (2nd Hirzebruch surface) as a toric stack
----- TESTING: toricStack(fan) 
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    X' = hirzebruch 2;
    F = fan X';
    X = toricStack(F)
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
    assert (max X == {{0,1},{1,2},{2,3},{3,0}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == dim X')
///


--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #9: P(1,1,1,3) weighted projective 3-space as a stack
----- TESTING: weightedProjectiveStack(List)
----- STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    X = weightedProjectiveStack(1,1,1,3);
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
    assert (max X == {{0,1},{1,2},{2,3},{3,0}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 3)
///


--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS #6: P(2,2,2,6) weighted projective 3-space
----- NON-STRICT
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    X = weightedProjectiveStack(2,2,2,6);
    -----
    assert isWellDefined X
    assert (map X == matrix {{1,0},{0,1}}
    assert (rays X == {{1,0},{0,1},{-1,2},{0,-1}})
    assert (max X == {{0,1},{1,2},{2,3},{3,0}})
    --assert (fan X ==)
    assert (isStrict X == true)
    assert (dim X == 3) s
///
