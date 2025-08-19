
--------------------------------------------------------------------
--------------------------------------------------------------------
----- Tests for withoutOddAut.
--------------------------------------------------------------------
--------------------------------------------------------------------
TEST ///
    M = uniformMatroid(2,4);
    assert (withoutOddAut(M) == false)
///

TEST ///
    M = uniformMatroid(0,4);
    assert (withoutOddAut(M) == false)
///

TEST /// 
    M = wheel 3;
    assert (withoutOddAut(M) == true)
///

