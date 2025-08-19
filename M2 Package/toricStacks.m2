-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2021  Juliette Bruce, ADD YOUR NAME
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- PURPOSE : A package for working with toric stacks
--
--
-- PROGRAMMERS : Juliette Bruce, ADD YOUR NAMES
--
--
-- UPDATE HISTORY #0 - 
--
--
-- UPDATE HISTORY #1 - August 2023 - Bailee Zacovic: Began preparing
-- package for eventual publication. Adding tests, comments, documentation,
-- cleaning up code, etc.
--
--
-- UPDATE HISTORY #2 - 
--
--
-- TO DO LIST : create tests
--------------------------------------------------------------------------------



newPackage("MatroidComplexes",
    Version => "1.0",
    Date => "01 August 2023",
    Headline => "Tools for computing the matroid chain complex",
    Authors => {
        {
            Name => "Juliette Bruce",
            Email => "juliette.bruce@berkeley.edu",
            HomePage => "https://juliettebruce.github.io"
        },
        {
            Name => "Benjamin Ashlock",
            Email => "bak6t@missouri.edu"
        },
        {
            Name => "Jacob Bucciarelli",
            Email => "jbucciarelli@ksu.edu"
        },	     
        {
            Name => "Bailee Zacovic",          
            Email => "bzacovic@umich.edu"
    }},
  PackageExports => {"Matroids","SpechtModule"},
  DebuggingMode => true,
  AuxiliaryFiles => true
  )

export {
  "withoutOddAut", 
  "rankedBasis", 
  "diffMatrixColumn", 
  "diffMatrix", 

  }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------
-*
needsPackage "Matroids"
importFrom("SpechtModule", {"permutationSign"})
*-
--------------------------- withoutOddAut --------------------------
--------------------------------------------------------------------

----- INPUT: Matroid
-----
----- OUTPUT: Boolean
-----
----- DESCRIPTION: Returns true if a matroid admits an odd 
----- automorphism, and false otherwise.
--------------------------------------------------------------------
-------------------------------------------------------------------- 
withoutOddAut = method();
withoutOddAut(Matroid) := (M) -> (
    not any(getIsos(M,M), perm -> permutationSign(perm) == -1)
)

--------------------------- rankedBasis --------------------------
--------------------------------------------------------------------
----- INPUT: (Number, Number) = (n,r)
-----
----- OUTPUT: List
-----
----- DESCRIPTION: Given a pair (n,r), r <= n <= 9, return a list of
----- matroids in allMatroids(n,r) without odd automorphisms. This
----- is a basis for C_n^r.
--------------------------------------------------------------------
-------------------------------------------------------------------- 
rankedBasis = method();
rankedBasis(Number,Number) := (n,r) -> (
    select(allMatroids(n,r), withoutOddAut)
)

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Begining of the tests and the documentation
--------------------------------------------------------------------
--------------------------------------------------------------------

load ("./MatroidComplexes/tests.m2")
beginDocumentation()
load ("./MatroidComplexes/doc.m2")

end


--------------------------------------------------------------------
--------------------------------------------------------------------
----- Begining of sandbox
--------------------------------------------------------------------
--------------------------------------------------------------------

---
---
restart
uninstallPackage "MatroidComplexes"
restart
installPackage "MatroidComplexes"
check "MatroidComplexes"
installPackage "MatroidComplexes"
viewHelp SchurVeronese
