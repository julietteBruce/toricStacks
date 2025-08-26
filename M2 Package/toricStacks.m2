-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2025  Juliette Bruce, Maya Banks, John Cobb
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
-- PROGRAMMERS : Juliette Bruce, Maya Banks, John Cobb
--
--
-- UPDATE HISTORY #0 - August 19, 2025 (AIM, Cal Tech, Pasadena, CA): Package started
-- at AIM workshop "Homological mirror symmetry and multigraded commutative algebra".
-- Juliette Bruce owner of repo. 
--
--
-- UPDATE HISTORY #1 -
--
--
-- UPDATE HISTORY #2 - 
--
--
-- TO DO LIST : create tests, create docs
--------------------------------------------------------------------------------



newPackage("ToricStacks",
    Version => "0.0",
    Date => "19 August 2025",
    Headline => "Working with toric stacks and related objects",
    Authors => {
        {
            Name => "Juliette Bruce",
            Email => "juliette.bruce@dartmouth.edu",
            HomePage => "https://www.juliettebruce.xyz"
        },
        {
            Name => "Maya Banks",
            Email => "mayadb@uic.edu"
        },
        {
            Name => "John Cobb",
            Email => "jdcobb3@gmail.com",
	    HomePage => "https://johndcobb.github.io"
        }},
  PackageExports => {"NormalToricVarieties", "Polyhedra", "Normaliz"},
  DebuggingMode => true,
  AuxiliaryFiles => true,
  Reload => true
  )

export {
    --Types
    "Stack", --docs, --test
    "ToricStack", --docs, --test
    "ToricStackMap", --docs, --test
    "Fantastack", --docs, --test
    "SecondaryFan", --docs, --test
    -----------------------------------------
    --Methods
    "toricStack", --docs, --test
    "toricStackMap", --docs, --test
    "isStrict", --docs, --test
    "weightedProjectiveStack", --docs, --test
    --"isIsomorphism", --docs, --test
    "rankSource", --docs, --test
    "rankTarget", --docs, --test
    "fanGensFromGeneralizedFan", --docs, --test
    "toricVarietyGeneralizedFan", --docs, --test
    "secondaryFan", --docs, --test
    "gkzGeneralizedFan", --docs, --test
    "tildeL", --docs, --test
    "bettaGamma", --docs, --test
    "gkzStack", --docs, --test
    -----------------------------------------
    --Functions
    --Symbols
    "NonStrict",
    "InputFan"
    --
  }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------

load "ToricStacks/code/Type.m2"
load "ToricStacks/code/Basics.m2"
load "ToricStacks/code/ToricExtras.m2"
load "ToricStacks/code/Constructors.m2"
load "ToricStacks/code/Fantastacks.m2"
load "ToricStacks/code/Maps.m2"
load "ToricStacks/code/SecondaryFans.m2"

--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS
--------------------------------------------------------------------
--------------------------------------------------------------------
load "ToricStacks/tests/TypeTests.m2"
load "ToricStacks/tests/BasicsTests.m2"
load "ToricStacks/tests/ConstructorsTests.m2"
load "ToricStacks/tests/MapsTests.m2"

--------------------------------------------------------------------
--------------------------------------------------------------------
----- DOCUMENTATION
--------------------------------------------------------------------
--------------------------------------------------------------------
beginDocumentation ()    
load "ToricStacks/docs/TypeDoc.m2"
load "ToricStacks/docs/BasicsDoc.m2"
load "ToricStacks/docs/ConstructorsDoc.m2"
load "ToricStacks/docs/MapsDoc.m2"

--------------
end


--------------------------------------------------------------------
--------------------------------------------------------------------
----- Beginning of sandbox
--------------------------------------------------------------------
--------------------------------------------------------------------

---
---
restart
uninstallPackage "ToricStacks"
restart
debug needsPackage "ToricStacks"
check "ToricStacks"
installPackage "ToricStacks"
