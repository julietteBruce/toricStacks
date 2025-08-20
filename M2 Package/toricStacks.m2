-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2025  Juliette Bruce, Maya Banks, Boyana Martinova, Christin Sum
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
-- PROGRAMMERS : Juliette Bruce, Maya Banks, 
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
            Name => "Boyana Martinova",
            Email => "martinova@wisc.edu",
	    HomePage => "https://sites.google.com/view/bmartinova/home"
        },	     
        {
            Name => "Christin Sum",          
            Email => "csum@hawaii.edu"
    }},
  PackageExports => {"NormalToricVarieties", "Polyhedra"},
  DebuggingMode => true,
  AuxiliaryFiles => true
  )

export {
    --Types
    "Stack", --docs, --test
    "ToricStackDatum", --docs, --test
    --Methods
    "toricStackDatum" --docs, --test
    --Functions
    --Symbols
    --
  }

--------------------------------------------------------------------
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
--------------------------------------------------------------------
needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

load "ToricStacks/code/Type.m2"
load "ToricStacks/code/Basics.m2"
load "ToricStacks/code/Constructors.m2"
load "ToricStacks/code/Maps.m2"


--------------------------------------------------------------------
--------------------------------------------------------------------
----- TESTS
--------------------------------------------------------------------
--------------------------------------------------------------------
load "ToricStacks/tests/TypeTests.m2"
load "ToricStacks/tests/BasicsTests.m2"
load "ToricStacks/tests/ConstructorTests.m2"
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
----- Begining of sandbox
--------------------------------------------------------------------
--------------------------------------------------------------------

---
---
restart
uninstallPackage "ToricStacks"
restart
installPackage "ToricStacks"
check "ToricStacks"
installPackage "ToricStacks"
viewHelp SchurVeronese
