-- -*- coding: utf-8 -*-
--------------------------------------------------------------------------------
-- Copyright 2025  Juliette Bruce, Maya Banks
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
-- UPDATE HISTORY #0 - 
--
--
-- UPDATE HISTORY #1 -
--
--
-- UPDATE HISTORY #2 - 
--
--
-- TO DO LIST : create tests
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
            Name => "first last",
            Email => " "
        },	     
        {
            Name => "first last",          
            Email => " "
    }},
  PackageExports => {"NormalToricVarieties"},
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
-*
needsPackage "NormalToricVarieties"
importFrom("SpechtModule", {"permutationSign"})
*-
needsPackage "NormalToricVarieties"
needsPackage "Polyhedra"

--------------------------------------------------------------------
--------------------------------------------------------------------
------------------------- CREATE TYPE ------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
--- kludge to access parts of the 'Core'
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";


-----------------------------------------------------------------------------
-- Stack  TYPE DECLA
-----------------------------------------------------------------------------

Stack = new Type of MutableHashTable
Stack.synonym = "stack"
Stack.GlobalAssignHook = globalAssignFunction
Stack.GlobalReleaseHook = globalReleaseFunction

-----------------------------------------------------------------------------
-- TORIC STACK TYPE DECLaRATION
-----------------------------------------------------------------------------

ToricStackDatum = new Type of Stack
ToricStackDatum.synonym = "toric stack datum"
ToricStackDatum.GlobalAssignHook = globalAssignFunction
ToricStackDatum.GlobalReleaseHook = globalReleaseFunction
expression ToricStackDatum := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
   (describe D)#0
describe ToricStackDatum := D -> Describe (expression toricStackDatum) (
    expression D.map, expression D.rays, expression D.max)

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

--------------------------- toricStackDatum ------------------------
--------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
--------------------------------------------------------------------
-------------------------------------------------------------------- 
toricStackDatum = method (
    TypicalValue => ToricStackDatum, 
    Options => {
    	CoefficientRing   => QQ,
    	Variable          => getSymbol "x",	  
	}
    )

 
toricStackDatum (Matrix, List, List) := opts -> (betaMap, rayList, coneList) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(coneList, sigma -> sort sigma);
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rayList,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
    )

toricStackDatum (Matrix, NormalToricVariety) := opts -> (betaMap,X) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(max X, sigma -> sort sigma); 
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rays X,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )

toricStackDatum (List, List) := opts -> (rayList, coneList) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(coneList, sigma -> sort sigma);
    dimTorus := #(rayList#0);
    betaMap := id_{ZZ^dimTorus};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rayList,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
    )

toricStackDatum (NormalToricVariety) := opts -> (X) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(max X, sigma -> sort sigma);
    dimTorus := #((rays X)#0);
    betaMap := id_{ZZ^dimTorusX};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rays X,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )

toricStackDatum (Fan) := opts -> (F) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(maxCones F, sigma -> sort sigma);
    dimTorus := rank transpose rays F;
    betaMap := id_{ZZ^dimTorusX};
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => entries transpose rays F,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   D.cache.CoefficientRing = opts.CoefficientRing;
   D.cache.Variable = opts.Variable;
   D
    )

--------------------------- isWellDefined ------------------------
--------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
--------------------------------------------------------------------
-------------------------------------------------------------------- 
isWellDefined ToricStackDatum := Boolean => D -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys D;
    expectedKeys := set {symbol map, symbol rays, symbol max, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList (K - expectedKeys);
	    missing := toList (expectedKeys - K);
	    if #added > 0 then 
	    << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	    << "-- missing keys(s): " << toString missing << endl
	    );	 
    	return false
    	);
    -- check toric variety
    X := normalToricVariety(D.rays,D.max);
    if not isWellDefined X then (
	if debugLevel > 0 then 
	    << "--  `rays' and `max' do not give well-defined normal toric variety." << endl;
	return false
	);
    if not instance(D.map, Matrix) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a matrix" << endl;
	return false
	);
     if (ring source D.map != ZZ) or (ring target D.map != ZZ) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a map of ZZ modules" << endl;
	return false
	);
    if not isFreeModule(source D.map) or not isFreeModule(target D.map) then (
	if debugLevel > 0 then 
	    << "-- expected `map' to be a map of free ZZ modules" << endl;
	return false
	);
	)


ToricStackDatumMap = new Type of HashTable
ToricStackDatumMap.synonym = "toric stack datum map"
source ToricStackDatumMap := ToricStackDatum => f -> f.source
target ToricStackDatumMap := ToricStackDatum => f -> f.target
map ToricStackDatumMap := List => opts -> f -> f.map

map(ToricStackDatum, ToricStackDatum, List) := ToricStackDatumMap => opts -> (D2, D1, A) -> (
    bigPhi := A#0;
    littlePhi := A#1;
    if ring bigPhi =!= ZZ or ring littlePhi =!= ZZ then error "-- expected integer matrices";
    if rank source bigPhi != rank source D1.map then 
        error("-- expected source of bigPhi be the source lattice of D1");
    if rank target bigPhi != rank source D2.map then 
        error("-- expected target of bigPhi be the source lattice of D1");
    if rank source littlePhi != rank target D1.map then 
        error("-- expected source of littlePhi be the target lattice of D2");
    if rank target littlePhi != rank target D2.map then
        error("-- expected target of littlePhi be the target lattice of D2");
    if (D2.map)*(bigPhi) != (D1.map)*(littlePhi) then
        error("-- expected maps to commute");
    
    new ToricStackDatumMap from {
    	symbol source => D1,
    	symbol target => D2,
    	symbol map => A,
    	symbol cache => new CacheTable}
    )

map(ToricStackDatum, ToricStackDatum, Matrix, Matrix) := ToricStackDatumMap => opts -> (D2, D1, bigPhi, littlePhi) -> (
    A := {bigPhi, littlePhi};
    map(D2, D1, A)
    )

isWellDefined ToricMap := Boolean => f -> (
    -- CHECK DATA STRUCTURE
    -- check keys
    K := keys f;
    expectedKeys := set{symbol source, symbol target, symbol matrix, symbol cache};
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
	    missing := toList(expectedKeys - K);
	    if #added > 0 then 
	        << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then 
	        << "-- missing keys(s): " << toString missing << endl);
    	return false
	);
    --Check types
    if not instance(f.source, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `source' to be a NormalToricVariety" << endl);
	return false	);
    if not instance(f.target, NormalToricVariety) then (
	if debugLevel > 0 then (
	    << "-- expected `target' to be a NormalToricVariety" << endl);
	return false
	);
    if not instance(f.matrix, Matrix) then (
	if debugLevel > 0 then (
	    << "-- expected `matrix' to be a Matrix" << endl);
	return false
	);
    if ring matrix f =!= ZZ then (
    	if debugLevel > 0 then (
	    << "-- expected `matrix' over the integers" << endl);
	return false
	);	 
    if not instance(f.cache, CacheTable) then (
    	if debugLevel > 0 then (
	    << "-- expected `f.cache' to be a CacheTable" << endl);
    	return false
	);
    --Check mathematical structure
    X := source f;
    Y := target f;
    A := matrix f;
    if rank source A =!= dim X then (
    	if debugLevel > 0 then (
	    << "-- expected number of columns of the matrix to equal the dimension of the source variety"
	    );
	return false
	);
    if rank target A =!= dim Y then (
    	if debugLevel > 0 then (
	    << "-- expected number of rows of the matrix to equal the dimension of the target variety"
	    );
	return false
	);
    V := transpose matrix rays X;
    if not all(max X, sigma -> any(max Y, tau -> (
		normals := outerMatrix outerNormals(Y, tau);
		innerProducts := normals * A * V_sigma;		
		all(flatten entries innerProducts, b -> b <= 0)))) then (
    	if debugLevel > 0 then (
	    << "-- expected image of each maximal cone to be contained in some maximal cone");
	return false
	);
    true
    )

ToricMap * ToricMap := ToricMap => (g, f) -> (
    if target f =!= source g then error "-- expected composable maps";
    new ToricMap from {
    	symbol source => source f,
    	symbol target => target g,
    	symbol matrix => (matrix g) * (matrix f),
    	symbol cache => new CacheTable
	}
    )

ToricMap == ToricMap := Boolean => (f, g) -> (
    source f === source g and target f === target g and matrix f == matrix g
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Begining of the tests and the documentation
--------------------------------------------------------------------
--------------------------------------------------------------------

load ("./ToricStacks/tests.m2")
beginDocumentation()
load ("./ToricStacks/doc.m2")

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
