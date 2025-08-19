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
-- PROGRAMMERS : Juliette Bruce, Maya Banks, Boyana Martinova, Christin Sum
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
    Version => "1.0",
    Date => "19 August 2025",
    Headline => "Tools for computing the matroid chain complex",
    Authors => {
        {
            Name => "Juliette Bruce",
            Email => "juliette.bruce@berkeley.edu",
            HomePage => "https://juliettebruce.github.io"
        },
        {
            Name => "Maya Banks",
            Email => "mayadb@uic.edu"
        },
        {
            Name => "Boyana Martinova",
            Email => "martinova@wisc.edu"
	    HomePage => "https://sites.google.com/view/bmartinova/home"
        },	     
        {
            Name => "Christin Sum",          
            Email => "csum@hawaii.edu"
    }},
  PackageExports => {"NormalToricVarieties","SpechtModule"},
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
needsPackage "NormalToricVarieties"
importFrom("SpechtModule", {"permutationSign"})
*-


------------------------------------------------------------------------------
-- Toric Stacks
------------------------------------------------------------------------------
KK = QQ  -- global base ring

--- kludge to access parts of the 'Core' -- NO IDEA ??
hasAttribute = value Core#"private dictionary"#"hasAttribute";
getAttribute = value Core#"private dictionary"#"getAttribute";
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary";


--------------------------- ToricStackDatum ------------------------
--------------------------------------------------------------------
----- INPUT: 
-----
----- OUTPUT: 
-----
----- DESCRIPTION: 
--------------------------------------------------------------------
-------------------------------------------------------------------- 
ToricStackDatum = new Type of Variety
ToricStackDatum.synonym = "toric stack datum"
ToricStackDatum.GlobalAssignHook = globalAssignFunction
ToricStackDatum.GlobalReleaseHook = globalReleaseFunction
expression ToricStackDatum := D -> if hasAttribute (D, ReverseDictionary) 
    then expression getAttribute (D, ReverseDictionary) else 
    (describe D)#0
describe ToricStackDatum := D -> Describe (expression toricStackDatum) (
    expression map D, expression rays D, expression max D)


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
    	CoefficientRing   => KK,
    	MinimalGenerators => false,
    	Variable          => getSymbol "x",	  
    	WeilToClass       => null
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
   -- if opts.WeilToClass =!= null then D.cache.fromWDivToCl = opts.WeilToClass;
    D.cache.CoefficientRing = opts.CoefficientRing;
    D.cache.Variable = opts.Variable;
    D
    )

toricStackDatum (Matrix, NormalToricVariety) := opts -> (betaMap,toricVar) -> (
    -- sorting cones creates a weak normal form (a.k.a. consistent output) -- from Greg
    coneList' := sort apply(max toricVar, sigma -> sort sigma); 
    D := new ToricStackDatum from {
	symbol map => betaMap,
    	symbol rays  => rays toricVar,
    	symbol max   => coneList',
    	symbol cache => new CacheTable
	};
   -- if opts.WeilToClass =!= null then D.cache.fromWDivToCl = opts.WeilToClass;
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
