newPackage(
    "Chambers",
    Version => "0.9",
    Date => "April 19, 2022",
    Authors => {
	{Name => "Lauren Cranton Heller", Email => "lch@math.berkeley.edu"},
	{Name => "Mahrud Sayrafi",        Email => "mahrud@math.umn.edu"}
	},
    Headline => "construct sheaves on toric varieties from maximal chambers",
    PackageImports => {"Polyhedra", "Truncations"},
    PackageExports => {"NormalToricVarieties", "LinearTruncations", "SimplicialComplexes"},
    DebuggingMode => true
    )

export{
    "primitiveCollections",
    "primitiveRelation",
    "chambersFromRelation",
    "idealFromChamber",
    -- TODO: polytopeFromChamber or divisorFromChamber
    "movGenerators",
    "secondaryFan",
    "chambers",
    "adjacentChambers",
    "fanFromIdeal",
    "conesFromIdeal",
    "degreeMap",
    "ringMap",
    "mapPresentation"
    }

-- TODO: use this more
exportFrom_Truncations {"effGenerators"}

-- TODO: replace the one in Core
nonempty = x -> select(x, i -> #i > 0)

-- move elements of L according to w
move = (w, L) -> apply(L, i -> position(w, j -> i == j))

-------------------------

-- Moved to NormalToricVarieties
-- finds all primitive collections of a toric variety
primitiveCollections = method()
primitiveCollections NormalToricVariety := X -> (
    isInCone := I -> any(X.max, C -> isSubset(I, C));
    -- TODO: this can get very slow
    select(subsets length rays X, I -> not isInCone I and all(subsets(I, #I - 1), isInCone))
    )
-- TODO: is this correct?
primitiveCollections NormalToricVariety := X -> indices \ (dual monomialIdeal X)_*

-- -- calculates the primitive relation associated to a collection
primitiveRelation = method()
primitiveRelation(NormalToricVariety, List) := memoize((X, I) -> (
    -- FIXME: use the effGenerators matrix
    s := transpose matrix { sum (rays X)_I };
    F := fan X;
    m := rays F;
    C := first flatten apply(dim F + 1,
	d -> select(cones(d, F), c -> inInterior(s, coneFromVData m_c)));
    r := solve(m_C, s);
    h1 := hashTable apply(I, i -> vector (rays X)_i => 1);
    h2 := hashTable apply(#C, i -> m_(C_i) => - r_(i,0));
    -- TODO: clear denominators
    merge(h1, h2, plus)
    ))
-- TODO: cache properly

-------------------------

-- identifies the chamber separated from the Nef cone
-- from the facet associated to a primitive relation
chambersFromRelation = method()
chambersFromRelation(NormalToricVariety, HashTable) := Matrix => (X, r) -> (
    vecs := transpose matrix rays X;
    coef := apply(#rays X,
	a -> if r#?(vecs_a) then r#(vecs_a) else 0);
    A := transpose matrix {coef};
    degs := matrix degrees ring X;
    s := numcols degs;
--    wall := coneFromVData gens ker 
    A = transpose(A // degs);
--    cham := coneFromVData \ chambers X;
    nefc := coneFromVData nefGenerators X;
    select(adjacentChambers(X,nefc),
	wall -> numcols wall == s-1 and A*wall == 0)
    )

-- calculates the irrelevant ideal corresponding to fixing a chamber as the Nef cone
-- TODO: also do chamberFromIdeal
idealFromChamber = method()
idealFromChamber(NormalToricVariety, Matrix) :=
idealFromChamber(NormalToricVariety, Cone)   := Ideal => (X, C) -> idealFromChamber(ring X, C)
idealFromChamber(Ring,               Matrix) := Ideal => (S, N) -> idealFromChamber(S, coneFromVData N)
idealFromChamber(Ring,               Cone)   := Ideal => (S, C) -> (
    mons := select(nonempty subsets gens S,
	-- TODO: change to only look at subsets of size the Picard rank of X
	-- TODO: contains acts funny with cones of different dimension
	ell -> contains(coneFromVData transpose matrix(degree \ ell), C));
    trim ideal(product \ mons)
    )

-- find the generators of the moving cone
movGenerators = method(TypicalValue => Matrix)
movGenerators Ring :=
-- TODO: see below
movGenerators NormalToricVariety := X -> movGenerators cover(QQ ** effGenerators X)
movGenerators Matrix := A -> (
    -- TODO: should this use ZZ-module intersection instead?
    rays intersection apply(numcols A,
	i -> coneFromVData submatrix'(A, , {i})))

-- find the secondary fan
secondaryFan = method(TypicalValue => Fan)
-- TODO: see below
secondaryFan NormalToricVariety := X -> secondaryFan cover(QQ ** effGenerators X)
secondaryFan Matrix := lookup(ccRefinement, Matrix)

maxMatrices := F -> ( A := rays F; apply(maxCones F, sigma -> A_sigma) )

-- finds all maximal chambers in Eff
chambers = method(TypicalValue => List)
-- TODO: which is correct?
chambers NormalToricVariety := X -> chambers cover(QQ ** effGenerators X)
-- FIXME: the target needs to be put back in Cl X?
-- FIXME: incorrect for fano(3,1)
--chambers NormalToricVariety := X -> chambers generators intersect(
--    image effGenerators X, image fromPicToCl X)
chambers Matrix := (cacheValue symbol chambers) (m -> maxMatrices secondaryFan m)


--
adjacentChambers = method()
adjacentChambers(NormalToricVariety, Cone) := (X, C) -> (
    cham := coneFromVData \ chambers X;
    adja := select(cham, D -> commonFace(C,D));
    full := new HashTable from apply(adja, D -> (
	    CD := intersection(C,D);
	    rays D => rays CD
	    ));
    wall := new HashTable from apply(rays \ facesAsCones(1,C),
	D -> D => D);
    merge(full, wall, identity)
    )


conesFromIdeal = B -> (
    if numgens B == 0 then error "expected nonzero irrelevant ideal";
    apply(flatten(exponents \ B_*),
	ell -> positions(ell, i -> i == 0))
    )

-- TODO: this description is wrong
-- returns the toric variety, list of variables, and degree map
-- given by changing the irrelevant ideal
fanFromIdeal = method()
fanFromIdeal(NormalToricVariety, Ideal) := (X,    B) -> fanFromIdeal(ring X, rays X, B)
fanFromIdeal(Ring, List,         Ideal) := (S, A, B) -> (
    maxs := conesFromIdeal trim B;
--    used := sort unique flatten maxs;
--    maxs  = move_used \ maxs;
    -- FIXME: this line makes fanFromIdeal SIGNIFICANTLY slower than just conesFromIdeal
    --fan apply(maxs, ell -> coneFromVData transpose matrix A_ell)
    -- this is silly and is also slow, but not as much
    -- FIXME: fan sometimes reorders the rays
    fan normalToricVariety(A, maxs)
    )

selectPositions = method()
selectPositions(List,List) := (M, L) -> (
    apply(M, m -> (
	    posi := positions(L, l -> l == m);
	    if #posi == 0 then error "not sublist";
	    if #posi >  1 then error "repeated entries";
	    posi_0
    	    ))
    )
-- TODO: make this faster / do something better

degreeMap = method()
degreeMap(NormalToricVariety, NormalToricVariety) := (Y, X) -> (
    (degs1, degs2) := (degrees ring X, degrees ring Y);
    -- TODO: implement for degenerate varieties?
    used := selectPositions(rays Y, rays X);
    zeros := toList(rank picardGroup Y : 0);
    C := new MutableList from (numgens ring X : zeros);
    scan(#used, a -> C#(used_a) = degs2_a);
    C  = matrix toList C;
    (used, transpose(C // matrix degs1))
    )

ringMap = method()
ringMap(NormalToricVariety, NormalToricVariety) := (Y,X) -> (
    (S, R) := (ring X, ring Y);
    (degs1, degs2) := (degrees R, degrees S);
    -- TODO: implement for degenerate varieties?
    used := selectPositions(rays Y, rays X);
    H := new HashTable from apply(#used, a -> used_a => a);
    f := apply(numgens S, a -> if H#?a then R_(H#a) else 1_R);
    map(R, S, f)
    )
-- TODO: cache

-- TODO: should this return the new irrelevant ideal also?
selectVariables(List, PolynomialRing, Matrix) := (L, S, A) -> (
    L = sort L;
    -- TODO: allow vars in different order
    deg := A * effGenerators S;
    T := newRing(S, Degrees => entries transpose deg);
    F := map(T, S);
    R := first selectVariables(L, T);
    H := new HashTable from apply(#L, a -> L_a => a);
    f := apply(numgens S, a -> if H#?a then R_(H#a) else 1_R);
--  f := new MutableList from (numgens S : 1);
--  scan(#L, a -> f#(L_a) = R_a);
    G := map(R, T, f);
    (R, G * F)
    )

mapPresentation = method()
mapPresentation(RingMap, Module, Matrix) := (f, M, A) -> (
    R := target f;
    if M == 0 then return module ideal 0_R;
    pres := matrix apply(entries presentation M, ell -> f \ ell);
    degs := entries((matrix degrees M)*(transpose A));
    if pres == 0 then R^(-degs) else cokernel map(R^(-degs), , pres)
    )
mapPresentation(NormalToricVariety, NormalToricVariety, Module) := (Y, X, M) -> (
    f := ringMap(Y, X);
    A := last degreeMap(Y, X);
    mapPresentation(f, M, A)
    )

-------------------------
-------------------------

beginDocumentation()

TEST ///
  -- check idealFromChamber for all five smooth Fano toric varieties
  scan(5, i -> assert(
	  ideal(X := smoothFanoToricVariety(2, i)) ==
	  idealFromChamber(ring X, nefGenerators X)))
///

-------------------------

doc ///
Key
  Chambers
Headline
  changing the irrelevant ideal of a toric variety
Description
  Text
    Let X be a normal toric variety.
///

end--

restart
needsPackage "Chambers"



galeDual = method()
galeDual (Matrix) := (A) -> (
    transpose mingens ker A
    )

galeDual (List) := (rayList) -> (
    galeDual(transpose (matrix rayList))
    )

secFan = method()
secFan (List) := (rayList) ->(
    ccRefinement(galeDual(rayList))
    )

secFan (NormalToricVariety) := (X) ->(
	secFan(rays X)
	)

apply(4,i->(
	time F1 = secondaryFan(smoothFanoToricVariety(2,i));
	time F2 = secFan(smoothFanoToricVariety(2,i));
	F1 == F2
	)
    )

X = smoothFanoToricVariety(2,3)
