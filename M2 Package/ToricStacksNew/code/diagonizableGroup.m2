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
-- DiagonalizableGroup  TYPE DECLARATION
-----------------------------------------------------------------------------



DiagonalizableGroup = new Type of MutableHashTable
DiagonalizableGroup.synonym = "Diagonalizable Group"
DiagonalizableGroup.GlobalAssignHook = globalAssignFunction
DiagonalizableGroup.GlobalReleaseHook = globalReleaseFunction
expression DiagonalizableGroup := G -> if hasAttribute (G, ReverseDictionary) 
    then expression getAttribute (G, ReverseDictionary) else 
   (describe G)#0
describe DiagonalizableGroup := G -> Describe (expression coxGroup) (
    expression G.characterGroup, expression G.torusRank, expression G.torsionInvariants,
    expression G.smithNormalForm, expression G.phi)



-----------------------------------------------------------------------------
--- Given a matrix A we return the basic smith normal form invariants of
--- A, i.e. coker(A) = ZZ^r + ZZ/d1 + ZZ/d2 +... ZZ/dt
--- returns a tuple (freeRank, torsionInvarisnts, D) where
--- freeRank = r
--- torsionInvariants = {d1,d2,...,dt}
--- (D,P,Q) = smith normal form of A.
----------------------------------------------------------------------------
snfInvariants = method();
snfInvariants(Matrix) :=  (M) -> (
    if not (ring M === ZZ) then error "Expected map of ZZ-modules";
    (D,P,Q) := smithNormalForm(M);
    --
    r := min(numRows D, numColumns D);
    diagonalEntries := apply(r, i-> abs D_(i,i));
    --
    freeRank := (numRows M) - #select(diagonalEntries, d -> d != 0);
    invariantFactors := sort select(diagonalEntries, d -> d > 1);
    (freeRank, invariantFactors, (D,P,Q)) 
    )


----------------------------------------------------------------------------
--- These are basic functions that basically allow one to call the keys
--- of DiagonalizableGroup as function.
-----------------------------------------------------------------------------
map DiagonalizableGroup := Module => G -> G.characterGroup
smithNormalForm DiagonalizableGroup := Sequence => G -> G.smithNormalForm

torusRank = method()
torsionInvariants = method()
phi = method()

torusRank DiagonalizableGroup := ZZ => G -> G.torusRank
torsionInvariants DiagonalizableGroup := List => G -> G.torsionInvariants
phi DiagonalizableGroup := Matrix =>  G -> G.phi


-------------------------------------------------------------------------------
-- Main: compute G-data from (B,Q)
-- Returns a HashTable describing the diagonalizable group G:
--   * characterGroup       = DG(β) as a ZZ-module
--   * torusRank            = rank of the free part (=> (G_m)^torusRank)
--   * torsionInvariants    = {n1,...,nt} (=> μ_{n1}×...×μ_{nt} after splitting)
--   * smithNormalForm            = Smith diagonal matrix for phi
--   * phi                  = transpose(B|Q) (presentation map for DG(β))
-------------------------------------------------------------------------------

diagonalizableGroup = method(Options => {});

diagonalizableGroup(Matrix, Matrix) := opts -> (B, Q) -> (
    validateMapData(B,Q);
    --
    phi := transpose (B|Q);
    DG := coker phi;
    --
    (freeRank, invariantFactors, SNF) := snfInvariants(phi);
    --
    G := new CoxGroup from {
	symbol characterGroup => DG,
	symbol torusRank => freeRank,
	symbol torsionInvariants => invariantFactors,
	symbol smithNormalForm => SNF,
	symbol phi => phi,
	symbol cache   => new CacheTable
	};
    --
    G
    )

diagonalizableGroup(Matrix) := opts -> (B) -> (
    if not (ring B === ZZ) then error "Expected a ZZ-linear map or matrix";
    if not isFreeModule(source B) then error "Expected the source to be a free ZZ-module";
    if isFreeModule(target B) then (
	r := numRows B;
	Q := map(ZZ^r,ZZ^0,0);
	coxGroup(B,Q)
	)
    else (
	L := target B;
	P := coverMap L;
	Q := matrix presentation L;
	coxGroup(B//P,Q)
	)
    )

    
diagonalizableGroup(ToricStack) := opts -> (D) -> (
    coxGroup(D.map,D.presentation)
    )


-----------------------------------------------------------------------------
-- Returns the dimension of the group, which is the rank of the torus factor
-----------------------------------------------------------------------------
dim(DiagonalizableGroup) := G -> (torusRank G)

-----------------------------------------------------------------------------
-- Checks whether group is finite
-----------------------------------------------------------------------------
isFinite(DiagonalizableGroup) := G -> (torusRank G == 0);

-----------------------------------------------------------------------------
-- Checks whether group is a torus
-----------------------------------------------------------------------------
isTorus = method();
isTorus(DiagonalizableGroup) := G -> (#torsionInvariants G == 0);

-----------------------------------------------------------------------------
-- Checks whether group is connected
-----------------------------------------------------------------------------
isConnected = method();
isConnected(DiagonalizableGroup) := G -> (isTorus(G));


-----------------------------------------------------------------------------
-- Returns order of the torsion part
-----------------------------------------------------------------------------
torsionOrder = method();
torsionOrder(DiagonalizableGroup) := G -> (
    product(torsionInvariants G) --M2 has 1 as the product over {}
);

-----------------------------------------------------------------------------
-- Returns the exponent of the group 
-----------------------------------------------------------------------------
exponent = method()
exponent(DiagonalizableGroup) := G -> (
    if torusRank(G) > 0 then error "Not every element is of finite order";
    lcm(torsionInvariants G)
    )

-----------------------------------------------------------------------------
-- Checks if two groups are isomorphic 
-----------------------------------------------------------------------------
areIsomorphic = method()
areIsomorphic(DiagonalizableGroup, DiagonalizableGroup) := (G, H) -> (
    (torusRank G == torusRank H) and (torsionInvariants G == torsionInvariants H)
    )
-----------------------------------------------------------------------------
-- Gives a matrix presenting a group from given invariants
-----------------------------------------------------------------------------
matrixFromInvariants = method();
matrixFromInvariants(ZZ, List) := (r, torsion) -> (
    t := #torsion;
    if t == 0 then D := map(ZZ^0,ZZ^0,0)
    else D := diagonalMatrix torsion;
    Z := map(ZZ^t, ZZ^r, 0);
    D || Z
);


-----------------------------------------------------------------------------
-- Returns group given a set of invariants 
-----------------------------------------------------------------------------
coxGroupFromInvariants = method();
coxGroupFromInvariants(ZZ, List) := (r, torsion) -> (
    if r < 0 then error "Torus rank must be non-negative";
    if any(torsion, n -> not instance(n, ZZ)) then error "Torsion invarians must be integers";
    if any(torsion, n -> n <= 1) then error "Torsion invariants must be >1";
    --
    phi := matrixFromInvariants(r, torsion);
    DG := coker phi;
    (freeRank, invariantFactors, SNF) := snfInvariants(phi);
    --
    G := new CoxGroup from {
        symbol characterGroup => DG,
        symbol torusRank => freeRank,
        symbol torsionInvariants => invariantFactors,
        symbol smithNormalForm => SNF,
        symbol phi => phi,
        symbol cache => new CacheTable
    };
    G
);


-----------------------------------------------------------------------------
-- WARNING AI CODE TO LOOK AT
-----------------------------------------------------------------------------
joinWith = (L, sep) -> (
    if #L == 0 then "" else (
        s := L#0;
        for i from 1 to #L-1 do s = s | sep | L#i;
        s
    )
);

groupStructureString = method();
groupStructureString DiagonalizableGroup := G -> (
    r := torusRank G;
    inv := torsionInvariants G;

    parts := {};

    -- torus part
    if r > 0 then (
        parts = append(parts,
            if r == 1 then "G_m" else "G_m^" | toString r
        )
    );

    -- finite diagonalizable part
    if #inv > 0 then (
        parts = parts | apply(inv, n -> "mu_" | toString n)
    );

    if #parts == 0 then "1" else joinWith(parts, " x ")
);

-- as a Net (prints without string quotes)
groupStructureNet = method();
groupStructureNet DiagonalizableGroup := G -> net groupStructureString G;

-- as an Expression (useful inside describe/expression methods)
groupStructureExpression = method();
groupStructureExpression DiagonalizableGroup := G -> expression groupStructureNet G;

