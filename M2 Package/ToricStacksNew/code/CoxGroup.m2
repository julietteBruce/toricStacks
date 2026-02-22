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
-- CoxGroup  TYPE DECLARATION
-----------------------------------------------------------------------------



CoxGroup = new Type of MutableHashTable
CoxGroup.synonym = "Cox group"
CoxGroup.GlobalAssignHook = globalAssignFunction
CoxGroup.GlobalReleaseHook = globalReleaseFunction
expression CoxGroup := G -> if hasAttribute (G, ReverseDictionary) 
    then expression getAttribute (G, ReverseDictionary) else 
   (describe G)#0
describe CoxGroup := G -> Describe (expression coxGroup) (
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
    invariantFactors := select(diagonalEntries, d -> d > 1);
    (freeRank, invariantFactors, (D,P,Q)) 
    )


----------------------------------------------------------------------------
--- These are basic functions that basically allow one to call the keys
--- of CoxGroup as function.
-----------------------------------------------------------------------------
map CoxGroup := Module => G -> G.characterGroup
torusRank CoxGroup := ZZ => G -> G.torusRank
torsionInvariants CoxGroup := List => G -> G.torsionInvariants
smithNormalForm CoxGroup := Sequence => G -> G.smithNormalForm)
phi CoxGroup := Matrix =>  G -> G.phi


-------------------------------------------------------------------------------
-- Main: compute G-data from (B,Q)
-- Returns a HashTable describing the diagonalizable group G:
--   * characterGroup       = DG(β) as a ZZ-module
--   * torusRank            = rank of the free part (=> (G_m)^torusRank)
--   * torsionInvariants    = {n1,...,nt} (=> μ_{n1}×...×μ_{nt} after splitting)
--   * smithNormalForm            = Smith diagonal matrix for phi
--   * phi                  = transpose(B|Q) (presentation map for DG(β))
-------------------------------------------------------------------------------

coxGroup = method(Options => {});

coxGroup(Matrix, Matrix) := opts -> (B, Q) -> (
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

coxGroup(Matrix) := opts -> (B) -> (
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

    
coxGroup(ToricStack) := opts -> (D) -> (
    coxGroup(D.map,D.presentation)
    )


-----------------------------------------------------------------------------
-- Returns the dimension of the group, which is the rank of the torus factor
-----------------------------------------------------------------------------
dim = method();
dim(CoxGroup) := G -> (torusRank G)

-----------------------------------------------------------------------------
-- Checks whether group is finite
-----------------------------------------------------------------------------
isFinite = method();
isFinit(CoxGroup) := G -> (torusRank G == 0);

-----------------------------------------------------------------------------
-- Checks whether group is a torus
-----------------------------------------------------------------------------
isTorus = method();
isTorus(CoxGroup) := G -> (#torsionInvariants G == 0);

-----------------------------------------------------------------------------
-- Returns order of the torsion part
-----------------------------------------------------------------------------
finiteOrder = method();
finiteOrder(CoxGroup) := G -> (
    product(torsionInvariants G) --M2 has 1 as the product over {}
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
groupStructureString CoxGroup := G -> (
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
groupStructureNet CoxGroup := G -> net groupStructureString G;

-- as an Expression (useful inside describe/expression methods)
groupStructureExpression = method();
groupStructureExpression CoxGroup := G -> expression groupStructureNet G;

