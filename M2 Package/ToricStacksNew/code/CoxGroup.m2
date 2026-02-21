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
    expression G.characterGroup, expression G.torusRank, expression G.torusInvariants,
    expression G.smithNormalForm, expression G.phi)



-----------------------------------------------------------------------------
--- Given a matrix A we return the basic smith normal form invariants of
--- A, i.e. coker(A) = ZZ^r + ZZ/d1 + ZZ/d2 +... ZZ/dt
--- returns a tuple (freeRank, torsionInvarisnts, D) where
--- freeRank = r
--- torsionInvariants = {d1,d2,...,dt}
--- D = smith normal form of A.
----------------------------------------------------------------------------
snfInvariants = method();
snfInvariants(Matrix) :=  (M) -> (
    if not ring M === ZZ then error "Expected map of ZZ-modules";
    (D,P,Q) := smithNormalForm(M);
    --
    r := min(numRows D, numRows D);
    diagonalEntries := apply(r, i-> abs D_(i,i));
    --
    freeRank := (numRows M) - #select(diagonalEntries, d -> d != 0);
    invariantFactors := select(diagonalEntries, d -> d > 1);
    (freeRank, invariantFactors, D) 
    )


-------------------------------------------------------------------------------
-- Main: compute G-data from (B,Q)
-- Returns a HashTable describing the diagonalizable group G:
--   * characterGroup       = DG(β) as a ZZ-module
--   * torusRank            = rank of the free part (=> (G_m)^torusRank)
--   * torsionInvariants    = {n1,...,nt} (=> μ_{n1}×...×μ_{nt} after splitting)
--   * smithForm            = Smith diagonal matrix for phi
--   * phi                  = transpose(B|Q) (presentation map for DG(β))
-------------------------------------------------------------------------------

coxGroup = method(Options => {});

coxGroup(Matrix, Matrix) := opts -> (B, Q) -> (
    phi := transpose (B|Q);
    DG := coker phi;
    --
    (freeRank, invariantFactors, D) := snfInvariants(phi);
    --
    G := new CoxGroup from {
	symbol characterGroup => DG,
	symbol torusRank => freeRank,
	symbol torusIvariants => invariantFactors,
	symbol smithNormalForm => D,
	symbol phi => phi,
	symbol cache   => new CacheTable
	};
    --
    G
    )

    
