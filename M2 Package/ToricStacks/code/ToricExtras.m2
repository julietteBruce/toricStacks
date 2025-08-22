needsPackage "Normaliz"

toricIdeal = method()
toricIdeal(Matrix,Ring) := (A,R) -> (
    m := product gens R;
    saturate(sub(toBinomial(transpose(syz(A)),R),R),m)
    )
toricIdeal(Matrix) := A -> (
    numcol := numColumns(A);
    p := local p;
    R := QQ[p_0..p_(numcol-1)];
    toricIdeal(A,R)
)
toricIdeal(NormalToricVariety, Ring) := Ideal => (X,R) -> toricIdeal(transpose matrix rays X, R)
toricIdeal(NormalToricVariety) := X -> toricIdeal(transpose matrix rays X, ring X)

fan(List, List) := Fan => (V,F) -> (
    fan(apply(F, C -> transpose matrix apply(C, idx -> V_idx)) / coneFromVData)
)

toBinomial = method()
toBinomial(Matrix,Ring) := (M,S) -> (
     toBinom := (b) -> (
       pos := 1_S;
       neg := 1_S;
       scan(#b, i -> if b_i > 0 then pos = pos*S_i^(b_i)
                   else if b_i < 0 then neg = neg*S_i^(-b_i));
       pos - neg);
     ideal apply(entries M, toBinom)
     )

maxFacesAsCones = method()
maxFacesAsCones(Fan) := List => (Sigma) -> (
    V := entries transpose rays Sigma;
    (for maxCone in maxCones(Sigma) list (V_maxCone)) / transpose / matrix / coneFromVData
)

areIsomorphic = (X1, X2) -> (
	dim X1 == dim X2 and length rays X1 == length rays X2 and length max X1 == length max X2 and any(
		permutations length rays X1, 
		perm -> (all(max X1, kone -> isMember(set apply(kone, i -> perm#i), apply(max X2, set)))) and
		-- don't need to do other direction assuming toric varieties are well defined with no repeated cones
		-- because a permutation is bijective, so the map on cones is injective (no two cones can be permuted to the same cone),
		-- and because the number of cones for X1 is same as X2,
		-- it is also surjective, so it is a bijection on the sets of cones
		( -- check that there is a matrix sending v_i to w_sigma(i) for all i
			V := matrix rays X1;
			W := matrix rays X2;
			Wsig := (permMatrix perm) * W;
			T := solve(V, Wsig, MaximalRank=>true); -- 'solve' uses row reduction over ZZ. Already implemented in Macaulay2 (InvariantRing package)
			diffMatrix := V*T - Wsig;
			diffMatrix == 0 and (try inverse T then true else false) -- to make sure inverse is also defined over ZZ
		)
	)
)


genRow = (i, n) -> (
    l := {};
    for ind from 0 to n-1 do (
        if ind == i then l = append(l, 1) else l = append(l, 0);
    );
    l
)

permMatrix = perm -> (
    n := length perm;
    mat := {};
    for i in perm do (
        mat = append(mat, genRow(i, n));
    );
    matrix mat
)


asCone = method()
asCone(List, Fan) := Cone => (tau, Sigma) -> (
    rayList := entries rays Sigma;
    coneFromVData transpose matrix flatten apply(tau, idx -> rayList_idx)
)

getHilbRays = method()
getHilbRays(Cone) := List => sigma -> (
    hilbBasis := entries ((normaliz(transpose rays sigma, "integral_closure"))#"gen") ;
    hilbRays := apply(hilbBasis, b -> coneFromVData transpose matrix{b});
    hilbRays
)

mapsConestoCones = method()
mapsConestoCones(Fan, Fan, Matrix) := Boolean => (Sigma2, Sigma1, phi) -> (
    all(apply(maxFacesAsCones(Sigma1), sigma -> (
        imagePhi := affineImage(phi, sigma);
        any(apply(maxFacesAsCones(Sigma2), tau -> contains(tau, imagePhi)), bool -> bool)
            )), bool -> bool)
    )