loadPackage "NormalToricVarieties"
loadPackage "Polyhedra"


galeDual = method()
galeDual (Matrix) := (A) -> (
    transpose mingens ker A
    )

galeDual (List) := (rayList) -> (
    galeDual(transpose (matrix rayList))
    )

SecondaryFan = new Type of Fan
SecondaryFan.synonym = "secondary fan"
SecondaryFan.GlobalAssignHook = globalAssignFunction
SecondaryFan.GlobalReleaseHook = globalReleaseFunction

SecondaryFan#(symbol inputRays) = null

secondaryFan = method(
    TypicalValue => SecondaryFan, 
    Options => {
        gkzGenFans   => false,
	gkzStacks => false,
    }   
)

secondaryFan(Matrix) := SecondaryFan => opts -> (rayInputMatrix) -> (
    -- Find the gale dual of our input rays.New rays are the cols still.
    galeRayMatrix := galeDual(rayInputMatrix);
    -- compute the secondary fan via ccr
    F := new SecondaryFan from ccRefinement(galeRayMatrix);
    F#(symbol inputRays) = rayInputMatrix;
    F.cache.galeDualMatrix = galeRayMatrix;
    if opts.gkzGenFans == true or opts.gkzStacks == true then (
	n := numcols rayInputMatrix;
	-- a splitting of the galeMatrix for our inputRays,
	-- will be used to lift from the secondary fan to ZZ^(Rays)
	splitGaleMatrix := id_(target F.cache.galeDualMatrix)//(F.cache.galeDualMatrix);
	-- non-empty faces of secondary fan (empty face would cause errors)
	faceSecFan := delete({},flatten values faces F);
	-- temp mutable hashes
	gkzHash := new MutableHashTable;
	stackHash := new MutableHashTable;
	apply(sort faceSecFan, f -> (
		-- get cone Gamma in secondary fan from list of rays f
		gammaMatrix = (rays F)_(f);
		m := numcols gammaMatrix;
		-- find point in the relative interior of Gamma and lift
		-- from secondary fan to ZZ^(Rays) with choosen splitting
		allOnesMatrix := matrix toList (m:{1});
		ptInRelInt :=  gammaMatrix*allOnesMatrix;
		ptLift := splitGaleMatrix*ptInRelInt;
		-- compute the gkz fan for Gamma 
		gammaFan' := regularSubdivision(rayInputMatrix, transpose ptLift);
		-- sorting for some semi-consistency
		gammaFan := apply(sort gammaFan', i -> sort i);
		irrRays := sort toList set(0..n-1) - set flatten gammaFan;
		gkzHash#f = {gammaFan,irrRays};
		-- this subroutine will compute the stacks if desired once implemented. 
		if opts.gkzStacks == true then (
		    stackHash#f = "This is where the stack will go";
		    );
		));
	F.cache.gkzGenFans = new HashTable from gkzHash;
	if opts.gkzStacks == true then (F.cache.gkzStacks = new HashTable from stackHash);
	);
    F
    )

rayInputMatrix =  transpose matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
F1 = secondaryFan(rayInputMatrix)
F2 = secondaryFan(rayInputMatrix,gkzGenFans => true)
F2.cache.gkzGenFans
F3 = secondaryFan(rayInputMatrix,gkzStacks => true)
F3.cache.gkzStacks


secondaryFan(List) := SecondaryFan => opts -> (rayInputList) -> (
    rayInputMatrix := transpose matrix rayInputList;
    secondaryFan(rayInputMatrix,opts)
    )

rayInputList = {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
G1 = secondaryFan(rayInputList)
G2 = secondaryFan(rayInputList,gkzGenFans => true)
G2.cache.gkzGenFans
G3 = secondaryFan(rayInputList,gkzStacks => true)
G3.cache.gkzStacks

secondaryFan(NormalToricVariety) := SecondaryFan => opts -> (X) -> (
    rayInputMatrix := transpose matrix rays X;
    secondaryFan(rayInputMatrix,opts)
    )

rayList = {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}};
coneList = {{0,1,4},{0,2,4},{2,3,4}}
X = normalToricVariety(rayList,coneList)
H1 = secondaryFan(X)
H2 = secondaryFan(X,gkzGenFans => true)
H2.cache.gkzGenFans
H3 = secondaryFan(X,gkzStacks => true)
H3.cache.gkzStacks


rayInputList =  matrix {{1,0,0},{1,1,0},{1,0,1},{1,0,2},{1,1,2}} 
secondaryFan(rayInputMat)
G = secondaryFan(rayInputMat,gkzGenFans => true)
G.cache.gkzGenFans
H = secondaryFan(rayInputMat,gkzStacks => true)
H.cache.gkzStacks

gkzGeneralizedFan = method()
gkzGeneralizedFan (Matrix, Matrix, Matrix) := (rayInputMatrix, gammaMatrix, galDualMatrix) -> (
    m := numcols gammaMtrix;
    -- find point in the relative interior of Gamma and lift
    -- from secondary fan to ZZ^(Rays) with choosen splitting
    allOnesMatrix := matrix toList (m:{1});
    ptInRelInt :=  gammaMatrix*allOnesMatrix;
    splitGaleMatrix := id_(target galDualMatrix)//(galDualMatrix);
    ptLift := splitGaleMatrix*ptInRelInt;
    -- compute the gkz fan for Gamma 
    gammaFan' := regularSubdivision(rayInputMatrix, transpose ptLift);
    -- sorting for some semi-consistency
    gammaFan := apply(sort gammaFan', i -> sort i);
    irrRays := sort toList set(0..n-1) - set flatten gammaFan;
    {gammaFan,irrRays}
    )
