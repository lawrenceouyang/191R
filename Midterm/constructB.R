constructB = function(distmatrix) {
#Part (i): Builds B matrix
	D = distmatrix;
	H = matrix( , nrow = dim(distmatrix)[1], ncol = dim(distmatrix)[1]);
	I = diag(dim(distmatrix)[1]);
	for (i in 1:dim(distmatrix)[1]) {
		for(j in 1:dim(distmatrix)[1]) {
		H[i,j] = 1/dim(distmatrix)[1];
		}
	}
	H = I - H;
	B = H %*% D;
	B = B %*% H;
	B = -1/2*B;
	return(B);
}

computeEmbedding = function(B) {
#Part (i), (ii): Creates eigenvalue barplot and then creates embedding
	E = eigen(B);
	Evals = E$values;
	barplot(Evals[1:10]);
	A = diag(dim(B)[1]);
	for (i in 1:dim(B)[1]) {
		A[i,i] = Evals[i];
	}
	A = sqrt(A);
	X = E$vectors %*% A;
	jpeg('embedding.jpeg', width = 400, height = 400, unit = 'px')
	plot(X[,1], X[,2], main = "2D Embedding");
	dev.off();
}