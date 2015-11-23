matrix_rand = function(RETS) {
Eigenvals = matrix(, nrow = 50, ncol = 1);
for (i in 1:50) {
	RETSRAND = matrix(sample(c(RETS)), nrow = dim(RETS)[1], ncol = dim(RETS)[2]);
	RANDEig = eigen(cov(RETSRAND))$values;
	Eigenvals[i] = mean(RANDEig);
}
return(Eigenvals);
}