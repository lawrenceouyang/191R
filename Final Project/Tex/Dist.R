Dist = function(RETS) {

RETS = na.omit(RETS);
numStock = dim(RETS)[2];
tRETS = RETS[1:500,];

DCOR = matrix(, nrow = numStock, ncol = numStock);
colnames(DCOR) = colnames(tRETS);
rownames(DCOR) = colnames(tRETS);
#install.packages('energy');
for (i in 1:numStock) {
	for(j in 1:numStock) {
		DCOR[i,j] = dcor(tRETS[,i],tRETS[,j]);
	}	
	print(i);
}
}