# Consider the given RETS matrix, remove poor columns and rows:
createCor = function(RETS, PCOR, SCOR, MaxCOR) {

RETS = na.omit(RETS);
numStock = dim(RETS)[2];
tRETS = RETS[1:500,]

# Calculate the various correlations:
# Pearson, Spearman:
PCOR = cor(tRETS, method = "pearson");
SCOR = cor(tRETS, method = "spearman");

#Maximal
#install.packages('acepack');
MaxCOR = matrix(, nrow = numStock, ncol = numStock);
colnames(MaxCOR) = colnames(tRETS);
rownames(MaxCOR) = colnames(tRETS);

for (i in 1:numStock) {
	for(j in 1:numStock) {
		transfVars = ace(tRETS[,i],tRETS[,j]);
		MaxCOR[i,j] = cor(transfVars$tx,transfVars$ty)[1];
	}
}

#Hoeffding's D
#install.packages('Hmisc');
HCOR = hoeffd(tRETS)$D;

#Distance
DCOR = matrix(, nrow = numStock, ncol = numStock);
colnames(DCOR) = colnames(tRETS);
rownames(DCOR) = colnames(tRETS);
#install.packages('energy');
for (i in 1:numStock) {
	for(j in 1:numStock) {
		DCOR[i,j] = dcor(tRETS[,i],tRETS[,j]);
	}	
}


