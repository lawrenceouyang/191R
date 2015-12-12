# Consider the given RETS matrix, remove poor columns and rows:

RETS = na.omit(RETS);
numStock = dim(RETS)[2];

# Calculate the various correlations:
# Pearson, Spearman:
PCOR = cor(RETS, method = "pearson");
SCOR = cor(RETS, method = "spearman");

#Maximal
#install.packages('acepack');
MaxCOR = matrix(, nrow = numStock, ncol = numStock);
colnames(MaxCOR) = colnames(RETS);
rownames(MaxCOR) = colnames(RETS);

for (i in 1:numStock) {
	for(j in 1:numStock) {
		transfVars = ace(RETS[,i],RETS[,j]);
		MaxCOR[i,j] = cor(transfVars$tx,transfVars$ty)[1];
	}
}

#Hoeffding's D
#install.packages('Hmisc');
HCOR = hoeffd(RETS)$D;

#Distance
DCOR = matrix(, nrow = numStock, ncol = numStock);
colnames(DCOR) = colnames(RETS);
rownames(DCOR) = colnames(RETS);
#install.packages('energy');
for (i in 1:numStock) {
	for(j in 1:numStock) {
		DCOR[i,j] = dcor(RETS[,i],RETS[,j]);
	}	
}

#MIC
#install.packages('minerva');
MICCOR = mine(RETS,n.cores = 4)$MIC

