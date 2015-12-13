# Sort the correlation coefficients in decending order
# Since pearson and spearman have negative coefficients that can represent
# strong correlation, sort by magnitude

SortCor = function(Cor, method, colLabel) {
	Sorted = data.frame(Cor, row.names = rownames(Cor));
	col = which(colnames(Sorted) == colLabel);
	if (method == "pearson" | method == "spearman") {
		Sorted = Sorted[ order(-abs(Sorted[,col])), ];
	}
	else {
		Sorted = Sorted[ order(-Sorted[,col]), ];
	}
	Top = rownames(Sorted)[2:21];
	return(Top);
}

#Hoeffding's D
#install.packages('Hmisc');
#library('Hmisc');
#This took several hours to compute.
H_COR = hoeffd(tRETS)$D;

#Distance
D_COR = matrix(, nrow = numStock, ncol = numStock);
colnames(DCOR) = colnames(tRETS);
rownames(DCOR) = colnames(tRETS);
#install.packages('energy');
#library('energy');
#This took several hours to compute.
for (i in 1:numStock) {
	for(j in 1:numStock) {
		DCOR[i,j] = dcor(tRETS[,i],tRETS[,j]);
	}	
}