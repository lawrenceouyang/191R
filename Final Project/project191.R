project191 = function() {
# Consider the given RETS matrix, remove poor columns and rows:
load(SP_500_2003_2015.data);
#If the first 100 entries are NA, remove the column
#By going in reverse direction, we avoid deleting columns during the iteration
#causing out of bound errors.

temp = dim(RETS)[2];
for (i in 0:(temp-1)) {
	Flag = 0;
	for (j in 1:100) {
		if (!is.na(RETS[j,temp-i])) {
			Flag = 1;
			break;
		}
	}
	if (Flag == 0) {
		RETS = RETS[,-(temp-i)];
	}
	Flag = 0;
}
RETS = na.omit(RETS);

#Build the 4 correlation matrices that we will be using
numStock = dim(RETS)[2];
tRETS = RETS[1:500,]
dfRETS = data.frame(RETS[501:dim(RETS)[1], ], row.names = rownames(RETS[501:dim(RETS)[1], ]));
dftRETS = data.frame(tRETS, row.names = rownames(tRETS));

# Pearson, Spearman:
P_COR = cor(tRETS, method = "pearson");
S_COR = cor(tRETS, method = "spearman");

#Hoeffding's D
#install.packages('Hmisc');
#library('Hmisc');
H_COR = hoeffd(tRETS)$D;

#Distance
DIST_COR = matrix(, nrow = numStock, ncol = numStock);
colnames(DIST_COR) = colnames(tRETS);
rownames(DIST_COR) = colnames(tRETS);
#install.packages('energy');
#library('energy');
#This took 8.5 hours to compute (190 million calculations)
for (i in 1:numStock) {
	for(j in 1:numStock) {
		DIST_COR[i,j] = dcor(tRETS[,i],tRETS[,j]);
	}	
}

#Choose an instrument(s) by name
Selection = matrix(c("SPY", "YHOO", "DELL", "AAPL", "IBM"));

#Obtain the top values and build the linear regression
#1. Sort by top correlation and retrieve first 20, excluding itself
#2. Copy the names of the top 20 and use them in the linear regression
#3. Compute the regression model
#4. Compute error term

for (i in 1:dim(Selection)[1]) {
	print(paste(Selection[i],": Pearson"));
	func = SortCor(P_COR, "pearson", Selection[i]);
	linRegression(func, dftRETS, dfRETS, Selection[i]);

	print(paste(Selection[i],": Spearman"));
	func = SortCor(S_COR, "spearman", Selection[i]);
	linRegression(func, dftRETS, dfRETS, Selection[i]);

	print(paste(Selection[i],": Hoeffding's D"));
	func = SortCor(H_COR, "hoeffding", Selection[i]);
	linRegression(func, dftRETS, dfRETS, Selection[i]);

	print(paste(Selection[i],": Distance"));
	func = SortCor(DIST_COR, "distance", Selection[i]);
	linRegression(func, dftRETS, dfRETS, Selection[i]);
}

}

##############################################################################

# Sort the correlation coefficients in decending order
# Since pearson and spearman have negative coefficients that can represent
# strong correlation, sort by magnitude
# return top 20 formulated in a string for the lm function

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
	func = paste(colLabel,"~");
	for (j in 1:20) {
		if (j == 20) {
			func = paste(func, Top[j]);
		}
		else {
			func = paste(func, Top[j], "+");
		}
	}
	func = formula(func);
	return(func);
}

##############################################################################

#Build linear regression model
linRegression = function(func, dftRETS, dfRETS, selection) {
#func: function for regression
#dftRETS: training matrix
#dfRETS: test matrix
#selection: the selected instrument

lm.fit = lm(func, data = dftRETS);
print(summary(lm.fit));
prediction = predict(lm.fit, newdata = dfRETS[501:dim(dfRETS)[1],], se.fit = TRUE);
prediction = prediction$fit;
prediction = matrix(prediction);
real = dfRETS[501:dim(dfRETS)[1], which(colnames(dfRETS) == selection)];
real = matrix(real);
print(dim(real));
print(dim(prediction));
error = abs(real - prediction);
error = colSums(error)/(dim(dfRETS)[1] - 500);
print(error);

}

