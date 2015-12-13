project191 = function() {
# Consider the given RETS matrix, remove poor columns and rows:

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

#Choose an instrument(s) by name
Selection = matrix(c("SPY", "YHOO", "DELL", "AAPL", "IBM"));

#Obtain the top values and build the linear regression

#Pearson
for (i in 1:dim(Selection)[1]) {
Top = SortCor(P_COR, "pearson", Selection[i]);
func = paste(Selection[i],"~");
for (j in 1:20) {
	if (j == 20) {
		func = paste(func, Top[j]);
	}
	else {
		func = paste(func, Top[j], "+");
	}
}
func = formula(func);
lm.fit = lm(func, data = dftRETS);
print(summary(lm.fit));
prediction = predict(lm.fit, newdata = dfRETS, se.fit = TRUE);
prediction = prediction$fit;
prediction = matrix(prediction);
real = RETS[501:dim(RETS)[1], which(colnames(RETS) == Selection[i])];
real = matrix(real);
error = abs(real - prediction);
error = colSums(error)/(dim(RETS)[1] - 500);
print(error);
}

#Spearman
for (i in 1:dim(Selection)[1]) {
Top = SortCor(S_COR, "spearman", Selection[i]);
func = paste(Selection[i],"~");
for (j in 1:20) {
	if (j == 20) {
		func = paste(func, Top[j]);
	}
	else {
		func = paste(func, Top[j], "+");
	}
}
func = formula(func);
lm.fit = lm(func, data = dftRETS);
print(summary(lm.fit));
prediction = predict(lm.fit, newdata = dfRETS, se.fit = TRUE);
prediction = prediction$fit;
prediction = matrix(prediction);
real = RETS[501:dim(RETS)[1], which(colnames(RETS) == Selection[i])];
real = matrix(real);
error = abs(real - prediction);
error = colSums(error)/(dim(RETS)[1] - 500);
print(error);
}

#Hoeffding
for (i in 1:dim(Selection)[1]) {
Top = SortCor(H_COR, "hoeffding", Selection[i]);
func = paste(Selection[i],"~");
for (j in 1:20) {
	if (j == 20) {
		func = paste(func, Top[j]);
	}
	else {
		func = paste(func, Top[j], "+");
	}
}
func = formula(func);
lm.fit = lm(func, data = dftRETS);
print(summary(lm.fit));
prediction = predict(lm.fit, newdata = dfRETS, se.fit = TRUE);
prediction = prediction$fit;
prediction = matrix(prediction);
real = RETS[501:dim(RETS)[1], which(colnames(RETS) == Selection[i])];
real = matrix(real);
error = abs(real - prediction);
error = colSums(error)/(dim(RETS)[1] - 500);
print(error);
}

#Distance Correlation
for (i in 1:dim(Selection)[1]) {
Top = SortCor(H_COR, "distance", Selection[i]);
func = paste(Selection[i],"~");
for (j in 1:20) {
	if (j == 20) {
		func = paste(func, Top[j]);
	}
	else {
		func = paste(func, Top[j], "+");
	}
}
func = formula(func);
lm.fit = lm(func, data = dftRETS);
print(summary(lm.fit));
prediction = predict(lm.fit, newdata = dfRETS, se.fit = TRUE);
prediction = prediction$fit;
prediction = matrix(prediction);
real = RETS[501:dim(RETS)[1], which(colnames(RETS) == Selection[i])];
real = matrix(real);
error = abs(real - prediction);
error = colSums(error)/(dim(RETS)[1] - 500);
print(error);
}

}


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

