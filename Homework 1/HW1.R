#Lawrence Ouyang
#504128219
#Math191
#Homework 1

#1. The histograms are attached.
#Create Histograms
pdf("Ticker_Histograms.pdf", width = 8.5, height = 11);
par(mfrow = c(4,3))
for (i in 1:nrTickers) {
	hist(RET[,i], main = tickers[i], xlab = 'Log Return', col = "red");
}
dev.off()

#2. Collect the means and variance into a vector SampleMean and SampleVar:

SampleMean = matrix(, nrow = nrTickers, ncol = 1);
SampleVar = matrix(, nrow = nrTickers, ncol = 1);

for (i in 1:nrTickers) {
	SampleMean[i] = mean(RET[,i]);
	SampleVar[i] = var(RET[,i]);
}

colnames(SampleMean) = c("SM of Log Return");
rownames(SampleMean) = tickers;
colnames(SampleVar) = c("SV of Log Return");
rownames(SampleVar) = tickers;

#install.packages('gplots');
#library('gplots');
pdf("SampleMeanVar,pdf", width = 8.5, height = 11);
par(mfrow = c(4,3));
textplot(SampleMean);
textplot(SampleVar);
dev.off();

#Other Correlation Measurements (put in test_correlations())
#Maximal
#install.packages('acepack');
Max_cor = matrix(, nrow = nrTickers, ncol = nrTickers);
colnames(Max_cor) = tickers;
rownames(Max_cor) = tickers;

for (i in 1:nrTickers) {
	for(j in 1:nrTickers) {
		transfVars = ace(RET[,i],RET[,j]);
		Max_cor[i,j] = cor(transfVars$tx,transfVars$ty)[1];
		Max_cor[i,j] = round(Max_cor[i,j],3);
	}
}
print('Max_cor:'); print(Max_cor);
list_CorMats[['Maximal']] = Max_cor

#Hoeffding's D
#library('Hmisc');
H_cor = hoeffd(RET)$D;
H_cor = round(H_cor,3);
print('Hoeffding\'s D:'); print(H_cor);
list_CorMats[['Hoeffding']] = H_cor

#Distance
D_cor = matrix(, nrow = nrTickers, ncol = nrTickers);
colnames(D_cor) = tickers;
rownames(D_cor) = tickers;

#library('energy');
for (i in 1:nrTickers) {
	for(j in 1:nrTickers) {
		D_cor[i,j] = dcor(RET[,i],RET[,j]);
		D_cor[i,j] = round(D_cor[i,j],3);
	}	
}
print('dCor:'); print(D_cor);
list_CorMats[['dCor']] = D_cor

#MIC
#library('minerva');
MIC_cor = mine(RET,n.cores = 4)$MIC
MIC_cor = round(MIC_cor,3);
print('MIC_cor:'); print (MIC_cor);
list_CorMats[['MIC']] = MIC_cor


#3. Modified test_correlations() to have a parameter Instr for the tickers being processed.
Pearson, Spearman, Maximal, Distance, and MIC all approximate to 1, indicating that they are
trivial relationships, wtih a near perfect linear relationship. We can conclude that
SPXS is the leveraged inverse ETF(short/bear) and SPXL is the leveraged ETF(long/bull).
From our results, we see that neither of these ETFs are earning or losing the expected
increase/decrease from being leveraged ETFs, and they are following SPY almost perfectly.

#4. The figures are attached.

#5. Strongest Positive Correlations:
	GLD - GOLD
	SPY - ^FTSE
	SPY - OIL

    Strongest Negative Correlations:
	SPY - ^VIX
     ^VIX - ^TNX
     ^VIX - ^FTSE

#6. A lagging indicator due to the extremely large dataset can be a reason why certain
correlations are so weak. The moving average is widely distributed giving inaccurate points.
With so many weak correlations in our figures, including a lagging variable would make 
our points more accurate and help predict future returns. We can also note the high chance
of causation with certain correlations. An increase in a certain area does often cause certain
stocks to increase or decrease.

#7. The figure is attached. 