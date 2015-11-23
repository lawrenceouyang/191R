
main_linear_regression= function(){

# The instruments in the universe (different 'stocks')
tickers = c('SPY','^VIX','^TNX','OIL','GLD','GOLD','^N225','^FTSE', 'SPXS','SPXL');
# install.packages('quantmod');  # library('quantmod');  # ended up not using this package

## Set the working directory to your own directory:
setwd('C:\\Users\\Atranemus-PC\\Documents\\R\\Homework 2');

# The file with the yahoo data (daily closing prices)
fileName = 'C:\\Users\\Atranemus-PC\\Documents\\R\\Homework 2\\ADJCLOSE.data';
load( file = fileName);

# The file with test data
fileName = 'C:\\Users\\Atranemus-PC\\Documents\\R\\Homework 2\\ADJCLOSET.data';
doWork = 0 # doWork = 1:  load data from Yahoo;   doWork = 0:  preload it from file
if(doWork == 1){ 
	source('C:\\Users\\Atranemus-PC\\Documents\\R\\Homework 1\\test_correlations.r');
	ADJCLOSET  = loadYahooData_saveToFile(tickers, dayStart='2015-09-16', dayEnd='2015-10-30', fileName = fileName)
}
else{
	load( file = fileName);
}

# If looking to subset the set of instruments: 
tickers = c('SPY','^VIX','^TNX','OIL','GOLD','^N225','^FTSE',  'GLD');  # left out:  'SPXS','SPXL'   
ADJCLOSE = ADJCLOSE[ , tickers];
ADJCLOSET = ADJCLOSET[ ,tickers];

#colnames(ADJCLOSE) =  c('SPY','VIX','TNX','OIL','GOLD','N225','FTSE', 'GLD'); # could rename the columns to get rid of the annoying "^" symbol...

nrDays = dim(ADJCLOSE)[1];

## Comput the 1-day log returns:
RET = log( ADJCLOSE[ 2: nrDays , ] / ADJCLOSE[ 1: (nrDays-1) , ] );		# print(head(RET))

## Drop the rows (i.e., days) which have at least one NA ("Not Available" entry):
nrNA_each_row = rowSums( is.na(RET) );		# print(nrNA_each_row);
okRows = nrNA_each_row == 0;
print(dim(RET));
RET = RET[ okRows , ];	print(dim(RET))


nrDays = dim(RET)[1];		days = rownames(RET);
nrTickers = dim(RET)[2];	tickers = colnames(RET);

##### In sample experiment 
#### Parts: a,b,c:

doWork_In_Sample = 1
if(doWork_In_Sample == 1){

	X_train = RET[1:nrDays - 1,];
	X_test = RET;
	STATS = NULL;
	CM = NULL;

	for (i in 1:nrTickers) {
		#print('#############################################');
		#print( paste('Stock = ',tickers[i] ) );

		daily_pnl = matrix(, nrow = nrDays - 101, ncol = 1);

		for (j in 101: (nrDays - 1)) {
			y_train = RET[ (j - 99) : j , tickers[i], drop=FALSE];
			X_train = RET[ (j - 100) : (j - 1) ,]
			y_hat = compute_linear_regression(X_train, y_train, X_test);
			if(y_hat[j] < 0) {
				daily_pnl[j - 100] = -RET[j + 1, i];
			}
			else {
				daily_pnl[j - 100] = RET[j + 1, i];
			}
		}

		cum_pnl = cumsum(daily_pnl);
		CM = cbind(CM,cum_pnl);
		mean_pnl = mean(daily_pnl, na.rm=TRUE);
		yearly_pnl = mean(daily_pnl, na.rm=TRUE) * 252;
		total_pnl = sum(daily_pnl, na.rm = TRUE);
		sharpe = compute_Sharpe_Ratio(daily_pnl);
		stats_this_stock = c(sharpe, mean_pnl, yearly_pnl, total_pnl)
		STATS = rbind(STATS,stats_this_stock);
	}
	
	rownames(STATS) = tickers;
	colnames(STATS) = c('sharpe', 'mean_pnl', 'yearly_pnl', 'total_pnl');
	print(STATS);
	colnames(CM) = tickers;
	plot_cumsum(CM);
}

return('DONE!');
}

#########################################################################################################
compute_Sharpe_Ratio = function(x){
	sh = mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE) * sqrt(252);  return(sh);
}


#########################################################################################################
plot_cumsum = function(CUM_SUM_MTX){
# INPUT: CUM_SUM_MTX, a matrix of size nrDays  x nrTickers 
# 	which plots the time series (one from each column) on the same plot.

nrTickers = dim(CUM_SUM_MTX)[2];
nrDays = dim(CUM_SUM_MTX)[1];

myColors = rainbow(nrTickers);  # generated however many colors are needed - one for each column.
# myColors = c('red','blue', 'green','black','magenta', 'cyan','yellow')

pdf(file = 'cumulative_pnl_across_time.pdf', height = 12,  width = 8);
plot (c(0, nrDays),c(-1,5),type='n', xlab ='days', ylab = 'pnl') # sets the x and y axes scales
for ( i in 1 : nrTickers){
	# plots the time series on the same graph
	lines( 1: nrDays, CUM_SUM_MTX[ ,i], col = myColors[i], lwd=2);  # , ylim = c(-1,1)
}

legend( 'topleft', legend = colnames(CUM_SUM_MTX), lty = 1, lwd = 2, col = myColors);  # , fill=TRUE
dev.off()

}

#########################################################################################################
compute_linear_regression = function(X_train, y_train, X_test){

# Training X matrix: X_train  of size n by p
# Training y vector: y_train  of size n by 1
# Test X matrix: X_test  of size k by p

# Performs multiple linear regression (without an intercept) on the training data, and applied the obtained coefficients to a test matrix.

colnames(y_train) = 'y'
both = data.frame(X_train, y  = y_train);
regObj = lm(y ~ . , data = both)  # with intercept  beta_0
# regObj = lm(y ~ . +0, data = both)  # without intercept  beta_0

#print(summary( regObj ) );   # see the summary of the regression

#rsq = summary( regObj )$r.squared;		# print( paste('R_square= ', rsq) );

# fit_residuals = resid( regObj );  # if you ever need the residuals from the training fit

# print(X_test)
# print(data.frame(X_test) )

# apply the beta's to X_test and compute your prediction:
X_test = data.frame(X_test);
y_hat_obj = predict(regObj, newdata = X_test, se.fit = TRUE)
y_hat = y_hat_obj$fit
coefficients = summary(regObj)$coefficients;  # if you ever need the actual coefficients (beta_i) i=0,1,...,p
#print(coefficients)

return(y_hat)
}

#########################################################################################################
run_PCA = function(X_train)

# Training X matrix: X_ train of size n by p

