# source("/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/R_code/load_yahoo_data/test_correlations.r");    test_correlations();

test_correlations = function(Instr){

# The instruments in the universe (different 'stocks')
tickers = c('SPY','^VIX','^TNX','OIL','GLD','GOLD','^N225','^FTSE', 'SPXS','SPXL');
# install.packages('quantmod');  # library('quantmod');  # ended up not using this package

# The file to which we will be saving the yahoo data
fileName = '/Users/Atranemus-PC/Documents/R/Homework 1/ADJCLOSE.data';

doWork = 0 # doWork = 1:  load data from Yahoo;   doWork = 0:  preload it from file
if(doWork == 1){ 
	ADJCLOSE  = loadYahooData_saveToFile(tickers, dayStart='2010-01-01', dayEnd='2015-09-15', fileName= fileName)
}
else{
	load( file = fileName);
}


# If looking to subset the set of instruments: 
# tickers = c('SPY','^VIX','^TNX','OIL','GLD','GOLD','^N225','^FTSE');  # , 'SPXS','SPXL'
tickers = Instr;    ADJCLOSE = ADJCLOSE[ , tickers];  # ,'SPXL'

nrDays = dim(ADJCLOSE)[1];
RET = log( ADJCLOSE[ 2: nrDays , ] / ADJCLOSE[ 1: (nrDays-1) , ] );		# print(head(RET))

nrNA_each_row = rowSums( is.na(RET) );		# print(nrNA_each_row);
okRows = nrNA_each_row == 0;
print(dim(RET));
RET = RET[ okRows , ];	#print(ADJdim(RET))

nrDays = dim(RET)[1];
tickers = colnames(RET);
nrTickers = dim(RET)[2];
days = rownames(RET);
tickers = c('l(SPX)','^VIX');
colnames(RET) = tickers;
list_CorMats = list();

RET[,1] = RET[,1]*1.5 + RET[,2]/5

#Compute Sample Mean and Var
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

#Create Histograms
#install.packages('gplots');
#library('gplots');

pdf("Ticker_Log_Data.pdf", width = 8.5, height = 11);
par(mfrow = c(4,3))
for (i in 1:nrTickers) {
	hist(RET[,i], main = tickers[i], xlab = 'Log Return', col = "red");
}
textplot(SampleMean);
textplot(SampleVar);
dev.off();

print('Histograms, Mean, and Variance saved to file.');

P_cor = cor(RET, method = 'pearson');	 	
P_cor = round(P_cor,3); 	
print('P_cor:'); print(P_cor);
list_CorMats[['Pearson']] = P_cor

S_cor = cor(RET, method = 'spearman');  	
S_cor = round(S_cor,3); 	
print('S_cor:'); print(S_cor);
list_CorMats[['Spearman']] = S_cor

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

### Add here code needed for the other correlation measures

# Turn lists of n - by- n matrices, into a 3-dim array
Array3D_Cors = turn_list2D_into_array3D(list_CorMats);  # a 3-dimesional array: tickers x tickers x correationType. Each "slice" is a different correlation matrix

save(file = 'saved_RET_Array3DCors.data', RET , Array3D_Cors);  print('Saved RET  and  Array3D_Cors to file');
plot_results(RET , Array3D_Cors);

return('END');
}



##############################################################################################################################
# source("/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/R_code/load_yahoo_data/load_yahoo_data.r");  plot_results();
plot_results = function(RET , Array3D_Cors){
## Plot results to pdf file:
	
# load(file = 'saved_RET_Array3DCors.data');  # RET , Array3D_Cors
print(dim(RET))
print(dim(Array3D_Cors))

# tickers = c('SPY','SPXS','SPXL');  RET = RET[, tickers];

nrDays = dim(RET)[1];    	days = rownames(RET);
nrTickers = dim(RET)[2];	tickers = colnames(RET);

pdf('Scatterplot_all_pairs.pdf', width = 22, height = 12);
par(mfrow = c(3,5))
par(mar = c(5, 5, 1, 1));  # default is c(5, 4, 4, 2) + 0.1 

# opar <- par(mfrow = c(3,5), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
	# opar <- par( oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
	# on.exit(par(opar))

# nrTickers = 3;

for (i in 1:(nrTickers-1)){
	for (j in (i+1):nrTickers){		
		plot(RET[,i], RET[,j], type='p', pch='.', xlab = tickers[i], ylab = tickers[j], cex.axis = 1.2, cex.lab = 2);
		# points(RET[,i], RET[,j], xlab = tickers[i], ylab = tickers[j], cex.axis = 1.2);
		x = RET[,i];  y = RET[,j];
		abline( lm(y~x), col="red", lwd=1.5);
		
		cors = round( Array3D_Cors[ tickers[i] , tickers[j], ] , 3)
		cors = paste( names(cors),': ', cors, sep='' )
		# legend( 'bottomleft', inset = c(-0.1, -0.1), legend = cors, cex=1.5, col='blue', text.col ='blue');
		 add_legend("bottomleft", legend=cors, pch=20,  horiz=FALSE, bty='n', cex=1.3, col='red', text.col ='red');
	}
}
dev.off();
print('Saved to pdf file');

return('End');
}


add_legend <- function(...) {
  # If want a plot on each page, with the legend outside:
	  # opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
	  # opar <- par( oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0))
	  # on.exit(par(opar))
	  #lines(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}





# source("/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/R_code/load_yahoo_data/test_correlations");  test_correlations();
turn_list2D_into_array3D = function(list_CorMats){
## Takes as input a list of length k, holding k different n -by- n arrays, and returns a 3-dimensional array of size n -by- n -by- k

# load(file = 'temp_list_CorMats.data');

ThreeDimArray = array(0, dim = c( dim(list_CorMats[[1]]) , length(list_CorMats) ) );
dimnames(ThreeDimArray)[[1]] = rownames(list_CorMats[[1]]);
dimnames(ThreeDimArray)[[2]] = colnames(list_CorMats[[1]]);
dimnames(ThreeDimArray)[[3]] = names(list_CorMats); 

for (x in names(list_CorMats) ){
	ThreeDimArray[,,x] = list_CorMats[[ x ]];		
} 

return(ThreeDimArray);
}


##############################################################################################################################
loadYahooData_saveToFile = function(tickers, dayStart, dayEnd, fileName){
# Input: 
	# tickers = set of instruments/stocks
	# dayStart, dayEnd = the interval of time
	# fileName = the file to which the data is saved (to avoid reloading it each time from yahoo)
# Output: saves to file and returns the ADJCLOSE matrix of size days x tickers, holding the daily closing prices
	#  This can be easily modified to return other information: opening prices, volume information, daily min and max
	
dataYahooList = NULL;  
print('Loading data from Yahoo Finance ...');

	for (tick in tickers){	print(tick);
		dataYahooList[[ tick ]] = try( dataLoadYahoo(tick, dayStart, dayEnd) );
	}

	# Do some data cleaning...
	ans = remove_Error_Tickers(dataYahooList);   dataYahooList = ans[[1]];   error_Tickers = ans[[2]];
	print(error_Tickers);

	# print(dataYahooList[[ 1 ]]);
	nrDays = dim( dataYahooList[[ 1 ]] [['adj.close']])[1];
	days = rownames(dataYahooList[[ 1 ]] [['adj.close']]);


	# Build the prices matrix ADJCLOSE  (holding the end of day closing prices)
	ADJCLOSE = NULL; okTickers = NULL;
	tickers = names(dataYahooList);		print(tickers);
	nrTickers = length(tickers);
	for (tick in tickers){
		temp = dataYahooList[[ tick ]];   temp = temp[['adj.close']];  	# print(dim(temp));
		common = intersect( rownames(temp), days);
		if ( length(common) == nrDays ){
			temp = temp[ days, ];
			ADJCLOSE = cbind(ADJCLOSE, temp);
			okTickers = c(okTickers,tick);
		}
}

	rownames(ADJCLOSE) = days;
	colnames(ADJCLOSE) = okTickers;
	print(head(ADJCLOSE))
	print(dim(ADJCLOSE));

	ADJCLOSET = ADJCLOSE;
	save(ADJCLOSET, file = fileName);
	
	print('Saved end-of-day-prices to file for future use.');



# If you also want to save to CSV file
#fileCSV = '/Users/Mihai/Google Drive/UCLA_Courses/191_Data_Science_Fall_2015/R_code/load_yahoo_data/ADJCLOSE.csv';
#write.table(ADJCLOSE , file = fileCSV, sep=',');

return(ADJCLOSE);
}




#####################################################################################################################################
remove_Error_Tickers = function(dataYahooList){
## For data cleaning purposes...	

all = names(dataYahooList);  # print(all);
okTickers = NULL;
error_Tickers =  NULL;   # The yahoo query came back with Error
for (i in all){  # print(i);
	if (substr( dataYahooList[[ i ]],1,3) =='Err'){
		error_Tickers = c(error_Tickers, i);
		print(i);
	}
	else{
	okTickers = c(okTickers,i);
	}
}

print('error_Tickers:');  print(error_Tickers); 
print(paste('Nr error_Tickers=', length(error_Tickers)));
# print(okTickers);
nrTickers = length(okTickers);  			print(paste('nr OK Tickers=', nrTickers));
dataYahooList = dataYahooList[okTickers];	print(length(dataYahooList));
	
ans = list(dataYahooList , error_Tickers);
return(ans)
}


# GLD: Launched in November 2004, SPDR Gold Trust (GLD) is a passively managed fund designed to deliver the return of the spot gold prices. With AUM of about $26.2 billion, GLD is the largest and most popular ETF in the precious metal space.




dataLoadYahoo = function(tickers, start.date, end.date){
## Scraping finance data from yahoo finance:

  # Change the locale
  sl <- Sys.setlocale(locale="US")
 
  # Create the universe of dates
  all.dates <- seq(as.Date(start.date), as.Date(end.date), by="day")
  all.dates <- subset(all.dates,weekdays(all.dates) != "Sunday" & weekdays(all.dates) != "Saturday")
  all.dates.char <- as.matrix(as.character(all.dates))
 
  # Create sparse matrices
  open <- matrix(NA, NROW(all.dates.char), length(tickers))
  hi <- open
  low <- open
  close <- open
  volume <- open
  adj.close <- open
 
  # Name the rows correctly
  rownames(open) <- all.dates.char
  rownames(hi) <- all.dates.char
  rownames(low) <- all.dates.char
  rownames(close) <- all.dates.char
  rownames(volume) <- all.dates.char
  rownames(adj.close) <- all.dates.char
 
  # Split the start and end dates to be used in the ULR later on
  splt <- unlist(strsplit(start.date, "-"))
  a <- as.character(as.numeric(splt[2])-1)
  b <- splt[3]
  c <- splt[1]
 
  splt <- unlist(strsplit(end.date, "-"))
  d <- as.character(as.numeric(splt[2])-1)
  e <- splt[3]
  f <- splt[1]
 
  # Create the two out of the three basic components for the URL loading
  str1 <- "http://ichart.finance.yahoo.com/table.csv?s="
  str3 <- paste("&a=", a, "&b=", b, "&c=", c, "&d=", d, "&e=", e, "&f=", f, "&g=d&ignore=.csv", sep="")
 
  # Main loop for all assets
  for (i in seq(1,length(tickers),1))
    {
      str2 <- tickers[i]
      strx <- paste(str1,str2,str3,sep="")
      x <- read.csv(strx)
 
      datess <- as.matrix(x[1])
 
      replacing <- match(datess, all.dates.char)
      open[replacing,i] <- as.matrix(x[2])
      hi[replacing,i] <- as.matrix(x[3])
      low[replacing,i] <- as.matrix(x[4])
      close[replacing,i] <- as.matrix(x[5])
      volume[replacing,i] <- as.matrix(x[6])
      adj.close[replacing,i] <- as.matrix(x[7])
  }
 
  # Name the cols correctly
  colnames(open) <- tickers
  colnames(hi) <- tickers
  colnames(low) <- tickers
  colnames(close) <- tickers
  colnames(volume) <- tickers
  colnames(adj.close) <- tickers
 
  # Return the ouput
  return(list(open=open, high=hi, low=low, close=close, volume=volume, adj.close=adj.close))
}
