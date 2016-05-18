#CalculateStockReturns
#Use R to calculate returns for Stocks and other securities

#import the stock data for Dell from github.
#In your Terminal, navigate to your working directory for R and clone this 
#repository (using https://github.com/mshreera/CalculateStockReturns.git) 
# $ git clone https://github.com/mshreera/CalculateStockReturns.git 

ddata = read.csv("Dell_HistoricalPrices.csv", header = TRUE, stringsAsFactors = F)

#Take a look at the Adjusted Close (Adj.Close) for the first few rows
ddata[1:5,7]

#Let's make a new dataframe now where the row names are the dates
d_prices_df = ddata[, "Adj.Close", drop=FALSE]
rownames(d_prices_df) = ddata$Date
head(d_prices_df)

#Plot the adjusted close prices
plot(d_prices_df$Adj.Close, 
     type = "l", 
     col = "blue", 
     lwd = 2, 
     ylab = "Adjusted close",
     main = "Monthly closing price of Dell")
#add a legend
legend(x='topleft',legend='Dell', lty=1, lwd=2, col='blue')

#Let's calculate the simple returns by subtracting the previous month (n-1) 
#from the current month, n
n = nrow(d_prices_df)
d_ret = (d_prices_df[2:n,1]-d_prices_df[1:n-1,1])/d_prices_df[1:n-1,1]


#add dates to the new vector d_ret and double check
names(d_ret) = d_prices_df[2:n, 1]
head(d_ret)

#Now let's calculate continuously compounded 1 month returns
#and assign names and double check
d_ccret = log(d_prices_df[2:n, 1]) - log(d_prices_df[1:(n-1), 1]) 
names(sbux_ccret) = d_prices_df[2:n,1] 
head(d_ccret)

#let's compare simple and continuously compounded returns
head(cbind(d_ret, d_ccret))

#Now we'll graph the simple & compounded returns
plot(d_ret, type="l", col="blue", lwd=2, ylab="Return",
     main = "Monthly Returns on Dell")

#add continuously compounded returns
lines(d_ccret, type = "l", col="red", lwd ="2")

#add a horizontal line at 0
abline(h=0)

#add a legend
legend(x="bottomright", legend=c("Simple", "CC"),lty=1, lwd=2, col=c("blue","red"))
