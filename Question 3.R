#change the location to where the data is present.
setwd("c:/users/sxf190018/documents/buan6359/assignment")

#clear previous environment variables.
rm(list = ls())

#read the data into variable.
GSS2014 <- readRDS("GSS2014.rds")

#get a subset of relevant data.
GSS2014sub <- subset(GSS2014, select = c(CASEID, AGE, EDUC))

EDUCLow = head(sort(GSS2014sub$EDUC), 1)
EDUCHigh = tail(sort(GSS2014sub$EDUC), 1)

#plotting histogram.
hist(GSS2014sub$EDUC, 
     breaks = 10,
     freq = T,
     main = "Histogram of Ameriacan adult's Education in 2014",
     xlab = "Educaation in years",
     ylab = "Frequency",
     labels = T, #label the values
     xlim = c(EDUCLow, EDUCHigh),
     ylim = c(0,1000),
     col = "yellow",
     border = "red", 
     cex=5, cex.lab=1, cex.axis=1,
     las=1, #rotate the value of x-axis
)
