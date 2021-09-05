#change the directory to where the data resides.
# setwd("c:/users/sxf190018/documents/buan6359/assignment")

#clear environment
rm(list = ls())

#include this library
library("dplyr")

#fetch the data into variable
olympics <- readRDS("OlympicsMedal.rds")

#find the starting year from the table.
StartYear <- head(sort(olympics$Year), 1)

#find the end year from the graph.
EndYear <- tail(sort(olympics$Year), 1)

#time series graph for USA.
USATS <- ts(olympics$`United States`,
            start = c(1924),
            end = c(2014),
            frequency = 0
)

#time series graph for Canada.
# CanadaTS <- ts(olympics$Canada,
#                start = c(1924, 1),
#                end = c(2014, 12),
#                frequency = 1
# )

#plot both ts into single graph.
ts.plot(USATS,
        main = "Time Series Graph: Olympic Medal",
        ylab = "No of medals obtained",
        xlab = "Year",
        col = c("blue", "red"),
        type="l",
        lty = c(1)
)

legend('topleft',
       legend = c("USA","Canada"),
       col = c("blue","red"),
       lwd = 1,
       lty = 1,
       bty = "n",
       cex = 0.7)
