#Group Members - Debjeet Dam, Sarika Francis, Tyler Scott

#change the location to where the data is present.
setwd("c:/users/sxf190018/documents/buan6359/assignment")

#clear previous environment variables.
rm(list = ls())

#include this library
library("dplyr")

#--------------------------------QUESTION 1------------------------------------#

workload <- readRDS("workload.rds")
workload$label <- factor(workload$`Home workload`,
                         levels=c(1,2,3,4,5,6),
                         labels=c("Mom: Full time; 
                                  Dad: Full time", 
                                  "Mom: Part time; 
                                  Dad: Full time", 
                                  "Mom: Not employed; 
                                  Dad: Full time",
                                  "Mom: Full time; 
                                  Dad: Part time or not employed",
                                  "Mom: Not employed; 
                                  Dad: Not employed",
                                  "Other"))
library("dplyr")
n=nrow(workload); n #total number of observations
Freq <- workload %>%
  group_by(label) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)) %>%
  na.omit(workload$`Home workload`); Freq

options(digits=3)
Rel.Freq<- workload %>%
  group_by(label) %>%
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)) %>%
  mutate('Relative Frequency'=Frequency/n) %>%
  na.omit(workload$`Home workload`); Rel.Freq

par(mar=c(14,6,4,4)) #Increase the plot section margin size
count<- table(workload$label); count
barplot(count, 
        main="Parents' Workload",
        xlab="Workload", 
        ylab="Frequency", 
        ylim=c(0,500),
        border="red", col="lightblue",
        las=2,
)

par(mar=c(4,6,4,4))
piepercent<- round(count/sum(count)*100, 1); piepercent
pie(piepercent, 
    labels=paste(piepercent,"%",sep=""),
    main="Parents' Workload (Percent)",
    radius=0.8,
    col=rainbow(length(count)))

legend("bottomleft", 
       c("Mom: Full time; Dad: Full time", 
         "Mom: Part time; Dad: Full time", 
         "Mom: Not employed; Dad: Full time",
         "Mom: Full time; Dad: Part time or not employed",
         "Mom: Not employed; Dad: Not employed",
         "Other"),
       cex=1,
       bty='n',
       fill=rainbow(length(piepercent))
)

#--------------------------------QUESTION 2------------------------------------#

#Load up MBA.rds data into the R data frame
MBA<- readRDS("MBA.rds")

#Add two new features to the data frame:MBA
MBA$Degreelabel <- factor(MBA$Degree,
                          levels=c(1,2,3,4),
                          labels=c("BA",
                                   "B.Eng",
                                   "BBA",
                                   "Other"))
levels(MBA$Degreelabel)

##Table of Frequencies
n=nrow(MBA); n #Total number of observations
addmargins(with(MBA,table(Degreelabel,University)))

#Row-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=1) %>% #value of each cell is divided by the sum of the row cells
  round(2)

#Column-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=2) %>% #each cell is divided by the sum of the column cells.
  round(2)

#Overall-relative Frequencies
with(MBA,table(Degreelabel,University)) %>% 
  prop.table(margin=) %>% #default is "margin=", which calculates proportions over the entire table.
  round(2)

#Graphing the Relationship b/w two Nominal Variables
#Comparison Bar Graph
count<- table(MBA$Degreelabel,MBA$University); count
barplot(count, 
        main="Two-dimensional Bar Chart",
        xlab="University", 
        ylab="Frequency", 
        col=c("darkblue","red","green", "yellow"),
        cex=1.2, cex.axis=1.2,cex.lab=1.2,
        legend=rownames(count), beside=TRUE, args.legend = list(bty='n', x='top', ncol=2))

#--------------------------------QUESTION 3------------------------------------#

#clear previous environment variables.
rm(list = ls())

#include this library
library("dplyr")

#read the data into variable.
GSS2014 <- readRDS("GSS2014.rds")

#get a subset of relevant data.
GSS2014sub <- subset(GSS2014, select = c(CASEID, AGE, EDUC))

#Calculating the minimum and maximum years of education
EDUCLow = min(GSS2014sub$EDUC, na.rm = TRUE)
EDUCHigh = max(GSS2014sub$EDUC, na.rm = TRUE)

par(mar = c(6, 6, 6, 4))
#plotting histogram.
hist(GSS2014sub$EDUC, 
     breaks = 10,
     freq = T,
     main = "Histogram of American adult's Education in 2014",
     xlab = "Education in Years",
     ylab = "Frequency",
     labels = T, #label the values
     xlim = c(EDUCLow, EDUCHigh),
     ylim = c(0,1000),
     col = "yellow",
     border = "red", 
     cex=5, cex.lab=1, cex.axis=1,
     las=1, #rotate the value of x-axis
)
 
#--------------------------------QUESTION 4------------------------------------#

#clear environment
rm(list = ls())

#include this library
library("dplyr")

#fetch the data into variable
olympics <- readRDS("OlympicsMedal.rds")

#find the starting year from the table.
StartYear <- min(olympics$Year)

#find the end year from the graph.
EndYear <- max(olympics$Year)

USATS <- ts(olympics$`United States`,
            start = c(1924),
            frequency = 0.25
)

#time series graph for Canada.
CanadaTS <- ts(olympics$Canada,
                start = c(1924),
                frequency = 0.25
)

#plot both ts into single time series graph.
ts.plot(USATS, CanadaTS,
        main = "Time Series Graph: Olympic Medal",
        ylab = "No of medals obtained",
        xlab = "Year",
        xlim = c(StartYear, EndYear),
        col = c("blue", "red"),
        type="l",
        lty = c(1)
)
legend('topleft',
       legend = c("USA","Canada"),
       col = c("blue","red"),
       lwd = 1,
       lty = 1,
       cex = 0.7)


#--------------------------------QUESTION 5------------------------------------#

#clear environment
rm(list = ls())

#include this library
library("dplyr")

#Load up GSS2014.rds data set into data frame
GSS2014<- readRDS("GSS2014.rds")

#calculating mean
mean(GSS2014$INCOME, na.rm=TRUE)

#calculating the median
median(GSS2014$INCOME, na.rm=TRUE)

#calculating the range
Range<- max(GSS2014$INCOME, na.rm = TRUE) - min(GSS2014$INCOME, na.rm = TRUE); Range

#calculating standard deviation
sd(GSS2014$INCOME, na.rm=TRUE)

#calculating coefficient of variance
c.v.<- sd(GSS2014$INCOME, na.rm=TRUE)/mean(GSS2014$INCOME, na.rm=TRUE); c.v.

#calculating quantile
quantile(GSS2014$INCOME, na.rm=TRUE)

#calculating coefficient of correlation
cor(GSS2014$INCOME, GSS2014$EDUC, use = "na.or.complete")

#Getting minimum and maximum value of Income
IncomeMin <- min(GSS2014$INCOME, na.rm = TRUE)
IncomeMax <- max(GSS2014$INCOME, na.rm = TRUE)

#Getting minimum and maximum value of Educ
EducMin <- min(GSS2014$EDUC, na.rm = TRUE)
EducMax <- max(GSS2014$EDUC, na.rm =  TRUE)

par(mar = c(4.5, 6, 4, 4))
#scatter plot
plot(GSS2014$EDUC, GSS2014$INCOME,
     main="Scatter Plot",
     ylab="Income",
     xlab="Education",
     ylim = c(IncomeMin, IncomeMax),
     xlim = c(EducMin, EducMax),
     col="blue",
     pch=20,
     cex=1.2, cex.lab=1.2, cex.axis=1.2)

