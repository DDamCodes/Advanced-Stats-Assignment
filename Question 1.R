rm(list = ls())
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
