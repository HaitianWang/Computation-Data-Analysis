library(ggplot2)
library(scales)


custdata<- read.table('custdata.tsv',
                      header=T,sep='\t')

boxplot(custdata$age, notch=TRUE, col="gold")

boxplot.stats(custdata$age)$stats

boxplot.stats(custdata$age)

print(custdata$age)




























