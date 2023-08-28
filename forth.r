library(ggplot2)
library(scales)
custdata_v1 <- read.table('custdata.tsv',header=T,sep='\t')

income_stat <- boxplot.stats(custdata_v1$income)$stats

income_stat_str <- paste(income_stat, collapse=" ")


ggplot(custdata_v1) + geom_density(aes(x=income)) +
  labs(y="density") +
  scale_x_continuous(labels=dollar, breaks=c(35000,200000,400000)) + annotate("text", x = 180000, y = 1e-05, label = paste("Most of the distribution is concentrated", "at the low end: less than $100,000 a year.", sep="\n"))

  


































