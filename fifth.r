library(ggplot2)
library(scales)

custdata_v1 <- read.table('custdata.tsv',header=T,sep='\t')

income_stat <- boxplot.stats(custdata_v1$income)$stats

income_stat_str <- paste(income_stat, collapse=" ")

ggplot(custdata_v1) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,35000,200000),labels=dollar) +
  annotation_logticks(sides="bt") + theme(text = element_text(size = 18))






























