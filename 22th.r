library(ggplot2)
library(scales)
ggplot(data = custdata,
       mapping = aes(x=income, y=as.numeric(health.ins))) +
  geom_jitter(alpha=1/5, height = 0.1) + geom_smooth() +
  scale_x_log10(breaks = c(100,1000,10000,100000,1000000), labels=dollar) +
  labs(x="income (log10 scale)", y="health.ins")
























