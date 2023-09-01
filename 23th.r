library(ggplot2)
library(scales)

ggplot(data = custdata,
       mapping = aes(x=age, y=as.numeric(health.ins))) +
  geom_jitter(alpha = 1/5, height = 0.1) + geom_smooth() +
  theme(text = element_text(size=16))



