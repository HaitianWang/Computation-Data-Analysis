library(ggplot2)

p1 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  theme(text = element_text(size = 12), aspect.ratio=1)

p2 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot() +
  coord_flip() +
  theme(text = element_text(size = 12), aspect.ratio=1)

grid.arrange(p1, p2, ncol=2)











