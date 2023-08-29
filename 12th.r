
library(ggplot2)
library(gridExtra)

p1 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "point geom")


p2 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  labs(title = "smooth geom")

grid.arrange(p1, p2, ncol=2)





















