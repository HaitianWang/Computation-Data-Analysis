
library(hexbin)
library(ggplot2)

# Sample data
set.seed(123)
x <- rnorm(10000)
y <- rnorm(10000)

# Using ggplot2 to create a hexbin plot
ggplot(data = data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_hex()











