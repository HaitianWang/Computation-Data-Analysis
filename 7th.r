x <- rnorm(10000); boxplot(x, col="gold"); grid()
cat(boxplot.stats(x)$stats)


gender <- c(rep("male",20), rep("female", 30))
gender1 <- factor(gender)
print(gender1)



