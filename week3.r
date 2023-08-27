library(ggplot2)
library(gridExtra)


mat = matrix(rnorm(50), nrow = 5)

indices = sample(1:50, 10)
mat[indices] = NA


df = as.data.frame(mat)
rownames(df) = paste("Patient", 1:5)
colnames(df) = paste("Test", 1:10)

# print(indices)
# print(df)


# subset1 = df[1:2, 1:3]
# print(subset1)
# 
# subset2 = df[-1,-1]
# print(subset2)


# subset3 <- df[c("Patient 1", "Patient 3"), c("Test 1", "Test 3")]
# print(subset3)
# 
# 
# subset4 = df[df[["Test 1"]] > 0, "Test 1"]
# print(subset4)


winedata <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")
#print(winedata)

# head(winedata)
# 
# str(winedata)
# 
# summary(winedata)


p1 <- ggplot(winedata, aes(x=residual.sugar)) + 
  geom_histogram(binwidth=0.5, fill="blue", color="black", alpha=0.7) + 
  labs(title="Histogram of Residual Sugar", x="Residual Sugar", y="Count")

p2 <- ggplot(winedata, aes(x=fixed.acidity)) + 
  geom_histogram(binwidth=0.5, fill="red", color="black", alpha=0.7) + 
  labs(title="Histogram of Fixed Acidity", x="Fixed Acidity", y="Count")

# Display the histograms side-by-side
grid.arrange(p1, p2, ncol=2)







