## ----echo=TRUE, eval=FALSE----------------------------------------------------
wineData <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")
fixedAcidity <- hist(wineData$fixed.acidity)

# a better solution is:
counts <- integer(12)  # initialise `counts` to a zero integer vector of 13 elements long

index <- 1
bins <- seq(4,15)
for (b in bins) {
    counts[index] <- fixedAcidity$counts[fixedAcidity$mids > b & fixedAcidity$mids <= b+1]
    index <- index + 1
}
counts

## ----eval=TRUE, echo=FALSE, out.width='60%'-----------------------------------
name_str <- paste("(", bins, ",", bins+1, "]", sep="")
barplot(counts, main="Histogram with bin-size 1", ylab="counts", xlab="fixed acidity", name=name_str)


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
freqs <- counts / sum(counts)


## ----eval=TRUE, echo=FALSE, out.width='60%'-----------------------------------
barplot(freqs, main="Density histogram with bin-size 1", ylab="density", xlab="fixed acidity", name=name_str)


## ----eval=TRUE, echo=F, out.width='60%'---------------------------------------
library(ggplot2)
ggplot(data = wineData, mapping = aes(x = fixed.acidity,  y=..density..)) +
  geom_histogram(binwidth=1) +
  geom_density(colour = "blue", alpha=0.5 )


## ----echo=FALSE, eval=TRUE, out.width='60%'-----------------------------------
column_idx <- names(wineData) != "quality"
boxplot(wineData[,column_idx])


## ----eval=TRUE, echo=TRUE-----------------------------------------------------
ggplot(stack(wineData)) +
  geom_boxplot(mapping = aes(x = ind, y = values)) +
  labs(x='variable', y='value') +
  coord_flip() 


## ----echo=FALSE, eval=TRUE----------------------------------------------------
boxplot.stats(wineData$fixed.acidity)$stats


## ----echo=TRUE, eval=TRUE-----------------------------------------------------
median_index <- floor(length(wineData$fixed.acidity)/2) + 1
sorted <- sort(wineData$fixed.acidity)
cat("The median value of fixed.acidity is", sorted[median_index])


## ----echo=FALSE, eval=TRUE----------------------------------------------------
myMedian <- function(v) {
   median_index <- floor(length(v)*0.5) + 1
   sorted <- sort(v)
   return (sorted[median_index])
}


## ----echo=TRUE, eval=TRUE, collapse=T-----------------------------------------
print(myMedian(wineData$fixed.acidity))


## ----echo=FALSE, eval=TRUE----------------------------------------------------
myQuantile <- function(v, q=0.5) {
   index <- floor(length(v)*q) + 1
   sorted <- sort(v)
   return (sorted[index])
}


## ----echo=TRUE, eval=TRUE, collapse=T-----------------------------------------
cat("The lower quartile is", myQuantile(wineData$fixed.acidity, q=0.25))
cat("The upper quartile is", myQuantile(wineData$fixed.acidity, q=0.75))
