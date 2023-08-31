
library(vtreat)

set.seed(123)

data <- data.frame(
  x1 = sample(c("A", "B", "C", NA), 100, replace = TRUE),
  x2 = rnorm(100),
  y = rnorm(100)
)

treatplan <- designTreatmentsZ(data, colnames(data)[1:2])

treated_data <- prepare(treatplan, data)














