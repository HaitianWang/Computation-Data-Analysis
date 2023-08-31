custdata_v2 <- data.frame(
  age = c(25, 0, 35, 110),
  income = c(2500, -1500, 3200, 5000)
)

library(dplyr)

customer_data <- mutate(custdata_v2,
                        age = na_if(age, 0),
                        income = ifelse(income < 0, NA, income))

print(customer_data)


