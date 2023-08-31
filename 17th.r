
library(dplyr)

customer_data <- data.frame(
  gas_usage = c(1, 5, 3, 2, 6, 1, 7, 2, 8, 3)
)

print(customer_data)

customer_data <- customer_data %>%
  mutate(
    gas_with_rent = (gas_usage ==1),
    gas_with_electricity = (gas_usage == 2),
    no_gas_bill = (gas_usage == 3)
  ) %>%
  mutate(
    gas_usage = ifelse(gas_usage < 4,
                       NA,
                       gas_usage
    )
  )
print(customer_data)






