
library(crayon)
library(knitr)


little_bunny <- function(name) {
  return("Little bunny " %+% name)
}
hop <- function(data, through) {
  return(data %+% "\nWent hopping through the " %+% through)
}
scoop <- function(data, up) {
  return(data %+% "\nScooping up the " %+% up)
}
bop <- function(data, on) {
  return(data %+% "\nAnd bopping them on the " %+% on)
}


customer_data <- customer_data %>%
  mutate(
    gas_with_rent = (gas_usage ==1),
    gas_with_electricity = (gas_usage == 2),
    no_gas_bill = (gas_usage == 3)
  ) %>%
  mutate(
    gas_usage = ifelse(gas_usage < 4, NA, gas_usage)
  )


merge(authors, books, by.x="surname", by.y="name") %>%
  subset(nationality=="NZ") %>%
  kable()


jack_degree <- students %>%
  filter(name == "Jack") %>%
  select(degree)

jack_units <- units %>%
  filter(degree %in% jack_degree$degree) %>%
  select(unit)















