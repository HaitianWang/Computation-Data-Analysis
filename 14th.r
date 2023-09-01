library(ggplot2)

ggplot(data = custdata) +
  geom_point(mapping = aes(x = age, y = income)) +
  facet_wrap(~ marital.stat, nrow = 2)

# ggplot(data = custdata) +
#   geom_point(mapping = aes(x = age, y = log(income))) +
#   facet_grid( sex ~ marital.stat )













