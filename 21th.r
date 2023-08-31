
library(vtreat)

varlist <- setdiff(colnames(customer_data), c("custid", "health_ins"))

treatment_plan <- design_missingness_treatment(customer_data, varlist = varlist)

training_prepared <- prepare(treatment_plan, customer_data)

nacounts <- count_missing(training_prepared)

sum(nacounts)

print(nacounts)











