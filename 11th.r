library("ggplot2")

custdata <- read.delim('custdata.tsv', as.is=FALSE)
#theme_set(theme_grey(base_size = 18))
pairs(custdata)

pairs(~sex+age+income+health.ins, data=custdata)

pairs(~displ+cty+hwy, data=mpg)



















