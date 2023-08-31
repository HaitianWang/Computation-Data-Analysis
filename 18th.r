count_missing <- function(df) {
  sapply(df, FUN = function(col) sum(is.na(col)) )
}

custdata <- read.table('custdata.tsv',
                       header=T, sep='\t')

nacounts <- count_missing(custdata)
hasNA = which(nacounts > 0)
nacounts[hasNA]


















