#' ---
#' title: "Histogram and Density Plot"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Liu"
#' institute: |
#'     | Department of Computer Science and Software Engineering
#'     | The University of Western Australia
#' graphics: yes
#' date: "Semester 2, 2023"
#' 
#' 
#' output:
#'   beamer_presentation:
#'     theme: "AnnArbor"
#'     colortheme: "dolphin"
#'     fonttheme: "structurebold"
#'     includes:
#'         in_header: "../common.tex"
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)

#' # Histogram
#' 
#' 
#' ## Histogram
## ----warning = FALSE, message = FALSE, out.width = '60%', size="small"----
custdata_v2 <- readRDS('../../data_v2/Custdata/custdata.RDS')
library(ggplot2)
ggplot(custdata_v2, aes(x=gas_usage)) +
geom_histogram(binwidth=10, fill="blue")

#' ## Reading a histogram
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{hist_gas.png}
#' \end{figure}
#' 
#' ## Most customers do not have gas heating?
#' 
#' Mixture of Numerical and Sentinel Values
#' 
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{gas_dict.png}
#' \end{figure}
#' 
#' * The values in the `gas_usage` column are a mixture of numerical values
#' and symbolic codes encoded as numbers.
#' * Options to deal with such cases:
#' 	- Convert numerical values 1-3 to NA, and 
#' 	- Add additional Boolean variables to indicate the possible cases.
#' 
#' ## Histogram Take-Away
#' 
#' * With the proper `binwidth`, histograms visually highlight where the data is \stressi{concentrated}, and point out the presence of potential \stressi{outliers and anomalies}.
#' * The primary disadvantage of histograms is that you must decide ahead of time how wide the bins are:
#' 	+ Bins too wide - you can lose information about the
#' shape of the distribution.
#' 	+ Bins too small - the histogram can look too
#' noisy to read easily.
#' 
#' 
#' # Density Plot
#' 
#' ## Density Plot - *A Continuous Histogram*
#' 
#' Can think of a \stressi{density plot} as a \stressi{continuous histogram} of a variable, except the area
#' under the density plot is rescaled to equal one. 
#' 
#' * A point on a density plot corresponds to the \stressi{fraction} of data (or the percentage of data, divided by 100) that takes on a particular value. 
#' * This fraction is usually very small. 
#' * When looking at a density plot, we should be more **interested in the overall shape** of the curve than the actual values on the y-axis.
#' 
#' ## Reading a plot and add annotation
#' 
#' 
#' \footnotesize
## ----warning = FALSE, message = FALSE, out.width = '90%'---------------
custdata_v1 <- read.table('../../data/custdata.tsv',header=T,sep='\t')
income_stat <- boxplot.stats(custdata_v1$income)$stats
income_stat_str <- paste(income_stat, collapse=" ")
library(scales)
fig <- ggplot(custdata_v1) + geom_density(aes(x=income)) +
  labs(y="density") +
  scale_x_continuous(labels=dollar, breaks=c(35000,200000,400000)) +
  annotate("text", x = 180000, y = 1e-05,
    label = paste("Most of the distribution is concentrated",
      "at the low end: less than $100,000 a year.", sep="\n")) +
  annotate("text", x = 400000, y = 1.5e-06,
    label = paste("Subpopulation of", "wealthy customers",
      "in the $400,000 range.", sep="\n")) +
  annotate("text", x = 550000, y = 1e-06,
    label = paste("Wide data range", "may consider log scale.",
	 sep="\n")) +
  annotate("text", x=350000, y = 1e-05, hjust=0,
    label=paste("stats: ", income_stat_str, sep="\n"))

#' \normalsize
#' 
#' ## The annotated plot 
#' 
## ----out.width = '75%'-------------------------------------------------
fig

#' 
#' ## When should we use a logarithmic scale
#' One should use a logarithmic scale when \stress{percent change} or \stress{change in orders of magnitude} is more important
#' than \stress{changes in absolute units}. 
#' 
#' - In other words, the absolute unit changes can be interpreted differently in different context.
#'     + For example, in income data, a $5,000 difference in income means something very different in a population where the incomes tend to fall in the $10,000 range, than it does in populations where incomes fall in the $100,000-$1000,000 range.
#'     + What constitutes a "significant difference" depends on the order of magnitude of the incomes you're looking at.
#' 
#' - A log scale should be used to better visualize data that is heavily skewed.
#'     + For example, a few people with very high income will cause the majority of the data to be compressed into a relatively small area of the graph. 
#'     
#' ## Plotting on a logarithmic scale
#' 
#' \footnotesize
## ----out.width = '55%', warning=F--------------------------------------
ggplot(custdata_v1) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,35000,200000),labels=dollar) +
  annotation_logticks(sides="bt") + theme(text = element_text(size = 18))

#' \normalsize
#' 
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{warning.png}
#' \end{figure}
#' 
#' 
#' 
#' ## References
#' 
#' - **Practical Data Science with R**, *Nina Zumel, John Mount*, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapter 3)
#' 
#' - Understanding and Interpreting Box Plots: https://www.wellbeingatschool.org.nz/information-sheet/understanding-and-interpreting-box-plots
#' 
#' 
