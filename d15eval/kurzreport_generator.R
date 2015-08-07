# source: http://stackoverflow.com/questions/15396755/using-loops-with-knitr-to-produce-multiple-pdf-reports-need-a-little-help-to
  # Load data

setwd("~/github/experiments/d15eval/")
source("./d15eval_setup.R")
setwd("~/github/experiments/d15eval/")
source("./d15eval_kurzy_setup.R")

# Prepare data

# Iterate

library("knitr")
for (ikurz in unique(kurzy_long$kurz)[c(3,12)]){
  knit2html("kurzreport_template.Rmd",
            output=paste0('./reporty_kurzy/report_', ikurz, '.html'),
            encoding = "CP1252")
}