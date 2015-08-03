# Sys.setlocale(category = "LC_ALL", locale = "Czech_Czech Republic.1252")
Sys.setlocale(category = "LC_ALL", locale = "cs_CZ.UTF-8")

library(pbtools)
library(dplyr)
library(reshape2)
library(tidyr)
library(Cairo)

loc <- "C:/Users/boupet/Downloads/eval"
loc <- "~/Documents/Work/Discover/Discover 2015/Evaluace/data_final/"

## Load data using LimeSurvey script ######

setwd(loc)
getwd()
source("survey_317642_R_syntax_file.R")

# Fix incompletes #########

# need to merge #3 and #8 od výběru kurzů dál (column 66 and on in "data")