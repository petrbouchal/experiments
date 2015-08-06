library(pbtools)
library(dplyr)
library(reshape2)
library(tidyr)
library(Cairo)

if(whatplatform()=="win") {
  Sys.setlocale(category = "LC_ALL", locale = "Czech_Czech Republic.1252")
  loc <- "C:/Users/boupet/Downloads/eval"
  options(encoding = "UTF-8")
  } else {
  Sys.setlocale(category = "LC_ALL", locale = "cs_CZ.UTF-8")
  loc <- "~/Documents/Work/Discover/Discover 2015/Evaluace/data_final/"
  }



## Load data using LimeSurvey script ######

setwd(loc)
getwd()
source("survey_317642_R_syntax_file.R")

# codebook here:
# http://localhost/index.php/admin/expressions/sa/survey_logic_file/sid/317642

# remove false starts
data <- data[!(data$id %in% 14:16),]

# merge records where respondent had to restart
# (identified visually by ID and note)
data[data$id==3,66:249] <- data[data$id==11,66:249] # merge
data <- data[data$id!=11,] # remove line for second entry

# now we have one incomplete and one missing (did not respond) and two 
# missing last pages (computer broke down)

# check incompletes

table(is.na(data[,1]))
table(is.na(data[,151]))
table(is.na(data[,153]))
