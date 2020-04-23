library(cogsciutils); library(data.table)
d <- fread("../../data/processed/study1_demographics.csv", encoding = "Latin-1") # Change this directory
participants(d, id = 'id', gender = 'sex', age = 'age', excl = 7, recruitedfrom = 'ClickWorker', collectedat = 'online', more = list('Native language' = c('language'), 'Occupation' = c('occupation')))