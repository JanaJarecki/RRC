library(cogsciutils); library(data.table)
d <- fread("../../data/processed/study2_demographics.csv", key = 'id') # Change this directory
d[, sex := gsub("w", "f", sex)]
participants(d, id = 'id', gender = 'sex', age = 'age', excl = 27, recruitedfrom = 'ClickWorker', collectedat = 'online', more = list('Native language was' = c('language')))

# Subsample sizes
dcond <- unique(fread("../../data/processed/study2.csv", select = c('id', 'quest', 'graph', 'finlit_calc'), key = 'id'))
d <- d[dcond]
library(xtable)
options(xtable.include.rownames = FALSE)
xtable(d[, .(.N, Age=mean(age), Agesd = sd(age), Women=scales::percent(mean(sex=='f'),2), Finlit = mean(finlit_calc)), by = .(graph, quest)][order(graph)])