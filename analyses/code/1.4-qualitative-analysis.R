library(data.table)
library(scales) # to use percent format
library(xtable)
library(papaja)

d <- fread("../../data/processed/study1_qualitative-coding.csv",
  select = 1:13,
  col.names = c('id', unlist(fread("../../data/processed/study1_qualitative-coding.csv", select = 16)[1:12])))
d <- d[id %in% unique(fread("../../data/processed/study1.csv")$id)] #exclude
dm <- melt(d, 1, variable.factor = FALSE)


# Give of open-ended statements
txtd <- fread("../../data/processed/study1_demographics.csv", select = c('id', 'definition_risk'), encoding = 'UTF-8')
txtd <- txtd[uniquecat, on = 'id']
txtd[Loss>0, definition_risk]
txtd[Variance>0, definition_risk]
txtd[Other>0, definition_risk]

# How many people fall into multiple categories?
id_multi_cat <- dm[, .N, by = id][N>1]
nrow(id_multi_cat[N==2]) #3
nrow(id_multi_cat[N==3]) #6

# Correct the counts for multi-categories
dm <- dm[value==1]
dm[, value_corrected := value / .N, by = id] # falling into multiple categories
dm[, Group := unlist(strsplit(variable, "\\:"))[1], by = variable]
dm[, Group := gsub(', unpredictability|, fluctuation', '', Group), by = variable]
dm[, Group := factor(Group, levels = c("Uncertainty", "Variance", "Loss", "Gain", "Danger", "Other"))]

# Make the table
N <- nrow(d)
tab <- dm[, .(N = sum(value), Relative = sum(value_corrected)/N), by = .(variable,Group)]
tab[, Group_N := sum(N), by = Group]
tab[, Group_Relative := sum(Relative), by = Group]
tab[, Relative := scales::percent(Relative)]
tab[, Group_Relative := scales::percent(Group_Relative)]
# clean
tab[duplicated(Group), c("Group", "Group_N", "Group_Relative") := .("", NA, NA)]
setcolorder(tab, c(1,3,4,2,5,6))
source("tab_setup.R")
print(xtable(tab), only.contents = TRUE)


# Compute the risk-return paradox conditional on mentioning "Loss"
choiced <- fread("../../data/processed/study1.csv")
uniquecat <- dcast(dm, id ~ Group)
dd <- choiced[uniquecat, on = 'id']
dd[, .(.N, apa_print(cor.test(risk_subj, ret_subj))), by = Loss>0]


