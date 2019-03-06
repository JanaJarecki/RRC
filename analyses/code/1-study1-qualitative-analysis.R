library(data.table)

d <- fread("../../data/processed/study1_qualitative-coding.csv")
nn <- unlist(d[1:12, 'name_EN']) # extract names
d <- d[, 1:13]
setnames(d, 2:13, nn)

# Excluded participants
exclude <- c(59, 126, 176, 180, 187, 77, 79, 165)
d <- d[!lfdn %in% exclude]
dm <- melt(d, 1, variable.factor = FALSE)

# correct for multi-categories
dm <- dm[value==1]
dm[, value_corrected := value / .N, by = lfdn]
dm[, Group := unlist(strsplit(variable, "\\:"))[1], by = variable]
dm[, Group := factor(Group, levels = c("Uncertainty, unpredictability", "Variance, fluctuation", "Loss", "Gain", "Danger", "Other"))]


# People falling into two categories
lfdn_multi_cat <- dm[, .N, by = lfdn][N>1]
nrow(lfdn_multi_cat[N==2]) #3
nrow(lfdn_multi_cat[N==3]) #6


# Table
N <- nrow(d)
tab <- dm[, .(N = sum(value), Relative = sum(value)/N), by = .(variable,Group)]
tab[, Group_N := sum(N), by = Group]
tab[, Group_Relative := sum(Relative), by = Group]

library(scales)
tab[, Relative := percent(Relative)]
tab[, Group_Relative := percent(Group_Relative)]

# clean
tab[duplicated(Group), c("Group", "Group_N", "Group_Relative") := .("", NA, NA)]
setcolorder(tab, c(1,3,4,2,5,6))

library(xtable)
print(xtable(tab), include.rownames = FALSE)

prop.test(45.33, 96/2)