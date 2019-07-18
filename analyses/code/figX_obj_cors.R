library(GGally)
source("fig_setup.R")
library(data.table)
library(patchwork)

d <- fread("../../data/processed/study1.csv")
du <- d[id == min(id)]

cd <- d[, c('mroi_obj','var_obj', 'risk_subj', 'var_subj', 'pred_subj', 'fluct_subj', 'ret_subj'), with = F]

ggpairs(cd) +theme(aspect.ratio = 1)


library(corrplot)
corrplot(cor(cd), method = "pie", type = "lower", family = "Roboto Condensed", tl.col = "black")
corrplot(cor(cd), method = "num", type = "upper", family = "Roboto Condensed", tl.col = "black", add = TRUE, diag = FALSE, col = 'black', yaxt='n', ann=FALSE)



