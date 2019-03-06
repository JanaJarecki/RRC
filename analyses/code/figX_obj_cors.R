library(GGally)
source("fig_setup.R")
library(data.table)
library(patchwork)

d <- fread("../../data/processed/study1.csv")
du <- d[id == min(id)]

cd <- d[, c('var_obj', 'risk_subj', 'var_subj', 'pred_subj', 'fluct_subj', 'ret_subj', 'mroi_obj'), with = F]

ggpairs(cd) +theme(aspect.ratio = 1)


library(corrplot)

corrplot(cor(cd), method = "pie", type = "upper", family = "Roboto Condensed", tl.col = "black") 