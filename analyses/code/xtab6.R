# Make table for supplementary material
source('tab_setup.R')
source('2.2-risk-return-paradox.R') # First, fit partial correlation networks

# Alpha level
alpha <- 0.0083

# get the partial correlations and p-values from the table
tablist <- lapply(corlist, get_input_for_table)

# Latex table with indents (therefore we use kable)
nr <- nrow(tablist[[1]])
ll <- seq(1, nr*length(tablist), nr)
ul <- ll + nr - 1
opts_current$set(label = "study2_pcc")
vars <- c("var_obj", "ret_subj", "mroi_obj", "risk_subj")
tab <- do.call(rbind, tablist)[, 1:3]

tablist[["Fluctuation_Graph_FALSE"]][4,2]
tablist[["Risk_Graph_FALSE"]][4,2]
tablist[["Riskvar_Graph_FALSE"]][4,2]


kable(tab, "latex", caption = "Study 2 results: partial correlations and p-values from the correlation networks.", align = c('r','c','c','c', 'c'), booktabs = TRUE, escape = FALSE) %>%
  pack_rows(make_labels(vars, " ")[1], ll[1], ul[1]) %>%
  pack_rows(make_labels(vars, " ")[2], ll[2], ul[2]) %>%
  pack_rows(make_labels(vars, " ")[3], ll[3], ul[3]) %>%
  pack_rows(make_labels(vars, " ")[1], ll[4], ul[4]) %>%
  pack_rows(make_labels(vars, " ")[2], ll[5], ul[5]) %>%
  pack_rows(make_labels(vars, " ")[3], ll[6], ul[6]) %>%
  add_footnote(paste0("\\emph{Note}: Boldface indicates significant partial correlations at $\\alpha=", alpha, '$'),
    notation="none",
    threeparttable = TRUE,
    escape = FALSE)


# Table the stocks used
tt <- fread('../../experiment/study2_material/stocks_used.csv')
print(xtable(tt[, 2:4], label = "tab:study2_material", caption = "Stocks used in study~2"), caption.placement = "top", include.rownames = FALSE, tabular.environment = "tabularx")


# Table the partial correlations for the robustness checks
# TODO