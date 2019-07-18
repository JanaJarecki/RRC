# Make table for supplementary material
source('tab_setup.R')

# Load data and fit networks
source('2.3-partialcorrelations.R') # First fit the networks

# get the partial correlations and p-values from the table
tablist <- lapply(corlist, get_input_for_table)

vars <- gsub("Riskvar", "Risk (variance)", gsub("_Graph_TRUE", " - Graph: TRUE",  gsub("_Graph_FALSE", " - Graph: FALSE ", names(corlist))))

nr <- nrow(tablist[[1]])
ll <- seq(1, nr*length(tablist), nr)
ul <- ll + nr - 1
opts_current$set(label = "study2_pcc")
tab <- kable(do.call(rbind, tablist)[, 1:3], "latex", caption = "Study 2 results: partial correlations and p-values from the correlation networks.", align = c('r','c','c','c'), booktabs = TRUE, escape = F) %>%
  pack_rows(make_labels(vars, " ")[1], ll[1], ul[1]) %>%
  pack_rows(make_labels(vars, " ")[2], ll[2], ul[2]) %>%
  pack_rows(make_labels(vars, " ")[3], ll[3], ul[3]) %>%
  pack_rows(make_labels(vars, " ")[4], ll[4], ul[4]) %>%
  pack_rows(make_labels(vars, " ")[5], ll[5], ul[5]) %>%
  pack_rows(make_labels(vars, " ")[5], ll[6], ul[6]) %>%
  add_footnote(paste0("\\emph{Note}: Boldface indicates significant partial correlations at $\\alpha=", alpha, '$'), notation="none", threeparttable = TRUE, escape = F)

write(tab, "C:/Users/Jana Jarecki//Google Drive/RRC/stab2.tex")