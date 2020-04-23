# Make table for supplementary material
source('tab_setup.R')

# Run this line if you have not fit the network before!
# source('1.2-risk-return-paraxos.R') # First fit the networks

# Alpha level
alpha <- 0.0125

# get the partial correlations and p-values from the table
tablist <- lapply(corlist_att, get_input_for_table)
names(tablist) <- rep(c("Risk", "Risk-as-variance", "Fluctuation"), 2)

for (i in list(c(1,4), c(2,5), c(3,6))) {
  tablist[[i[1]]] <- do.call(cbind, tablist[i])
} 
tablist[4:6] <- NULL
# Latex Table
cat(apa_table(tablist
  , caption = "Study 2 results: partial correlations and p-values from the correlation networks.\n\\label{study1_pcc}"
  , align = "l"
  , landscape = TRUE,
  , font_size = "small",
  , placement = 'H'
  , note = paste("Significance levels: $^{*} = .05, ^{**} = .01, ^{***} = .001$")
  , escape = F
  , merge_method = 'indent'
  , col_spanners = list(`With graph` = c(1, 4), `Without graph` = c(5, 8))
  ))

# '../tables/stab2.tex'

