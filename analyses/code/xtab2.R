# Make table for supplementary material
source('tab_setup.R')

# Run this line if you have not fit the network before!
# source('1.2-risk-return-paraxos.R') # First fit the networks

# Alpha level
alpha <- 0.0125

# get the partial correlations and p-values from the table
tablist <- lapply(corlist, get_input_for_table)
tablist <- lapply(tablist, function(i) {
  i[diag(1,4,4)==1] <- '---'
  i <- i[, -4]
})
names(tablist) <- paste('\\cmidrule{2-4} \\textbf{', make_labels(vars, " "), '}')

# Latex Table
apa_table(tablist
  , caption = "Study 1 results: partial correlations and p-values from the correlation networks.\n\\label{study1_pcc}"
  , align = c('r','c','c','c','c')
  , placement = 'H'
  , note = paste('Boldface $=$ significant partial correlations at $\\alpha=', alpha, '$')
  , escape = F
  , merge_method = 'indent'
  )

# '../tables/stab2.tex'

