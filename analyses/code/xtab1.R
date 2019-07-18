# Table the stocks used
tt <- fread('../../experiment/study2_material/stocks_used.csv')
print(xtable(tt[, 2:4], label = "tab:study2_material", caption = "Stocks used in study~2"), caption.placement = "top", include.rownames = FALSE, tabular.environment = "tabularx")