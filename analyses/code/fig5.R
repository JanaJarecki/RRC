source('fig_setup.R')
# source('2.2-risk-return-paradox.R')

# Make plots, set alpha
samplesizes <- d[, length(unique(id)), by = graph]$V1
plots <- lapply(1:length(corlist), function(z) plot_net(corlist[[z]], Title = gsub("Riskvar", "Risk (m. as var.)", gsub("_Graph_TRUE", "",  gsub("_Graph_FALSE", " ", paste0(letters[z], ". ", names(corlist)[z])))), n = samplesizes[z], mlvar = TRUE, alpha = alpha))
ngraph <- d[, length(unique(id)), by = graph]$V1 # Number of subjects

# Plot
png(paste0('../figures/fig5.png'), width = 1300, height = 1000, units = "px", res = 220, pointsize = 10)
plot.new()
layout(mat = matrix(c(1,2,3,7,7,7,4,5,6), 3, byr=T), heights = c(4, 1, 4))
par(family = "Roboto Condensed", oma = c(1, 0, 2.5, 0))
for (i in 1:3) {
  plot(plots[[i]], oma=c(7,25,7,25))
}
for (i in 4:6) {
    plot(plots[[i]], oma=c(0,25,10,25))
}
title(paste0("Graph (n=", ngraph[1], ")"), outer=TRUE, cex.main = 1.8, family = "Roboto Condensed", font = 2, adj = 0)
mtext(paste0("Without Graph (n=", ngraph[2], ")"), side = 3, line = -21, outer = TRUE, cex = 1.15, family = "Roboto Condensed", font = 2, adj = 0)
dev.off()