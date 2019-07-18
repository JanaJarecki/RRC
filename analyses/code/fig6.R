source('fig_setup.R')
# source('2.4-affect.R')

ngraph <- d[, length(unique(id)), by = graph]$V1 # Number of subjects

# Plot
png(paste0('../figures/fig', fn, '.png'), width = 1200, height = 600, units = "px", res = 110, pointsize = 14)
  plot.new()
  layout(mat = matrix(c(1,2,3,7,7,7,4,5,6), 3, byr = TRUE), heights = c(7, 2, 7))
  par(family = "Roboto Condensed", oma = c(0, .5, 2, 0), mar = c(2,2.5,2,2))
  for (i in 1:6) {
    plot(mediatelist[[i]]
      #,labels = c('AvMed\nEffect','AvDirect\nEffect','Total\nEffect')
      ,main = paste0(letters[i], '. ', sub('f', 'F', sub('r', 'R', covarlevels[i, quest]))),
      ,font.main = 1
      ,adj = 0,
      ,cex.main = 1.5
      ,cex = 1.8
      ,family = "Roboto Condensed"
      )
  }
  title(paste0("Graph: TRUE (n=", ngraph[1], ")"), outer=TRUE, cex.main = 1.8, family = "Roboto Condensed", font = 2, adj = 0)
  mtext(paste0("Graph: FALSE (n=", ngraph[2], ")"), side = 3, line = -19, outer = TRUE, cex = 1.15, family = "Roboto Condensed", font = 2, adj = 0)
dev.off()