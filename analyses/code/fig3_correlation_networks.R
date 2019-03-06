library(data.table)
library(mlVAR) # Partial correlation networks in hierarchical designs
library(bootnet) # Partial correlation networks
source("fig_setup.R")

# Read the data
d <- fread("../../data/processed/study1.csv") # Change this directory

# ------------------------------------------------------------------------
#  Partial correlation network
vars <- paste0(c('risk', 'fluct', 'var', 'pred'), '_subj')
fits <- lapply(vars, function(i) mlVAR(d, vars = c('ret_subj', i, 'mroi_obj', 'var_obj'), idvar = "id", lags = 2))
make_labels <- function(x, sep = "\n") {
  x <- gsub("ret_subj", "Perceived_Return", x)
  x <- gsub("risk_subj", "Perceived_Risk", x)
  x <- gsub("var_subj", "Perceived_Variation", x)
  x <- gsub("fluct_subj", "Perceived_Fluctuation", x)
  x <- gsub("pred_subj", "Perceived_Predictability", x)
  x <- gsub("mroi_obj", "Objective_Return", x)
  x <- gsub("var_obj", "Objective_Risk (variance)", x)
  return(gsub("_", sep, x))
}

Colors <- c(
  d = "#49494b", l = "#efefef", m = "#737374",
  a1 = "#DE5B6D", a2 = "#ce486c", a3 = "#C39EA0", a4 = "#F8E5E5",
  b1 = "#478BA2", b2 = "blue", b3 = "darkblue", b4 = "#e3ecf9")
plot_net <- function(ml_var_fit, Title, colors = Colors) {
  p <- plot(ml_var_fit, type = "contemporaneous", layout = 'circle', 
    groups = list(1:2,3:4), nonsig = "hide", legend = FALSE, 
    details = FALSE, mar = c(5,5,10,5), order = c(4,1,3,2),
    border.width = 10, border.color = 'white',
    title = Title, title.cex = 1.8,
    loop = .7, node.width = 3.4, edge.width = 1.4, asize = 4, 
    label.fill.vertical = .7, label.fill.horizontal = 1,
    label.scale = FALSE, negDashed=TRUE, fade = FALSE,
    color = c('white', 'white', rep('white', 2))[c(4,1,3,2)],
    edge.color = colors[c('l','l','l','a1','l')],
    edge.label.color = colors[c('d','d','a1','d','d')],
    label.color = colors[c('black','black','m','m')][c(4,1,3,2)],
    edge.labels = TRUE, edge.label.cex = 3.4, edge.label.font = 2, edge.label.position = .4, alpha = .01)
    # Linebreaks in labels
    p$graphAttributes$Nodes$labels <- make_labels(p$graphAttributes$Nodes$labels)
    # More space around labels
    p$graphAttributes$Edges$labels <- gsub("-", "-", paste(" ", p$graphAttributes$Edges$labels, " "))
  return(p)
}
plots <- lapply(1:4, function(z) plot_net(fits[[z]], Title = sub("Perceived ", paste(LETTERS[z], " "), make_labels(vars[z], " "))))
# Erroneous red edge for Predictability graph
plots[[4]]$graphAttributes$Edges$color[3] <- plots[[3]]$graphAttributes$Edges$color[3]
plots[[1]]$graphAttributes$Edges$color[4] <- Colors['b1']
plots[[1]]$graphAttributes$Edges$label.color[4] <- Colors['b1']
# Change color
# dev.off()
png('../figures/fig3b.png', width = 1600, height = 500, units = "px", res = 220, , pointsize = 12)
par(mfrow = c(1, 4), family = "Roboto Condensed")
for (i in 1:4) {
  plot(plots[[i]])
}
dev.off(); dev.off()