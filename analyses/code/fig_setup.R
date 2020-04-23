options(warn = -1)
library(ggplot2)
theme_set(theme_minimal())
options(warn = 0) # Reset


cat('\n')
make_labels <- function(x, sep = "\n") {
  x <- gsub("rvar_subj|rv_", "Perceived_Risk-as-variance", x)
  x <- gsub("ret_subj_z|rt_|ret_subj|Return_z", "Perceived_Return", x)
  x <- gsub("risk_subj|rs_|Risk$", "Perceived_Risk", x)
  x <- gsub("fluct_subj|fl_|Fluctuation", "Perceived_Fluctuation", x)
  x <- gsub("pred_subj|pr_|Predictability", "Perceived_Predictability", x)
  x <- gsub("mroi_obj|mr_", "Objective_Return", x)
  x <- gsub("var_obj|vr_$|vr_b", "Objective_Risk (variance)", x)
  x <- gsub("var_subj|vr_$|vr_s|Variability", "Perceived_Variability", x)
  x <- gsub("gl_", "Mean Graph_Literacy", x)
  x <- gsub("attitude_mw_c", "Positive_Attitude", x)
  return(gsub("_", sep, x))
}
cat('defined fun make_labels(x, sep)\n')

Colors <- c(
  d = "#49494b", l = "#efefef", m = "#737374",
  a1 = "#DE5B6D", a2 = "#ce486c", a3 = "#C39EA0", a4 = "#F8E5E5",
  b1 = "#478BA2", b2 = "blue", b3 = "darkblue", b4 = "#e3ecf9")
cat('defined vec Colors[d, l, m, a1, ..., a4, b1, ... b4]\n')




plot_net <- function(fit, type = "contemporaneous", Title="", colors = Colors, n = NULL, mlvar, alpha, cex = 3.4) {
  if (!mlvar) {
    Groups <- c(grep('subj',colnames(fit),invert=TRUE),grep('subj',colnames(fit)))
    ngr <- length(Groups)
    p <- qgraph(fit,
      graph = "pcor",
      layout = "circle",
      #threshold = "holm",
      minimum = "sig",
      sampleSize = n,
      alpha = 0.1,
      groups = list(Groups),
      mar = c(5,5,10,5),
      border.width = 10,
      border.color = 'white',
      title = Title,
      title.cex = 1.8,
      loop = .7, node.width = 3.4, edge.width = 1.4, asize = 4, 
      label.fill.vertical = .7, label.fill.horizontal = 1,
      label.scale = FALSE, fade = FALSE,
      color = rep('white', ngr),
      label.color = c('black', colors['m'])[c(2,1,2,1)],
      negDashed=TRUE,
      negCol = colors['b2'],
      posCol = colors['l'],
      edge.labels = TRUE,
      edge.label.cex = cex,
      label.font = 1,
      edge.label.font = 1,
      label.scale.equal = TRUE
      )
    # p$graphAttributes$Edges$label.color <- colors[c('d','b2')][p$graphAttributes$Edges$lty]
    # p$graphAttributes$Edges$labels <- gsub('-', '  - ', p$graphAttributes$Edges$labels)
    # p$graphAttributes$Nodes$labels <- make_labels(p$graphAttributes$Nodes$labels)
  }

  if (mlvar) {
    Groups <- c(grep('subj', fit$fit$var,invert=TRUE),grep('subj', fit$fit$var))
    ngr <- length(Groups)
    p <- plot(fit,
      type = type,
      layout = 'circle', 
      groups = list(Groups),
      nonsig = "hide",
      alpha = alpha,
      #threshold = "holm",
      legend = FALSE, 
      details = FALSE,
      mar = c(5,5,10,5),
      border.width = 10,
      border.color = 'white',
      title = Title,
      title.cex = 1.8,
      loop = .7,
      node.width = 3.4,
      edge.width = 1.4,
      asize = 4, 
      label.fill.vertical = .7,
      label.fill.horizontal = 1,
      label.scale = FALSE,
      negDashed=TRUE,
      fade = FALSE,
      color = rep('white', ngr),
      negCol = colors['b2'],
      posCol = colors['l'],
      edge.label.color = colors['d'],
      label.color = c('black', colors['m'])[c(2,1,2,1)],
      edge.labels = TRUE,
      edge.label.cex = cex,
      edge.label.font = 2,
      edge.label.position = .54)
  }
  ew <- which.max(p$graphAttributes$Edges$width)
  eneg <- which(p$graphAttributes$Edges$lty==2)
  p$graphAttributes$Edges$edge.label.position[eneg] <- .5
  p$graphAttributes$Edges$edge.label.position[ew] <- .5

  p$graphAttributes$Edges$label.color <- colors[c('d','b2')][p$graphAttributes$Edges$lty]
  # More space around negative labels
  p$graphAttributes$Edges$labels <- paste0(gsub('-', '  - ', p$graphAttributes$Edges$labels), ' ')
  # Linebreaks in labels
  p$graphAttributes$Nodes$labels <- make_labels(p$graphAttributes$Nodes$labels)
  l <- p$graphAttributes$Nodes$labels
  p$graphAttributes$Nodes$label.color <- ifelse(grepl('Perceived', l), 'black', ifelse(grepl('Attitude', l), 'red', colors['m']))
  return(p)
}
cat('defined plot_net(fit, Title, n, mlvar, alpha)\n')



