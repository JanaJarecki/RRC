# source("2.3-attitudes.R")
source("fig_setup.R")
library(diagram)

tlo <- corlist_att[c(3,1,2,6,4,5)]
# Experimental code to plot a path diagram
plot_diagram <- function(i, digits = 3, tablist = tlo, colid = 'b2') {
  par(mar=c(0,0,1,0))
  v <- paste0(names(tablist)[i], ':perception_z')
  lab <- gsub('quest|:.*', '', v)
  lab <- sub(" \\(m. as var.\\)", "-as-variance", lab)
  fmt <- paste0("%.", digits, "f")
  ab <- tablist[[i]]['Mediation Effect', 1] # indirect effect, aX - bM - Y 
  c <- tablist[[i]]['Direct Effect', 1] # direct effect: cX - Y
  total <- tablist[[3]]['Total Effect', 1] # c + (a * b)
  a <- nlme::fixef(a_model)[v] # path a: aX - M
  b <- ab / a
  cpri <- lme4::fixef(cpr_model)[v]
  ps <- c(sjstats::p_value(a_model)[which(names(fixef(a_model))==v), 2], # a
    tablist[[i]][, 'p'], # ab, c, c+ab
    sjstats::p_value(cpr_model)[which(names(fixef(cpr_model))==v), 2] # c'
    )
  ss <-  ifelse(ps >= .05, '', ifelse(ps >= .01, '*', '**'))
  ps <- paste0(', p ', ifelse(ps < .001, '', ' = '), papaja:::printp(ps))
  bs <- c(a, b, c, cpri)
  co <- ifelse(bs>0, 'grey', Colors[colid])
  bs <- sprintf(fmt, bs)
  bscs <- paste0(bs, ss)
  bscs <- gsub('-', '- ', bscs)
  names(bscs) <- names(co) <- c('a', 'b', 'c', 'cpri')
  Ma <- matrix(NA, 3, 3)
  Ma[1,2] <- bscs['a']
  Ma[1,3] <- bscs['b']
  Ma[2,3] <- paste0(bscs['c'], "\n     (", bscs['cpri'], ')')
  # Ma <- matrix(data, 3, 3, byrow = TRUE)
  Co <- Ma
  Co[1,2] <- co['a']
  Co[1,3] <- co['b']
  Co[2,3] <- co['c']
  # attitude, perception, return: c(x-pos, y-pos) in %
  pos <- cbind (c(0.425, 0.1, 0.75), c(0.8, 0.4, 0.4))
  plot <- plotmat(Ma,
    pos = pos, #c(1,2), 
    # Labels of the boxes
    name = c("Attitude", paste0("Perceived\n", lab), "Perceived\nReturn"), 
    # global scaling for size of graph
    #relsize = 0.8,
    # box layout
    box.type = "rect",
    box.size = 0.15,
    box.lcol = 'gray',
    box.prop=0.42, # length to width ratio of the box
    box.lwd = .3,
    shadow.size = 0,
    mx = 0.1, # horizontal shift of box
    # line layout
    curve=0,
    arr.col = 'grey',
    arr.width = 0,
    arr.length = 0,
    arr.lcol = Co,
    arr.lwd = 1.3,
    # text layout
    cex = 0.9, # relative font size
    txt.font = 1,
    absent = "NA",
    latex = T,
    dtext = -0.96, # position of arrow text relative to arrowhead
    prefix = 'b = ',
    cex.txt = .8)
  title(main = paste0(letters[i], '. ', toupper(substr(lab,1,1)), substr(lab, 2, nchar(lab))), adj = 0, font.main = 1, cex.main = 1.2)
  return(plot)
  }

# Plot
png(paste0('../figures/fig7.png'), width = 1400, height = 600, units = "px", res = 200, pointsize = 12.5)
plot.new()
layout(mat = matrix(c(1,2,3,7,7,7,4,5,6), 3, byr=T), heights = c(4, 0.5, 4))
par(family = "Roboto Condensed Light", oma = c(0.5, 0, 1.5, 0))
par(mar=c(0,0,3,0))
for (i in 1:3) {
  plot_diagram(i, 3)
}
for (i in 4:6) {
    plot_diagram(i, 3)
}
title("Graph", outer=TRUE, cex.main = 1.8, family = "Roboto Condensed", font = 2, adj = 0)
mtext("No Graph", side = 3, line = -12, outer = TRUE, cex = 1.15, family = "Roboto Condensed", font = 2, adj = 0)
dev.off()











 v <- paste0(names(tablist)[i], ':perception_z')
  lab <- gsub('quest|:.*', '', v)
  lab <- sub(" \\(m. as var.\\)", "-as-variance", lab)
  fmt <- paste0("%.", digits, "f")
  ab <- tablist[[i]]['Mediation Effect', 1] # indirect effect, aX - bM - Y 
  c <- tablist[[i]]['Direct Effect', 1] # direct effect: cX - Y
  total <- tablist[[3]]['Total Effect', 1] # c + (a * b)
  a <- nlme::fixef(a_model)[v] # path a: aX - M
  b <- ab / a
  cpri <- lme4::fixef(cpr_model)[v]
  ps <- c(parameters::p_value(a_model)[which(names(fixef(a_model))==v), 2], # a
    tablist[[i]][, 'p'], # ab, c, c+ab
    parameters::p_value(cpr_model)[which(names(fixef(cpr_model))==v), 2] # c'
    )
  ss <-  ifelse(ps >= .05, '', ifelse(ps >= .01, '*', '**'))
  ps <- paste0(', p ', ifelse(ps < .001, '', ' = '), papaja:::printp(ps))
  bs <- c(a, b, c, cpri)



  co <- ifelse(bs>0, 'grey', Colors[colid])
  bs <- sprintf(fmt, bs)
  bscs <- paste0(bs, ss)
  bscs <- gsub('-', '- ', bscs)
  names(bscs) <- names(co) <- c('a', 'b', 'c', 'cpri')
  Ma <- matrix(NA, 3, 3)
  Ma[1,2] <- bscs['a']
  Ma[1,3] <- bscs['b']
  Ma[2,3] <- paste0(bscs['c'], "\n     (", bscs['cpri'], ')')
  # Ma <- matrix(data, 3, 3, byrow = TRUE)
  Co <- Ma
  Co[1,2] <- co['a']
  Co[1,3] <- co['b']
  Co[2,3] <- co['c']
  # attitude, perception, return: c(x-pos, y-pos) in %
  pos <- cbind (c(0.425, 0.1, 0.75), c(0.8, 0.4, 0.4))

library(tidygraph)
library(ggraph)

gr <- create_complete(3) %>%
  activate(nodes) %>%
  mutate(xx = c(paste("Perceived\n", lab), "Attitude", "Perceived\nReturn")) %>%
  activate(edges) %>%
  mutate(yy = 1:3)

ll <- create_layout(gr, "manual", x = c(1, 3, 5), y = c(1,2,1))

ggraph(ll) + 
  geom_edge_link(aes(label = yy)) + 
  geom_node_tile(width = 0.55, height = 0.15, fill = "white", color = NA, size = 2) +
  geom_node_tile(width = 0.5, height = 0.1, fill = "white") +
  geom_node_text(aes(label = xx))


