source("fig_setup.R")
library(data.table)
library(patchwork)

d <- fread("../../data/processed/study2.csv")
setnames(d, 'risk_subj', 'value')
setnames(d, 'quest', 'variable')
setnames(d, 'graph', 'Graph')
dlong <- d
dlong[, value_z := scale(value), by = id]
dlong[, ret_subj_z := scale(ret_subj), by = id]
dindices <- d[id == min(id), c("var_obj", "mroi_obj", "index", "Graph"), with = F]
dindices[, letter := letters[1:.N]]
dindices[, mroi_obj := mroi_obj/100]
dindices[, var_obj := var_obj/100]

o <- ggplot(dindices, aes(var_obj, mroi_obj)) +
      geom_point(aes(shape = index), size = 1, fill ="grey", color = "grey33") +
      geom_smooth(method = "lm", se = F, color = "black", size = 1) +
      theme(legend.direction = 'vertical', legend.position = 'left', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_line(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9), aspect.ratio = 1, legend.key.height = unit(.5, "lines"), legend.text = element_text(size = 8, lineheight = 5, color = "grey33")) +
        scale_x_continuous('Objective Risk (variance)', expand = c(0.01,0), limits = c(0.10,0.7), breaks=seq(.15,.65,.1), labels = c('.15','.25','.35','.45', '.55', '.65')) +
        scale_y_continuous('Objective Return', expand = c(0,0), limits = c(-0.03,0.35), breaks=seq(.0,.30,.05), labels = c('0','.05','.10','.15','.20', '.25', '.30')) +
        scale_shape_manual(name = NULL, values = 1:nrow(dindices))+
        guides(size = "none") +
        ggtitle("A  Objective Relation")

dlong[, Graph  := factor(Graph, levels = c(TRUE, FALSE), ordered = TRUE)]
s <- ggplot(dlong, aes(value_z, ret_subj_z)) +
      geom_smooth(aes(linetype = variable), method = "lm", se = F, color = "black", size = 1) +
      geom_text(aes(label = label), data.frame(
        Graph = factor(rep(c(FALSE, TRUE), each = 3), levels = c(TRUE, FALSE)),
        label = rep(c("Risk", "Risk\n(variance)", "Fluctuation"), 2),
        value_z =    c(-2.35, -2, 0.10,    -2.20, -2.1, 2.1),
        ret_subj_z = c( 0.60, 0.19, 0.70,  0.60,  0.04, 0.61)),
        size = 2.7,
        fontface = "italic",
        lineheight = .8) +
      themejj(facet = TRUE) +
      theme(legend.position = 'none', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_line(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9), legend.key.width = unit(1.5, "lines"), aspect.ratio = 1, strip.text = element_text(face = 'bold')) +
      scale_x_continuous('Perceived Values', expand = c(0.015,0), limits = c(-3,3.3), breaks = seq(-2,2,1), labels = sprintf('%.2f', seq(-2,2,1))) +
      scale_y_continuous('Perceived Return', expand = c(0.05,0), breaks = seq(-.8,.7,.3), labels = sprintf('%.2f', seq(-.8,.7,.3))) +
      scale_linetype_manual("Perception of ...",values = c(1,4,2), labels = c("Fluctuation", "Risk", "Risk (variance)")) +
      guides(size = "none") +
      ggtitle("B  Perceived Relation") +
      facet_wrap(~Graph, nrow = 2, label = "label_both") +
      labs(caption = "Perceived values z-transformed")

ggsave("../figures/fig4.png", o + s, width = 7, height = 5.5)


