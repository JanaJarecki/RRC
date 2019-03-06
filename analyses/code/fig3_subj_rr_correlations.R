source("fig_setup.R")
library(data.table)
library(patchwork)

d <- fread("../../data/processed/study1.csv")

dlong <- melt(d, id = c('id', 'ret_subj'), c('risk_subj', 'var_subj', 'fluct_subj', 'pred_subj'))
dlong[, value_z := scale(value), by = id]
dlong[, ret_subj_z := scale(ret_subj), by = id]

dindices <- d[id == min(id), c("var_obj", "mroi_obj", "index"), with = F]
dindices[, letter := letters[1:.N]]

o <- ggplot(dindices, aes(var_obj, mroi_obj)) +
      geom_point(aes(shape = index), size = 1, fill ="grey", color = "grey33") +
      geom_smooth(method = "lm", se = F, color = "black", size = 1) +
      theme(legend.direction = 'vertical', legend.position = 'left', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_line(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9), aspect.ratio = 1, legend.key.height = unit(.5, "lines"), legend.text = element_text(size = 8, lineheight = 5, color = "grey33")) +
        scale_x_continuous('Objective Risk (variance)', expand = c(0.02,0), limits = c(0,0.3), breaks=seq(.05,.25,.05), labels = c('.05','.10','.15','.20','.25')) +
        scale_y_continuous('Objective Return', expand = c(0,0), limits = c(0,0.25), breaks=seq(.05,.20,.05), labels = c('.05','.10','.15','.20')) +
        scale_shape_manual(name = NULL, values = 1:nrow(dindices))+
        guides(size = "none") +
        ggtitle("A  Objective Relation")

s <- ggplot(dlong, aes(value_z, ret_subj_z)) +
      geom_smooth(aes(linetype = variable), method = "lm", se = F, color = "black", size = 1) +
      geom_text(aes(label = label), data.frame(
         label = c("Risk", "Variation", "Fluctuation", "Predictability"),
         value_z =    c(-2.30, 1.30, 3.05, -2.70),
         ret_subj_z = c( 0.36, 0.23, 0.16,  0.12)),
          size = 2.7,
          fontface = "italic") +
      theme(legend.position = 'none', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_line(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9), legend.key.width = unit(1.5, "lines"), aspect.ratio = 1) +
      scale_x_continuous('Perceived Values', expand = c(0.015,0), limits = c(-3.8,3.9), breaks = c(-3,-1.5,0,1.5,3)) +
      scale_y_continuous('Perceived Return', expand = c(0.15,0), breaks = seq(-.3,.3,.15)) +
      scale_linetype_manual("Perception of ...",values = c(6,4,1,3), labels = c("Risk", "Variation", "Fluctuation", "Predictability")) +
      guides(size = "none") +
      ggtitle("B  Perceived Relation") +
      labs(caption = "Perceived values z-transformed")
o + s



ggsave("../figures/fig3a.png", o + s, width = 7.4, height = 3.4)


