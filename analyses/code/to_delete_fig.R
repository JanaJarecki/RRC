source("fig_setup.R")
library(data.table)

d <- fread("../../data/processed/study1.csv")

dlong <- melt(d, id = c('id', 'var_obj'), c('risk_subj', 'var_subj', 'fluct_subj', 'pred_subj'))

dlong[, scaled := scale(value), by = .(variable, id)]
dlong[, .(r = round(cor(var_obj, scaled), 2)), by = variable]


dlong[, variable := factor(variable, labels = c('Risk', 'Variability', 'Fluctuation', 'Predictability'))]
dcor <- dlong[, .(rho = cor(var_obj, value, method = 'spearman')), by = variable]
dlong[, variable := factor(variable, levels = dcor[order(abs(rho)), variable])]

ggplot(dlong, aes(x = var_obj, y = value)) +
  geom_count(alpha = .1, color = 'grey45') +
  facet_wrap(~variable, nrow = 1, scale = 'free_y') +
  theme(aspect.ratio = 1) +
  geom_smooth(method = "lm", color = "black", se = F, fullrange = T) +
  geom_text(aes(x = .15, y = 8, label = paste0('rho==', round(rho,2))), dcor, parse = TRUE, family = 'Roboto Condensed', size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_blank(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9)) +
  scale_x_continuous('Objective Variance', expand = c(0.07,0), limits = c(0,0.3), breaks=seq(.05,.25,.05), labels = c('.05','.10','.15','.20','.25')) +
  scale_y_continuous('Subjective\nJudgment', expand = c(0.15,0), limits = c(1,8), breaks=1:7) +
  guides(size = "none")

ggsave("../figures/fig2.png")