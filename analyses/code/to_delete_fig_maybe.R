source("fig_setup.R")
library(data.table)

d <- fread("../../data/processed/study2.csv")
d[, var_obj := var_obj/100]
d[, quest := tools::toTitleCase(quest)]

dcor <- d[, .(rho = cor(var_obj, risk_subj, method = 'spearman')), by = .(graph,quest)]

d[, quest := factor(quest, levels = dcor[graph==TRUE][order(abs(rho)), quest], ordered = TRUE)]
dcor[, quest := factor(quest, levels = levels(d$quest))]

ggplot(d, aes(x = var_obj, y = risk_subj)) +
  geom_count(alpha = .1, color = 'grey45') +
  facet_grid(graph~quest, scale = 'free_y', labeller = labeller(graph = label_both, quest = label_value), as.table = F) +
  theme(aspect.ratio = 1) +
  geom_smooth(method = "lm", color = "black", se = F, fullrange = T) +
  geom_text(aes(x = .35, y = 8, label = paste0('rho==', gsub("0", "", sprintf("%.2f", rho)))), dcor, parse = TRUE, family = 'Roboto Condensed', size = 3) +
  theme(legend.direction = 'vertical', legend.position = 'right', panel.spacing.x = unit(1.5, 'lines'), axis.line = element_blank(), axis.ticks = element_blank(), axis.title.y = element_text(lineheight = .9)) +
  scale_x_continuous('Objective Variance', expand = c(0.04,0), limits = c(0.05,0.7), breaks=seq(.15,.65,.1), labels = gsub("^0", "", sprintf("%.2f", seq(.15,.65,.1)))) +
  scale_y_continuous('Subjective\nJudgment', expand = c(0.15,0), limits = c(1,8), breaks=1:7) +
  guides(size = "none")

ggsave("../figures/fig3.png")