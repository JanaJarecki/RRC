# ==========================================================================
# Analysis of Study 2 including the influence of attutudes
# ==========================================================================
library(data.table)
library(lme4)
library(psych)
library(mediation)
library(xtable)
library(kableExtra)
library(car)
library(emmeans)
library(papaja)
library(mlVAR)
source('utils.R')
if ('lmerTest' %in% .packages()) {
  detach("package:lmerTest", unload=TRUE) # otherwise mediation() won't work
}


# 
# Read and preprocess the data
# -----------------------------------------------------------------------------
d <- fread('../../data/processed/study2.csv')
# Format the 'condition' variables as factors
d[, c("id", "index") := .(factor(id), factor(index))]
d[, quest := factor(quest, levels=c('risk (variance)', 'fluctuation', 'risk'), labels = c('Risk-as-variance', 'Fluctuation', 'Risk'))]
d[, graph := factor(graph, levels=c(FALSE, TRUE), labels = c('No Graph', 'Graph'))]
# Compute attitude score from the five-item attitude scale
d[, attitude_aktiv_subj := 6-attitude_aktiv_subj] # TODO: check
d[, attitude_mw := rowMeans(cbind(attitude_gut_subj, attitude_interessant_subj, attitude_stark_subj, attitude_aktiv_subj))]
# Center the subjective ratings (1-7 likert) and attitude (1-5 likert)
d[, perception_c := perception - 4]
d[, ret_subj_c := ret_subj - 4]
d[, attitude_mw_c := rowMeans(cbind(attitude_gut_subj, attitude_interessant_subj, attitude_stark_subj, attitude_aktiv_subj) - 3)]
# Do or don't z-standardise values
z <- TRUE
vars <- c('var_obj', 'ret_subj', 'mroi_obj', 'perception')
if (z) {
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, as.numeric), .SDcols = vars]
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, function(z) c(scale(z))), .SDcols = vars, by = id]
}



#
# Mediation Analysis
# -----------------------------------------------------------------------------
# Checks
# the first line is the model with the main effects
# fit_id_old <- lmer(ret_subj~ quest*graph + perception_z + (quest*graph):perception_z + (1 + perception_z | id), d)
# Test:
# Compute the marginal effects by hand without graph
# sum(fixef(fit_id)[c(
#    'questrisk (variance):perception_z'
#   ,'graph1:perception_z'
#   ,'quest1:graph1:perception_z')])
# sum(fixef(fit_id)[c(
#    'questfluctuation:perception_z'
#   ,'graph1:perception_z'
#   ,'quest2:graph1:perception_z')])


#         att
#      /      \
#     a        b
#   /            \
# Per --   c  -- Ret
#
# Per --   c'  -- Ret
#
# Paths (M=mediatior, X=predictor, Y=outcome)
# a: X - M, riskPerception -> attitudes
# b: M - Y, attitudes -> returnPerception
# c: X - Y, riscPrception -> returnPerception (direct effect) 
# c': X - M - Y, riskPercept. -> attitudes -> returnPercept. (indirect effect)

# X - Y (c path)
c_model <- lmer(ret_subj ~ quest*graph + quest:graph:perception_z + (1 + perception_z | id), d)
# X - M - Y (c' path)
cpr_model <- lmer(ret_subj ~ quest*graph*attitude_mw_c + (quest:graph:perception_z) * attitude_mw_c + (1 + perception_z | id), d)
# Path a (X - M)
a_model <- lmer(attitude_mw_c ~ quest*graph + quest:graph:perception_z + (1 + perception_z | id), data = d)

# Check: does adding attitude as predictor improve the fit at all?
anova(cpr_model, c_model, refit = F) # Yes

# Describe the slopes / trends
emtrends(cpr_model, ~ quest | graph, var = 'perception_z')
test(emtrends(c_model, ~quest | graph, var = 'perception_z'))
test(emtrends(a_model,  ~quest | graph, var = 'perception_z'))
cpr_slopes <- test(emtrends(cpr_model, ~quest | graph, var='attitude_mw_c'))
apa_table(cpr_slopes[,-2],
  digits = 3,
  al = c('l',  'c', 'c', 'r','r','r'),
  col.n = c('Question', 'Slope', 'SE', 'df', '\\textit{z}-ratio', '\\textit{p}'),
  p = 'H',
  caption = 'Estimated slopes from the linear-mixed effects modelwith dependent variable perceived return')


# Apply simulation-based mediation analysis for all covaraite combinations (conditions)
n_sims <- 1000
covarlevels <- d[, unique(.SD), .SDcols = c('quest', 'graph')][order(-graph,quest)]
control.treat.values <- d[, as.list(round(quantile(perception_z, c(.25,.75)), 2)), by = .(quest, graph)][order(-graph,quest)]
mediatelist <- lapply(1:nrow(covarlevels), function(i) mediation::mediate(
  model.m = a_model,
  model.y = cpr_model,
  treat = 'perception_z',
  mediator = 'attitude_mw_c',
  sims = n_sims,
  covariates = list(quest=covarlevels$quest[i], graph=covarlevels$graph[i]),
  control.value = 0,
  treat.value = 1
  )
)
names(mediatelist) <- control.treat.values[, paste0(quest, graph)]

## Make the results table
tablist <- lapply(mediatelist, function(x) data.frame(matrix(unlist(x[c('d0', 'd0.ci', 'd0.p','z0', 'z0.ci', 'z0.p','tau.coef', 'tau.ci', 'tau.p')]),
  ncol = 4,
  byrow = TRUE,
  dimnames = list(c('Mediation Effect', 'Direct Effect', 'Total Effect'), c('Coefficient','5%CI','95%CI','p')))))
names(tablist) <- covarlevels[, paste0('quest', quest, ':graph', graph), by = .(quest, graph)]$V1

# ACME = beta_2 * gamma = a * b
# ADE = beta_3 = c'

tab <- rbindlist(tablist, id='ID')
tab[, Graph := rep(c(F,T), each = 9)]
tab[, Effect := rep(c('Mediation', 'Direct', 'Total'), 6)]
tab[, Effect := factor(Effect, levels = c('Mediation', 'Direct', 'Total'))]
tab[, Question := rep(c('risk-as-variance', 'fluctuation', 'risk'), each=3, 2)]
tab[, `95%CI` := paste0('[', sprintf('%.2f',X5.CI), ', ', sprintf('%.2f',X95.CI), ']'), by = .(Graph,Effect)]

tab2 <- dcast(tab, Graph + Question ~ Effect, value.var=c('Coefficient', '95%CI', 'p'))
setcolorder(tab2, c(1,2,3,6,9,4,7,10,5,8,11))
tab2
library(magrittr)
library(kableExtra)
gsub('( |\\[)(-)', '\\U\\1$-$', kable(tab2[c(4:6,1:3),-1],
    format = 'latex',
    digits = 3,
    caption = 'Mediation analysis, dependent variable: perceived risk; mediator: attitude score',
    label = 'tab:study2_mediation',
    booktabs = TRUE,
    align=c('l', rep(c('r','c','c'), 3)),
    escape = F,
    col.names = c(' ', 'b', '95\\% CI', '$p$', 'b', '95\\% CI', '$p$', 'b', '95\\% CI', '$p$')
    ) %>%
  kable_styling(
    font_size = 9.5) %>%
  add_header_above(c(' ' = 1, 'Mediaton Effect' = 3, 'Direct Effect' = 3, 'Total Effect' = 3)) %>%
  group_rows('With Graph', 1, 3) %>%
  group_rows('Without', 4, 6) %>%
  footnote(general = 'Effects shown are those at the 50% quantile of the other variables.'), perl = TRUE)

source('fig_setup.R')
library(viridis)
ggplot(tab, aes(x = factor(Question, labels = c('Fluctuation', 'Risk', 'Risk\n(measured as variance)')), y = Coefficient, shape = Effect)) +
  geom_hline(aes(yintercept = 0), linetype = 2, size = .3)  +
  geom_pointrange(aes(ymin = X5.CI, ymax = X95.CI, fill = ..y..), position = position_dodge(width = .45), size = .5, stroke = .3) +
  facet_wrap(~ifelse(grepl(TRUE, ID), 'Graph', 'No Graph')) +
  scale_shape_manual(values  = c(Mediation = 24, Direct = 23, Total = 22), labels = c(expression(Mediation[IV%->%A%->%R]), expression(Direct[IV%->%R]), expression(Total[Mediation+Direct]))) +
  ylim(-.6,.55) +
  xlab('Condition') +
  ggtitle('Mediation of the perceived risk-return relationship through attitudes') +theme(panel.spacing.x = unit(5, 'mm'), legend.background = element_rect(color = 'black'), legend.margin = margin(1, 1, 1, 1, unit = 'mm'), legend.key = element_rect(color=NA)) +
  scale_fill_viridis(begin = 0, end = .92) +
  guides(fill = 'none', shape = guide_legend(override.aes = list(color = '#238B8E', stroke = .5)))
ggsave('../figures/fig6.png', w = 14, h = 6, unit = 'cm', dpi = 600)


d[, attitude_mw_by_id := median(attitude_mw_c), by = id]
d[, attitude_bin := cut(attitude_mw_by_id, 3)]
d[, attitude_binf := factor(attitude_bin, labels = paste(c('Bad', 'Neutral', 'Good'), levels(attitude_bin)))]
d[, questf := factor(quest, levels = c('risk', 'risk (variance)', 'fluctuation'), labels = c('Risk', 'Risk\n(measured as variance)', 'Fluctuation'))]
ggplot(d, aes(y=ret_subj, x=perception_z, color = attitude_binf)) +stat_smooth(method='lm', se = F, aes(linetype = attitude_binf), size = .8) +facet_grid(factor(graph, levels = c('TRUE', 'FALSE'), labels = c('Graph', 'No Graph')) ~ questf) +scale_color_viridis_d(end = .9, option = 'A') +scale_linetype_manual(values = c(1,3,6)) +guides(linetype = guide_legend('Attitude Score (binned)'), color = guide_legend('Attitude Score (binned)')) +labs(caption = "Attitude score centered\nPerceived risk/fluctuation z-standardized") +xlab('Perceived risk or fluctuation') +ylab('Perceived return') +themejj(facet=TRUE) +theme(aspect.ratio = 1, legend.position = 'right', legend.direction = 'vertical')
M <- d[, .(ret_subj = mean(ret_subj), perception_z = median(perception_z)), by = .(quest, graph, attitude_binf)]
M
ggplot(d, aes(x = perception_z, y = ret_subj)) +geom_jitter(aes(color = attitude_binf), alpha = .05) +facet_grid(factor(graph, levels = c('TRUE', 'FALSE'), labels = c('Graph', 'No Graph')) ~ questf+attitude_binf) +geom_violin(alpha=0) +scale_color_viridis_d(end = .9, option = 'A') +geom_point(data = M, aes(color = attitude_binf))