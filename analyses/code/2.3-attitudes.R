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


# Read the data
d <- fread('../../data/processed/study2.csv', 
  colClasses = list(factor = c('id', 'index')))
# Compute attitude score
d[, attitude_aktiv_subj := 6-attitude_aktiv_subj] # TODO CHEK
d[, attitude_mw := rowMeans(cbind(attitude_gut_subj, attitude_interessant_subj, attitude_stark_subj, attitude_aktiv_subj))]
# Scale the subjective ratings (1-7 likert) and attitude (1-5 likert)
d[, perception_c := perception - 4]
d[, ret_subj_c := ret_subj - 4]
d[, attitude_mw_c := rowMeans(cbind(attitude_gut_subj, attitude_interessant_subj, attitude_stark_subj, attitude_aktiv_subj) - 3)]
# z-standardise or not?
z <- TRUE
vars <- c('var_obj', 'ret_subj', 'mroi_obj', 'perception')
if (z) {
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, as.numeric), .SDcols = vars]
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, function(z) c(scale(z))), .SDcols = vars, by = id]
}
d[, quest := factor(quest, levels=c('risk (variance)', 'fluctuation', 'risk'), labels = c('Risk (m. as var.)', 'Fluctuation', 'Risk'))]
d[, graph := factor(graph, levels=c(FALSE, TRUE), labels = c('Without Graph', 'Graph'))]



# -----------------------------------------------------------------------------
# Attitude score
# -----------------------------------------------------------------------------
# Cronbach's alpha
alpha_by_index <- d[, as.list(summary(psych::alpha(cbind(.SD)))), .SDcols = c('attitude_gut_subj', 'attitude_interessant_subj','attitude_stark_subj','attitude_aktiv_subj'), by = index]
summary(alpha_by_index$raw_alpha, digits = 2)
# Descriptive statistics
d[, as.list(round(describe(attitude_mw),2))][, c('median', 'mean', 'sd', 'min', 'max')]
d[, mean(attitude_mw), by = country][V1 %in% range(V1)]


# -----------------------------------------------------------------------------
# Linear mixed-effects model
# -----------------------------------------------------------------------------
contrasts(d$quest) <- contr.sum(3)
contrasts(d$graph) <- contr.sum(2)


# Partial correlation Network
# Generate a list with data subsets
vars <- c('id', 'var_obj', 'ret_subj_z', 'mroi_obj', 'attitude_mw_c', 'perception_z')
datasubsets <- list(
  Risk_Graph_TRUE = setnames(d[quest=='Risk' & graph == 'Graph', vars, with = FALSE], length(vars), 'risk_subj'),
  Riskvar_Graph_TRUE = setnames(d[quest=='Risk (m. as var.)' & graph == 'Graph', vars, with = FALSE], length(vars), 'rvar_subj'),
  Fluctuation_Graph_TRUE = setnames(d[quest=='Fluctuation' & graph == 'Graph', vars, with = FALSE], length(vars), 'fluct_subj'),
  Risk_Graph_FALSE= setnames(d[quest=='Risk' & graph != 'Graph', vars, with = FALSE], length(vars), 'risk_subj'),
  Riskvar_Graph_FALSE = setnames(d[quest=='Risk (m. as var.)' & graph != 'Graph', vars, with = FALSE], length(vars), 'rvar_subj'),
  Fluctuation_Graph_FALSE = setnames(d[quest=='Fluctuation' & graph != 'Graph', vars, with = FALSE], length(vars), 'fluct_subj'))

# Fit the partial correlation networks
corlist_att <- lapply(datasubsets, function(z) {
  # We need to round because mlVAR does not want the data otherwise
  z[, names(z)[2:6] := lapply(.SD, round, 6), .SDcols = 2:6]
  mlVAR(z, vars = names(z)[2:6], idvar = 'id', lags = 1, temporal = "fixed")
  })


tab <- list()
for (i in 1:6) {
  rnms <- make_labels(corlist_att[[i]]$fit$var, ' ')
  tmp <- cbind(rbind(corlist[[i]]$fit, attitude_mw_c=NA)[, -1], corlist_att[[i]]$fit[c(1,2,3,5,4), -1])
  row.names(tmp) <- rnms
  tab[[i]] <- tmp
}
names(tab) <- c('Graph + Risk', 'Risk (m. as var.)', 'Fluctuation', 'Without Graph + Risk', 'Risk (m. as var.)', 'Fluctuation')

source('tab_setup.R')

papaja::apa_table(tab
  , caption = 'Study 2: Goodness of fit of partial correlation networks.'
  , col.names = c('', toupper(colnames(tab[[1]])))
  , row.names = TRUE
  , col_spanners = list('Base\nNetwork' = c(2,3),
                        'Base + Attitudes\nNetwork' = c(4,5))
  , merge_method = 'table_spanner')




source('fig8.R')




# v_318 v_319 v_320 v_321
# Wonach haben wir Sie in dieser Studie gefragt?
# 318 Schwankung der Renditeverläufe von Aktienindizes
# 319 Risiko von Aktienindizes
# 319 Risiko (gemessen an der Varianz) von Aktienindizes
# 320 Andere Fragen



# -----------------------------------------------------------------------------
# Checks
# the first line is the model with the main effects
# fit_id_old <- lmer(ret_subj~ quest*graph + perceived_z + (quest*graph):perceived_z + (1 + perceived_z | id), d)
# Test:
# Compute the marginal effects by hand without graph
# sum(fixef(fit_id)[c(
#    'questrisk (variance):perceived_z'
#   ,'graph1:perceived_z'
#   ,'quest1:graph1:perceived_z')])
# sum(fixef(fit_id)[c(
#    'questfluctuation:perceived_z'
#   ,'graph1:perceived_z'
#   ,'quest2:graph1:perceived_z')])

# -----------------------------------------------------------------------------
# Mediation Model
# -----------------------------------------------------------------------------
#         att
#      /      \
#     a        b
#   /            \
# Per -- c, c' -- Ret
#
# Paths (M=mediatior, X=predictor, Y=outcome)
# a: X - M, riskPerception -> attitudes
# b: M - Y, attitudes -> returnPerception
# c: X - Y, riscPrception -> returnPerception (direct effect) 
# c': X - M - Y riskPercept. -> attitudes -> returnPercept. (indirect effect)
# 
# X - Y (c' path)
c_model <- lmer(ret_subj~ quest*graph + quest:graph:perceived_z + (1 + perceived_z | id), d)
c_model2 <- lmer(ret_subj~ quest*graph + mroi_obj + var_obj + quest:graph:perceived_z + (1 + perceived_z | id), d)
emtrends(c_model2, ~quest | graph, var = 'perceived_z')

# X - M - Y (c path)
cpr_model <- lmer(ret_subj ~ quest*graph*attitude_mw_c + (quest:graph:perceived_z) * attitude_mw_c + (1 + perceived_z | id), d)
# X - M (a path)
a_model <- lmer(attitude_mw_c ~ quest*graph + quest:graph:perceived_z + (1 + perceived_z | id), data = d)

# Check if adding attitude improves fit
anova(cpr_model, c_model, refit = F) # Yes

# Describe the slopes / trends
emtrends(cpr_model, ~ quest | graph, var='perceived_z')
test(emtrends(c_model, ~quest | graph, var = 'perceived_z'))
test(emtrends(a_model,  ~quest | graph, var = 'perceived_z'))
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
control.treat.values <- d[, as.list(round(quantile(risk_subj, c(.25,.75)), 2)), by = .(quest, graph)][order(-graph,quest)]
mediatelist <- lapply(1:nrow(covarlevels), function(i) mediate(
  model.m = a_model,
  model.y = c_model,
  treat = 'perceived_z',
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

tlo <- tablist[c(3,1,2,6,4,5)]
# Experimental code to plot a path diagram
plot_diagram <- function(i, digits = 3, tablist = tlo, colid = 'b2') {
  par(mar=c(0,0,1,0))
  v <- paste0(names(tablist)[i], ':perceived_z')
  lab <- gsub('quest|:.*', '', v)
  fmt <- paste0("%.", digits, "f")
  ab <- tablist[[i]]['Mediation Effect', 1] # indirect effect, aX - bM - Y 
  c <- tablist[[i]]['Direct Effect', 1] # direct effect: cX - Y
  total <- tablist[[3]]['Total Effect', 1] # c + (a * b)
  a <- fixef(a_model)[v] # path a: aX - M
  b <- ab / a
  cpri <- fixef(cpr_model)[v]
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
  plot <- plotmat(Ma, pos = c(1,2), 
                  name = c("Attitude", paste0("Perceived\n", lab), "Perceived\nReturn"), 
                  box.type = "rect", box.size = 0.15, box.prop=0.48, curve=0, txt.font = 1, box.lcol = 'gray', absent = "NA", latex = T, box.lwd = .5, shadow.size = 0, dtext = -0.96, prefix = 'b = ', cex.txt = .8, arr.col = 'grey', arr.width = 0, arr.length = 0, arr.lcol = Co, arr.lwd = 1.3)
  title(main = paste0(letters[i], '. ', toupper(substr(lab,1,1)), substr(lab, 2, nchar(lab))), adj = 0, font.main = 1, cex.main = 1.2)
  return(plot)
  }

# Plot
png(paste0('../figures/fig7.png'), width = 1300, height = 650, units = "px", res = 200, pointsize = 11.5)
plot.new()
layout(mat = matrix(c(1,2,3,7,7,7,4,5,6), 3, byr=T), heights = c(4, 1, 4))
par(family = "Roboto Condensed", oma = c(1, 0, 2.5, 0))
par(mar=c(0,0,3,0))
for (i in 1:3) {
  plot_diagram(i, 3)
}
for (i in 4:6) {
    plot_diagram(i, 3)
}
title("Graph", outer=TRUE, cex.main = 1.8, family = "Roboto Condensed", font = 2, adj = 0)
mtext("Without Graph", side = 3, line = -12.5, outer = TRUE, cex = 1.15, family = "Roboto Condensed", font = 2, adj = 0)
dev.off()




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
ggplot(d, aes(y=ret_subj, x=perceived_z, color = attitude_binf)) +stat_smooth(method='lm', se = F, aes(linetype = attitude_binf), size = .8) +facet_grid(factor(graph, levels = c('TRUE', 'FALSE'), labels = c('Graph', 'No Graph')) ~ questf) +scale_color_viridis_d(end = .9, option = 'A') +scale_linetype_manual(values = c(1,3,6)) +guides(linetype = guide_legend('Attitude Score (binned)'), color = guide_legend('Attitude Score (binned)')) +labs(caption = "Attitude score centered\nPerceived risk/fluctuation z-standardized") +xlab('Perceived risk or fluctuation') +ylab('Perceived return') +themejj(facet=TRUE) +theme(aspect.ratio = 1, legend.position = 'right', legend.direction = 'vertical')
M <- d[, .(ret_subj = mean(ret_subj), perceived_z = median(perceived_z)), by = .(quest, graph, attitude_binf)]
M
ggplot(d, aes(x = perceived_z, y = ret_subj)) +geom_jitter(aes(color = attitude_binf), alpha = .05) +facet_grid(factor(graph, levels = c('TRUE', 'FALSE'), labels = c('Graph', 'No Graph')) ~ questf+attitude_binf) +geom_violin(alpha=0) +scale_color_viridis_d(end = .9, option = 'A') +geom_point(data = M, aes(color = attitude_binf))


















# library(lavaan)
# # ret_subj ~ quest*graph*attitude_mw_c + (quest:graph:perceived_z) * attitude_mw_c + (1 + perceived_z | id), d
# d[, a_p := attitude_mw_c * perceived_z]
# d[, q1 := contrasts(quest)[quest, 1]]
# d[, q2 := contrasts(quest)[quest, 2]]
# d[, q3 := ifelse(q1==-1|q2==-1, 1, 0)]
# d[, g := contrasts(graph)[graph]]
# d[, g0 := as.numeric(g==1)]
# d[, g1 := as.numeric(g==-1)]
# d[, q1_g := q1 * g]
# d[, q2_g := q2 * g]
# d[, q1_a := q1 * attitude_mw_c]
# d[, q2_a := q2 * attitude_mw_c]
# d[, g_a := g * attitude_mw_c]
# d[, q1_g_a := q1_g * attitude_mw_c]
# d[, q2_g_a := q2_g * attitude_mw_c]
# d[, q1_g0_p := q1 * g0 * perceived_z ]
# d[, q2_g0_p := q2 * g0 * perceived_z]
# d[, q3_g0_p := q3 * g0 * perceived_z]
# d[, q1_g1_p := q1 * g1 * perceived_z]
# d[, q2_g1_p := q2 * g1 * perceived_z]
# d[, q3_g1_p := q3 * g1 * perceived_z]
# d[, q1_g0_p_a := q1 * g0 * perceived_z * attitude_mw_c ]
# d[, q2_g0_p_a := q2 * g0 * perceived_z * attitude_mw_c ]
# d[, q3_g0_p_a := q3 * g0 * perceived_z * attitude_mw_c ]
# d[, q1_g1_p_a := q1 * g1 * perceived_z * attitude_mw_c ]
# d[, q2_g1_p_a := q2 * g1 * perceived_z * attitude_mw_c ]
# d[, q3_g1_p_a := q3 * g1 * perceived_z * attitude_mw_c ]

# model_lm <- ret_subj ~ quest * graph * attitude_mw_c + (quest:graph:perceived_z) * attitude_mw_c + (1 | id)
# model <- '
#   # FIXED EFFECTS
#   level: 2
#     ret_subj ~ q1 + q2 + g + attitude_mw_c + q1_g + q2_g + q1_a + q2_a + g_a + q1_g_a + q2_g_a + q1_g0_p + q2_g0_p + q3_g0_p + q1_g1_p + q2_g1_p + q3_g1_p + q1_g0_p_a + q2_g0_p_a + q3_g0_p_a + q1_g1_p_a + q2_g1_p_a + q3_g1_p_a
#     # RANDOM EFFECTS
#     level: 1
#       ret_subj ~~ ret_subj
#     '

# fit_lm <- lmer(model_lm, d)
# fit <- sem(model, d, cluster = 'id')
# cbind(fixef(fit_lm)[-1], coef(fit)[1:length(fixef(fit_lm))])

# library(lavaan)

# model_lm <- ret_subj ~ quest + (1 | id)
# fit_lm <- lmer(model_lm, d, REML = FALSE)

# model <- '# Random effects
#           level: 1
#             ret_subj ~~ ret_subj
#           # Fixed effects
#           level: 2
#             ret_subj ~ q1 + q2          
#           '
# fit <- sem(model, d, cluster = 'id')
# summary(fit)
# summary(fit_lm)
# cbind(fixef(fit_lm)[-1], coef(fit)[c(-c(1:2))])

# # Mediation test
# y.mod <- lm(ret_subj ~ quest * attitude_mw_c, d)
# m.mod <- lm(attitude_mw_c ~ quest, d)
# m1 <- mediate(model.y = y.mod, model.m = m.mod, treat = 'quest', mediator = 'attitude_mw_c')

# sem.fml <- '# random effects

#               # direct
#                 ret_subj ~ c * perceived_z
#               # mediation
#                 attitude_mw_c ~ a * perceived_z
#                 ret_subj ~ b * attitude_mw_c               
#             # indirect effect
#               ab := a * b
#             # totel effect
#               total := c + (a * b)
#             '
# covarlevels
# sems <- lapply()
# sem.mod <- sem(sem.fml, d[graph==TRUE & quest=='risk'])

# summary(sem.mod)
# summary(m1)
# cbind(coef(y.mod)[-1], coef(sem.mod))

# library(semPlot)
# x = c(1, 0, -1)
# y = c(0, -1, 0)
# ly = matrix(c(x, y), ncol=2)
# source('fig_setup.R')
# i <- 'Risk'

# plot_sem <- function(i) {
#   semPaths(sems[[i]], 'model', 'est',
#   layout = ly,
#   nCharNodes = 20,
#   sizeMan = 20,
#   sizeMan2 = 10,
#   nodeLabels = gsub('x', i, c('Perceived\nReturn', 'Attitude', 'Perceived\nx')),
#   equalizeManifests = F,
#   label.cex = 1,
#   edge.label.cex = 1.1,
#   label.scale = F,
#   label.font = 2,
#   edge.label.font = 2,
#   negDashed = TRUE,
#   negCol = Colors['b2'],
#   posCol = Colors['l'],
#   mar = c(9,6,9,7))
#   title(names(sems)[[i]],line=3, adj=0)
# }

# sem.mod@ret_subjArgument



# library(OpenMx)

# ## openmx
# lm <- ret_subj ~ attitude_mw_c
# # * perceived_z + (1 + perceived_z | id)
# lmfit <- lm(lm, d[quest=='risk' & graph==TRUE])
# lmnames <- names(coef(lmfit))
# # dataset

# Y <- 'ret_subj'
# X <- c('perceived_z')
# reX <- c('perceived_z')
# M <- c('attitude_mw_c')
# manifests <- c(Y, X, M)
# latents <- c()
# nX <- length(X)
# nreX <- length(reX)
# co <- combn(nX, 2)
# # ivs <- c('q1', 'q2', 'g', 'attitude_mw_c', 'q1_g', 'q2_g', 'q1_a', 'q2_a', 'g_a', 'q1_g_a', 'q2_g_a', 'q1_g0_p', 'q2_g0_p', 'q3_g0_p', 'q1_g1_p', 'q2_g1_p', 'q3_g1_p', 'q1_g0_p_a', 'q2_g0_p_a', 'q3_g0_p_a', 'q1_g1_p_a', 'q2_g1_p_a', 'q3_g1_p_a') 

# m2 <- umxRAM("m1",
#   data = as.data.frame(d[quest=='risk' & graph == TRUE]),
#   umxPath(X, to = c(M, Y)),
#   umxPath(M, to = Y),
#   # each of the manifests and latents needs a mean and variance
#   umxPath(v.m. = c(X, Y))
#   )
# plot(m2, showFixed = T)

# m1 <- umxRAM2('
#             ret_subj ~ c * perceived_z
#             affect_mw_c ~ a * perceived_z
#             ret_subj ~ b * affect_mw_c
#          ',
#          data = as.data.frame(d[quest=='risk' & graph == TRUE])
#          )
# plot(m1, showFixed = TRUE)


# umxSummary(m1, report = 'markdown')
# coef(m1)

# lmfit <- lm(ret_subj ~ perceived_z, d[quest == 'risk' & graph == TRUE])
# coef(lmfit)
# logLik(m1)
# logLik(lmfit)


# sem <- mxModel('sem', 
#   type = 'RAM',
#   manifestVars = manifests,
#   latentVars = latents,
#   # X -> M and X -> Y
#   mxPath(X, to = c(M, Y),
#     value = c(1.0,1.0), arrows=1, free = TRUE, label=c("X__M","X__Y") ),
#   # Mediation:
#   # M -> Y
#   mxPath(from = M,to = Y, value = c(1.0) , arrows=1, label=c("M__Y") ),
#   # Variance 
#   mxPath(from = X, to = X, free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_X") ),
#   mxPath(from = M, to = M, free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_M") ),
#   mxPath(from = Y, to = Y, free=c(TRUE), value=c(1.0) , arrows=2, label=c("VAR_Y") ),
#   # Intercepts
#   mxPath(from = 'one', to = c(X, M, Y), free = TRUE, value=0, arrows=1),
#   mxData(d[quest == 'risk' & graph == TRUE], type = 'raw')
# );

# semfit <- mxTryHard(sm)



# # âttitude ~ perceived_z
# medA <- mxModel(model = 'medA', type = 'RAM',
#               # B/c SEM can handle latent and manifest vars
#               manifestVars = c('attitude_mw_c', 'perceived_z'),
#               # Data: mxData()
#               mxData(d[quest=='risk' & graph==TRUE], type = 'raw'),
#               # variance paths only from = ...
#               mxPath(from = c('attitude_mw_c', 'perceived_z'),
#                 a = 2, v = c(1,1), la = c('var_medA', 'var_medP')),
#               # Covariance paths from = ... to = ...
#               #mxPath(from = 'attitude_mw_c', to = 'perceived_z',
#               #  a = 2, v = 1, la = c('cov_medA')),
#               # regression paths from IV to DV
#               mxPath(from = 'perceived_z', to = 'attitude_mw_c',
#                 v = 1, la = c('mediation')),
#               # means and intercept paths from one to IV/DV
#               mxPath(from = 'one', to = c('attitude_mw_c', 'perceived_z'),
#                 v = c(1,1), la = c('med intercept', 'med M'))
#               )

# # âttitude ~ perceived_z
# medR <- mxModel(model = 'medR', type = 'RAM',
#               # B/c SEM can handle latent and manifest vars
#               manifestVars = c('attitude_mw_c', 'ret_subj'),
#               # Data
#               mxData(d[quest=='risk' & graph==TRUE], type = 'raw'),
#               # variance paths
#               mxPath(from = c('attitude_mw_c', 'ret_subj'), a = 2, v = c(1,1), la = c('var_medA', 'var_medP')),
#               # Covariance paths
#               #mxPath(from = 'attitude_mw_c', to = 'ret_subj', a = 2, v = 1, la = c('cov_medA')),
#               # regression paths
#               mxPath(from = 'attitude_mw_c', to = 'ret_subj', v = 1, la = c('mediation')),
#               # means and intercept paths
#               mxPath(from = 'one', to = c('attitude_mw_c', 'ret_subj'), v = c(1,1), la = c('medRintercept', 'medRM'))
#               )

# sm <- mxModel(model = 'sem', type = 'RAM',
#               # Variable type
#               manifestVars = c(dv,ivs),
#               # Data mxDAta() type raw
#               mxData(d[quest=='risk' & graph==TRUE], type = 'raw'),   
#               # Variances
#               mxPath(from = c(dv,ivs),
#                 a = 2, v = rep(1, nivs+1), la = paste0('var_', c(dv,ivs)) ),
#               # Covariances
#                #mxPath(from = ivs[co[1,]], to = ivs[co[2,]],
#                # a = 2,  v=0.5, la=paste0('cov_', ivs[co[1,]], ivs[co[2,]])),
#                # Regression
#                mxPath(from = ivs, to = dv,
#                 v = 1, la=gsub(':', 'X', lmnames[-1])),
#                # Intercepts
#                mxPath(from = 'one', to = c(dv,ivs), 
#                 v = rep(1, length(ivs)), la = c('(Intercept)', paste('M_', ivs))),
#                # Add the other paths to it
#                medA,
#                medR,
#                # byid,
#                # refePath
#                # Connect the random-effects to the fixed-effects, NOT free
#                # mxPath(from = paste0('byid.', c('slope','intercept')), to=dv,
#                #  a=1, free = FALSE, v = c(1,NA), la=c(NA, "data.perceived_z"), joinKey = 'id')
#                # Connect the mediator to the predictor
#                mxPath(from = 'medA.affect_mw_c', to = 'affect_mw_c'),
#                 a = 1, free = FALSE, v = 1, la = 'estimated')
#                )

# # random effects structure
# byid <- mxModel(model =  'baid', type = 'RAM',
#                 # Variable declaration (new vars)
#                 latentVars = c('slope', 'intercept'),
#                 # Data
#                 mxData(data.frame(id=unique(d[quest=='risk' & graph==TRUE]$id)), type = 'raw', primaryKey='id'),
#                 # Random effects work through the variance:
#                 # Variance
#                 mxPath(from = c('intercept', 'slope'),
#                   a = 2, v = 1),
#                 # Covariance
#                 mxPath(from = 'intercept', to = 'slope',
#                   a=2, v=.25, labels="cov1"))




# semfit <- mxTryHard(sm)
# # semfit <- mxRun(sm)
# # mxCheckIdentification(semfit)

# # round(coef(semfit), 5)
# # fixef(lmfit)
# round(cbind(fixef(lmfit), coef(semfit)[gsub(':', 'X', names(fixef(lmfit)))]), 5)
# omxGetParameters(sm)





# ## Test setup of the regression vs openmx
# ## openmx
# lm <- ret_subj ~ quest + (1 + perceived_z | id)
# lmfit <- lmer(lm, d)
# # dataset

# library(OpenMx)
# dv <- 'ret_subj'
# ivs <- c('q1', 'q2')
# nivs <- length(ivs)
# # Random effects
# reivs <- c('perceived_z')
# nreivs <- length(reivs)
# # variance paths
# varPaths <- mxPath(from=c(dv,ivs), arrows=2, free=TRUE, values=rep(1, nivs+1), labels=paste0('var_', c(dv,ivs)) )
# # covariance of predictors
# co <- combn(nivs, 2)
# covPaths <- mxPath(from=ivs[co[1,]], to=ivs[co[2,]], arrows=2, 
#                         free=TRUE, values=0.5, labels=paste0('cov_', ivs[co[1,]], ivs[co[2,]]))
# # regression weight
# regPaths  <- mxPath(from=ivs, to=dv, arrows=1,  free=TRUE, values=1, labels=gsub(':', 'X', names(fixef(lmfit))[-1]) )
# # means and intercepts
# means <- mxPath(from="one", to=c(dv,ivs), arrows=1,  free=TRUE, values=rep(1, length(ivs)), labels=c('(Intercept)', paste('M_', ivs)) )
# # random effects structure
# byid <- mxModel(model="byid", type="RAM",
#   latentVars=c('slope', 'intercept'),
#   mxData(data.frame(id=unique(d$id)), type = 'raw', primaryKey='id'),
#   mxPath(c('intercept', 'slope'), arrows=2, values=1),
#   mxPath('intercept', 'slope', arrows=2, values=.25, labels="cov1"))

# refePath <- mxPath(from=paste0('byid.', c('slope','intercept')), to=dv, arrows=1, free=FALSE, values=c(1,NA), labels=c(NA, "data.perceived_z"), joinKey='id')

# sm <- mxModel(model='sem', type="RAM", mxData(observed = d, type='raw'), manifestVars=c(dv,setdiff(ivs,reivs)), latentVars = reivs, varPaths, covPaths, regPaths, means, byid, refePath)

# semfit <- mxRun(sm)

# round(coef(semfit), 5)
# fixef(lmfit)
# round(cbind(fixef(lmfit), coef(semfit)[gsub(':', 'X', names(fixef(lmfit)))]), 5)








# ## SO **
#     # use iris
#     # Change contrasts to effects-coding
#     contrasts(iris$Species) <- contr.sum(3)
#     # Linear regression
#     lmmodel <- Sepal.Length ~ Species
#     lmfit <- lm(lmmodel, iris)
#     # SEM
#     iris$s1 <- contrasts(iris$Species)[iris$Species, 1] # Numeric and effects-coed
#     iris$s2 <- contrasts(iris$Species)[iris$Species, 2] #     - " -
#     semmodel <- 'Sepal.Length ~ s1 + s2'
#     semfit <- sem(semmodel, iris)
#     # Compare the coefficients
#     cbind(coef(lmfit)[-1], coef(semfit)[-length(coef(semfit))])


#     # With interaction
#     lmmodel <- Sepal.Length ~ Species + Species:Sepal.Width
#     lmfit <- lm(lmmodel, iris)
#     # SEM
#     iris$s3 <- ifelse(iris$Species=='virginica', 1, 0) # Code third species
#     iris$s1_w <- iris$s1 * iris$Sepal.Width # Numeric interaction
#     iris$s2_w <- iris$s2 * iris$Sepal.Width #      - " -
#     iris$s3_w <- iris$s3 * iris$Sepal.Width #      - " -"
#     semmodel <- 'Sepal.Length ~ s1 + s2 + b1*s1_w + b2*s2_w + b3*s3_w'
#     semfit <- sem(semmodel, iris)
#     # Compare the coefficients
#     cbind(coef(lmfit)[-1], coef(semfit)[-length(coef(semfit))])

#     #                                     [,1]       [,2]
#     # Species1                      -0.7228562 -0.7228566
#     # Species2                       0.1778772  0.1778772
#     # Speciessetosa:Sepal.Width      0.6904897  0.6904899
#     # Speciesversicolor:Sepal.Width  0.8650777  0.8650779
#     # Speciesvirginica:Sepal.Width   0.9015345  2.4571023
