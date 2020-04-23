detach(package:lmerTest) # otherwise mediation() won't work
library(data.table)
library(lme4)
library(psych)
library(mediation)
library(xtable)
library(kableExtra)
library(car)
library(emmeans)
source('utils.R')


# Read the data
d <- fread('../../data/processed/study2.csv', 
  colClasses = list(factor = c('id', 'index')))
# Scale the subjective ratings (1-7 likert) and affect (1-5 likert)
d[, risk_subj_c := risk_subj - 4]
d[, ret_subj_c := ret_subj - 4]
d[, affect_mw_c := rowMeans(cbind(affect_gut_subj, affect_interessant_subj, affect_stark_subj, affect_aktiv_subj) - 3)]
# z-standardise or not?
z <- TRUE
vars <- c('var_obj', 'ret_subj', 'mroi_obj', 'risk_subj')
if (z) {
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, as.numeric), .SDcols = vars]
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, function(z) c(scale(z))), .SDcols = vars, by = id]
}



# -----------------------------------------------------------------------------
# Familiarity
# -----------------------------------------------------------------------------
# How well do you know the index? (1=not at all, 3=medium, 7=very well)
d[, as.list(round(describe(knowledge_index_subj),2))][, c('median', 'mean', 'sd', 'min', 'max')]
d[, mean(knowledge_index_subj), by = .(index, country)][V1 %in% range(V1)]


# -----------------------------------------------------------------------------
# Linear mixed-effects model
# -----------------------------------------------------------------------------
setnames(d, 'risk_subj_z', 'perceived_z')
d[, quest := factor(quest, levels=c('risk (variance)', 'fluctuation', 'risk'))]
d[, graph := factor(graph, levels=c(F, T))]
contrasts(d$quest) <- contr.sum(3)
contrasts(d$graph) <- contr.sum(2)

detach("package:lmerTest", unload=TRUE)
# the first line is the model with the main effects
# fit_id_old <- lmer(ret_subj~ quest*graph + perceived_z + (quest*graph):perceived_z + (1 + perceived_z | id), d)
fit_id <- lmer(ret_subj~ quest*graph + quest:graph:perceived_z + (1 + perceived_z | id), d)
# Test:
# Compute the marginal effects by hand without graph
sum(fixef(fit_id)[c(
   'questrisk (variance):perceived_z'
  ,'graph1:perceived_z'
  ,'quest1:graph1:perceived_z')])
sum(fixef(fit_id)[c(
   'questfluctuation:perceived_z'
  ,'graph1:perceived_z'
  ,'quest2:graph1:perceived_z')])


## Two more models for the mediation analysis
# Model with attitudes
fit_id_attitudes <- lmer(ret_subj ~ quest*graph*affect_mw_c + (quest:graph:perceived_z) * affect_mw_c + (1 + perceived_z | id), d)
emtrends(fit_id_attitudes, ~quest | graph, var = 'perceived_z')
slopes <- test(emtrends(fit_id_attitudes, ~quest | graph, var='affect_mw_c'))
apa_table(slopes[,-2],
  digits = 3,
  al = c('l',  'c', 'c', 'r','r','r'),
  col.n = c('Question', 'Slope', 'SE', 'df', '\\textit{z}-ratio', '\\textit{p}'),
  p = 'H',
  caption = 'Estimated slopes from the linear-mixed effects modelwith dependent variable perceived return')

# Check if adding affect improves fit
anova(fit_id, fit_id_attitudes, refit = F) # Yes
# Fit the mediation model
fit_mediator <- lmer(affect_mw_c ~ quest*graph + quest:graph:perceived_z + (1 + perceived_z | id), data = d)

# Levels of the graph and question variables
covarlevels <- d[, unique(.SD), .SDcols = c('quest', 'graph')][order(-graph,quest)]
control.treat.values <- d[, as.list(round(quantile(risk_subj, c(.25,.75)), 2)), by = .(quest, graph)][order(-graph,quest)]
# apply the mediatio nmodel, mediate(), for all combinations of covariates
# 1:nrow(covarlevels)
mediatelist <- lapply(1:nrow(covarlevels), function(i) mediate(
  model.m = fit_mediator,
  model.y = fit_id_attitudes,
  treat = 'perceived_z',
  mediator = 'affect_mw_c',
  # group.out = 'id',
  sims = 1000,
  covariates = list(quest=covarlevels$quest[i], graph=covarlevels$graph[i]),
  control.value = 0,
  treat.value = 1
  # control.value = control.treat.values[i, 3],
  # treat.value = control.treat.values[i, 4]
  )
)
names(mediatelist) <- control.treat.values[, paste0(quest, graph)]

tablist <- lapply(mediatelist, function(x) data.frame(matrix(unlist(x[c('d0', 'd0.ci', 'd0.p','z0', 'z0.ci', 'z0.p','tau.coef', 'tau.ci', 'tau.p')]),
  ncol = 4,
  byrow = TRUE,
  dimnames = list(c('Mediation Effect', 'Direct Effect', 'Total Effect'), c('Coefficient','5%CI','95%CI','p')))))

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