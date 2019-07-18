library(data.table)
library(cogsciutils)
library(papaja)
library(lmerTest)
library(emmeans)
library(mlVAR)
library(stargazer)
library(xtable)
# -----------------------------------------------------------------------------
# BEFORE GETTING STARTED
# -----------------------------------------------------------------------------
# Change your working directory to this file location


# -----------------------------------------------------------------------------
# Load data and some settings
# -----------------------------------------------------------------------------
# z-standardise or not?
z <- TRUE

# Read the data
d <- fread('../../data/processed/study2.csv') # Change this directory
if (z) {
  v <- c('risk_subj', 'ret_subj')
  d[, c(v) := lapply(.SD, as.numeric), .SDcols = v]
  d[, c(v) := lapply(.SD, function(z) c(scale(z))), .SDcols = v, by = .(quest, id)]
}
d[, id := factor(id)]
d[, graph := factor(graph)]


# -----------------------------------------------------------------------------
# Perceived risk and objective variance
# Correlations between objective variance and subjective risk
# -----------------------------------------------------------------------------
m <- ifelse(z, 'pearson', 'spearman')
d[, .(cor = round(cor(risk_subj, var_obj, method = m), 2)), by = .(graph, quest)][order(-cor)]
# Print for manuscsript
apa_table(
  d[, apa_print(cor.test(risk_subj, var_obj, method = m))[c(1,2)], by = .(graph, quest)][order(-estimate)],
  caption = 'Correlations between perceived risk and objective risk (variance)',
  note = 'Pearson correlations from pooled data, subjective values z-standardized at individual level.',
  col.names = c('Graph', 'Question wording', 'Correlation (95% CI)', 'Statistic'),
  escape = FALSE)

# -----------------------------------------------------------------------------
# Controling for clusters, generalized linear mixed models
# -----------------------------------------------------------------------------
# Sum-to-zero contrasts:  the last level of the factor is dropped
d[, quest := factor(quest, levels = c('risk (variance)', 'fluctuation', 'risk'))] # Make 'risk' the baseline quesstion, drop risk
d[, graph := factor(graph, levels = c(TRUE, FALSE))] # drop graph = FALSE
contrasts(d$graph) = contr.sum(2) # contrast coding for graph (TRUE/FALSE)
contrasts(d$quest) = contr.sum(3) 
# Fit model
hlmfit <- lmer(var_obj ~ 0 + (graph*quest):risk_subj + (1 | id), data = d)
trnd_graph <- emtrends(hlmfit, ~ graph, var = 'risk_subj')
test(trnd_graph); pairs(trnd_graph)
trnd_quest_graph <- emtrends(hlmfit, ~quest | graph, var = 'risk_subj')
test(trnd_quest_graph)
source("tab_setup.R")
xt <- xtable(test(trnd_quest_graph)
  ,caption = "Hierarchical linear model results -- trend analysis; dependent variable: objective risk (variance of the index funds)"
  ,label="tab:study2_hlm_trends",
  digits=3)
names(xt[[1]]) <- names(xt[[2]]) <- c('Question', 'Trend', 'SE', 'df', 'z-ratio', '$p$-value')
print(xt,
  sanitize.colnames.function = function(x) x)

# Pairwise comparisons
pairs(trnd_quest_graph)
print(xtable(pairs(trnd_quest_graph)
  ,digits=3,
  ,caption = "Pairwise comparisons between trend coefficients, dependent variable: objective risk (variance of the index funds)"
  ,label="tab:study2_hlm_trends_pairwise"))


# -----------------------------------------------------------------------------
# The risk-return paradox
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Real-world correlation between risk/variance and return
# -----------------------------------------------------------------------------
apa_print(d[!duplicated(index), cor.test(var_obj, mroi_obj)])

# -----------------------------------------------------------------------------
# Correlations (pooled) between subjective risk and subjective return
# -----------------------------------------------------------------------------
d[, .(cor = round(cor(risk_subj, ret_subj, method = m), 3)), by = .(graph, quest)][order(-graph,cor),]
xt <- xtable(d[, apa_print(cor.test(risk_subj, ret_subj, method = m))[c(1,2)], by = .(graph, quest)][order(-graph)],
  digits = 2,
  caption = 'Correlation between perceived risk and objective risk (variance)',
  booktabs = TRUE,
  label = 'tab:study2_rr',
  align = 'lllrr')
names(xt) <- c('Graph', 'Question wording', '$r$ [95\\% CI]', 'Statistic')
print(xt,
  hline.after = c(0,3),
  add.to.row = list(pos = list(6), command = c("\\bottomrule\n\\multicolumn{4}{l}{{\\textit{Note}. Pearson correlations $r$ from pooled data, subjective values z-standardized at individual level.}}\n")),
  sanitize.text.function = function(x) { gsub('r = |95|CI|%','', x) },
  sanitize.colnames.function = function(x) x)