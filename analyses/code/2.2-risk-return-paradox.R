library(data.table)
library(mlVAR) # Estimate partial correlation networks (hierarchical)
library(xtable)
library(papaja)
library(lme4)
library(emmeans)
library(lmerTest)

# Read the data
d <- fread('../../data/processed/study2.csv', colClasses = list(factor = c('quest', 'graph', 'id')))
d[, quest := factor(quest, levels=c('risk (variance)', 'fluctuation', 'risk'), labels = c('Risk (m. as var.)', 'Fluctuation', 'Risk'))]
d[, graph := factor(graph, levels=c(FALSE, TRUE), labels = c('Withoug Graph', 'Graph'))]

# z-standardise or not?
z <- TRUE
vars <- c('var_obj', 'ret_subj', 'mroi_obj', 'perception')
if (z) {
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, as.numeric), .SDcols = vars]
  d[, paste(c(vars), 'z', sep = '_') := lapply(.SD, function(z) c(scale(z))), .SDcols = vars, by = id]
}


# -----------------------------------------------------------------------------
# Real-world correlation between risk/variance and return
# -----------------------------------------------------------------------------
apa_print(d[!duplicated(index), cor.test(var_obj, mroi_obj)])$full_result


# -----------------------------------------------------------------------------
# Simple correlations (pooled) between subjective risk and subjective return
# -----------------------------------------------------------------------------
m <- ifelse(z, 'pearson', 'spearman')# method conditional on z-standardization
d[, .(cor = round(cor(risk_subj, ret_subj, method = m), 2)), by = .(graph, quest)][order(-graph,cor),]
xt <- xtable(d[, apa_print(cor.test(risk_subj, ret_subj, method = m))[c(1,2)], by = .(graph, quest)][order(-graph)],
  digits = 2,
  caption = 'Correlation between perception risk and objective risk (variance)',
  booktabs = TRUE,
  label = 'tab:study2_rr',
  align = 'lllrr')
names(xt) <- c('Graph', 'Question wording', '$r$ [95\\% CI]', 'Statistic')
print(xt,
  hline.after = c(0,3),
  add.to.row = list(pos = list(6), command = c("\\bottomrule\n\\multicolumn{5}{l}{{\\textit{Note}. Pearson correlations $r$ from pooled data, subjective values z-standardized at individual level.}}\n")),
  sanitize.text.function = function(x) { gsub('r = |95|CI|%','', x) },
  sanitize.colnames.function = function(x) x)


# -----------------------------------------------------------------------------
# Linear mixed-effects model
# -----------------------------------------------------------------------------
contrasts(d$quest)<- contr.sum(3)
contrasts(d$graph) <- contr.sum(2)
fit_id <- lmer(ret_subj~ quest*graph + perception_z + (quest*graph):perception_z + (1 + perception_z | id), d)

summary(fit_id)

make_lmer_table <- function(model) {
  # Table
  bstar <- lm.beta.lmer(model)
  b <- summary(model)$coefficients[, 1]
  SE <- summary(model)$coefficients[, 2]
  df <- summary(model)$coefficients[, 3]
  t <- summary(model)$coefficients[, 4]
  P <- summary(model)$coefficients[,5]
  CI <- confint(model, method="boot", .progress="txt", PBargs=list(style=3), nsim = 50)
  CI <- CI[names(b),]
  CIf <- apply(round(CI, digits), 1, function(x) paste0('[',paste(x, collapse=','),']'))
  digits <- 3
  fmt <- gsub('x', digits, '%.xf')

  tab <- cbind(
    b = sprintf(fmt, b),
    CI = CIf,
    bstar = sprintf(fmt, c(NA, bstar)),
    df = sprintf(fmt, df),
    t = sprintf(fmt, t),
    p =  papaja::printp(P))
}

tab <- make_lmer_table(fit_id)

row.names(tab) <- c('Constant', 'Risk (m. as variance)', 'Fluctuation', 'Graph', 'Perception', 'Graph:risk (m. as variance)', 'Graph:fluctuation', paste('Perception', levels(d$quest)[-3], sep=":"), 'Graph:perception', 'Graph:perception:risk (m. as variance)', 'Graph:perception:fluctuation')
roworder <- c(1,2,3,5,4, 8, 9, 6, 7, 10)
apa_table(tab[roworder, ],
  caption = 'Regression (mixed-effects model) with dependent variable: perception return \\\\\\label{study2:mixed_effects_model}',
  note = '\\textit{b}: unstandardized fixed effects; \\textit{b}$^*$ (standardized fixed effects; \\textit{df}: degrees of freedom (Satterthwaite); \\textit{CI}s are bootstrapped (500 runs).',
  escape = FALSE,
  align = c('l','r','c','r','c','r','r'),
  col.names = c('','b', '95\\%\\textit{CI}', '$b^*$', '\\textit{df}', '\\textit{t}', '\\textit{p}'),
  p = 'H')


# emm_options(pbkrtest.limit = 4840)
slopes <- test(emtrends(fit_id, ~quest | graph, var = 'perception_z'))
apa_table(slopes[,-2],
  digits = 3,
  al = c('l',  'c', 'c', 'r','r','r'),
  col.n = c('Question', 'Slope', 'SE', 'df', '\\textit{z}-ratio', '\\textit{p}'),
  p = 'H',
  caption = 'Estimated slopes from the linear-mixed effects modelwith dependent variable perception return')
emm_options(pbkrtest.limit = 3000)


# -----------------------------------------------------------------------------
# Partial correlation networks
# -----------------------------------------------------------------------------
# Generate a list with data subsets
vars <- c('id', 'var_obj', 'ret_subj_z', 'mroi_obj', 'perception_z')
datasubsets <- list(
  Risk_Graph_TRUE = setnames(d[quest=='Risk' & graph == 'Graph', vars, with = FALSE], length(vars), 'risk_subj'),
  Riskvar_Graph_TRUE = setnames(d[quest=='Risk (m. as var.)' & graph == 'Graph', vars, with = FALSE], length(vars), 'rvar_subj'),
  Fluctuation_Graph_TRUE = setnames(d[quest=='Fluctuation' & graph == 'Graph', vars, with = FALSE], length(vars), 'fluct_subj'),
  Risk_Graph_FALSE= setnames(d[quest=='Risk' & graph != 'Graph', vars, with = FALSE], length(vars), 'risk_subj'),
  Riskvar_Graph_FALSE = setnames(d[quest=='Risk (m. as var.)' & graph != 'Graph', vars, with = FALSE], length(vars), 'rvar_subj'),
  Fluctuation_Graph_FALSE = setnames(d[quest=='Fluctuation' & graph != 'Graph', vars, with = FALSE], length(vars), 'fluct_subj'))

# Fit the partial correlation networks
corlist <- lapply(datasubsets, function(z) {
  # We need to round because mlVAR does not want the data otherwise
  z[, names(z)[2:5] := lapply(.SD, round, 6), .SDcols = 2:5]
  mlVAR(z, vars = names(z)[2:5], idvar = 'id', lags = 1, temporal = "fixed")
  })
cat('fit partial correlation network -> "corlist"\n')
# Plot graph
alpha <- 0.0084 # Alpha level to use
source('fig5.R')

