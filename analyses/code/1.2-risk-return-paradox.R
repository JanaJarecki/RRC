library(data.table) # powerful data handling in general
library(mlVAR) # Estimate partial correlation networks (hierarchical)
library(papaja) # Print apa tables
library(lme4) # Linear mixed-effects models
library(emmeans) # Marginal means and sloes of mixed-effects models
source('utils.R') # other utility functions not in packages


# Read the data
d <- fread("../../data/processed/study1.csv", colClasses  = list(factor = c('id', 'index')))
d[, quest := factor(quest, levels = c('Risk', 'Fluctuation', 'Variability', 'Predictability'))]
# Z-standardizes perceiveds by-individual (1-7 Likert-type scale) 
d[, perception_z := scale(perceived), by=.(id, quest)]
d[, ret_subj_z := scale(ret_subj), by=.(id, quest)]
# d[, mroi_obj_z := scale(mroi_obj), by = .(quest, id)]
# d[, var_obj_z := scale(var_obj), by = .(quest, id)]


# -----------------------------------------------------------------------------
# Real-world correlation between risk/variance and return
# -----------------------------------------------------------------------------
apa_print(d[!duplicated(index), cor.test(var_obj, mroi_obj)])$full_result



# -----------------------------------------------------------------------------
# Perceived correlation between risk/variance/fluctuation/predictability and return
# -----------------------------------------------------------------------------
d[, apa_print(cor.test(ret_subj_z, perception_z))[[1]], by=quest]
tab <- d[, as.list(unlist(cor.test(ret_subj_z, perception_z)[c('estimate', 'conf.int', 'statistic', 'p.value')])), by = quest]
apa_table(tab,
  col.names = c('', '\\textit{r}', 'CI1', 'CI2', '\\textit{t(1822)}', '\\textit{p}'),
  caption = 'SStudy1: Simple correlations between the perceived return and the perception of variance
  \\label{sup:tab:study1_rrc}',
  p = 'H',
  d = 3,
  al = c('l','l','r','r'),
  e = F,
  n = 'Pearson correlations $r$ from pooled data, subjective values z-standardized at individual level.')


# ----------------------------------------------------------------------------
# Mixed-effects model
# ----------------------------------------------------------------------------
contrasts(d$quest) = contr.sum(4) # Sets effect coding (deviance coding)

# Runs the model
# Testing maximum-complexity models that fail
fit_full <- lmer(ret_subj ~ quest*perception_z + (quest*perception_z|id) + (quest*perceived|index) + (1|id:index), d) # Results in a SINGULAR model
fit_index <- afex::lmer_alt(ret_subj ~ quest*perception_z + (1 + quest:perception_z || index), d) # Results in a SINGULAR model

fit_id <- afex::lmer_alt(ret_subj ~ perception_z + quest:perception_z + (1 + quest:perception_z || id), d)

vcmat <- as.matrix(vcov(fit_id, full = TRUE))
class(vcmat) <- 'matrix'
vcmat[upper.tri(vcmat)] <- NA
nn <- paste0('(', 1:5, ') ', c('Constant', 'Perception', paste('Perception', levels(dl$quest)[-4], sep=":")))
rownames(vcmat) <- nn
colnames(vcmat) <- paste0('(', 1:5, ')')
apa_table(list(vcmat), digits = 3, caption="Study1: Variance-covariance matrix of the linear mixed-effects model", note = " ':' denotes interaction terms. Model specification see footnotes in main text.", placement = 'H')


apa_table(anova(fit_id), caption = "Overview: linear-mixed effects model, dependent variable: perceived return \\label{study1:lme_summary}", escape = F, digits = 3, placement = 'H')



# Table
bstar <- lm.beta.lmer(fit_id)
b <- summary(fit_id)$coefficients[, 1]
SE <- summary(fit_id)$coefficients[, 2]
df <- summary(fit_id)$coefficients[, 3]
t <- summary(fit_id)$coefficients[, 4]
P <- summary(fit_id)$coefficients[,5]
CI <- confint(fit_id, method="boot", .progress="txt", PBargs=list(style=3), nsim = 500)
CI <- CI[names(b),]
CIf <- apply(round(CI, digits), 1, function(x) paste0('[',paste(x, collapse=','),']'))

digits <- 3
fmt <- gsub('x', digits, '%.xf')

emmip(fit_id, perception_z ~ quest)

tab <- cbind(
  b = sprintf(fmt, B),
  CI = CIf,
  bstar = sprintf(fmt, c(NA, bstar)),
  df = sprintf(fmt, df),
  t = sprintf(fmt, t),
  p =  papaja::printp(P))
row.names(tab) <- c('Constant', 'Perception', paste('Perception', levels(dl$quest)[-4], sep=":"))
apa_table(tab,
  caption = 'Regression (mixed-effects model) with dependent variable: perceived return \\\\label{study1:mixed_effects_model}',
  note = '\\textit{b}: fixed effects (unstandardized); \\textit{b}$^*$ (standardized); \\textit{df}: degrees of freedom (Satterthwaite); \\textit{CI}s are bootstrapped (500 runs); questition main effects are excluded, because the criterion was measured once for all within-subject questitions and did thus not vary across questitions.',
  escape = FALSE,
  align = c('l','r','c','r','c','r','r'),
  col.names = c('','b', '95\\%\\textit{CI}', '$b^*$', '\\textit{df}', '\\textit{t}', '\\textit{p}'),
  p = 'H')

# emm_options(pbkrtest.limit = 7296)
# emm_options(pbkrtest.limit = 3000)

apa_table(test(emtrends(fit_id, ~quest, var = 'perception_z')),
  caption = "Slopes of the questition variable in the linear-mixed effects model, dependent variable: perceived return \\label{study1_lme_trends}",
  placement = 'H',
  digits = 3,
  align = c('l','r','r','r','r','r'),
  col.names = c('','slope', 'SE', '\\textit{df}', '\\textit{z}-ratio', '\\textit{p}'),
  e = F,
  n = 'Degrees-of-freedom method: Satterthwaite')


# ----------------------------------------------------------------------------
# Partial correlations using the clustered structure
# ----------------------------------------------------------------------------
# Here we do NOT z-standardize the data as compared to the other analysie
# because the partial correlation networks do not converge with the
# z-standardized data
dd <- dcast(d, id + var_obj + ret_subj_z + mroi_obj ~ quest, value.var = 'perception_z')
vars <- levels(d$quest)
# We need to round because mlVAR does not want the data otherwise
dd[, c(vars, 'ret_subj_z') := lapply(.SD, round, 6), .SDcols = c(vars,  'ret_subj_z')]
corlist <- lapply(vars, function(i) {
  mlVAR(dd, vars = c('var_obj', 'ret_subj_z', 'mroi_obj', i), idvar = "id", lags = 1, temporal = 'fixed')})

source('fig3.R')




# ----------------------------------------------------------------------------
# Extract correlation coefficients
# ----------------------------------------------------------------------------
# The table for the supplementary material is in the script
source('xtab2.R')

# # HOWTO
# # Extract exact weights and p-values for the partial correlations
# # For "Peceived Risk"
# options(scipen = 99)
# round(cbind(part_cor=fits[[1]]$results$Theta$pcor$mean[,2], p_value=fits[[1]]$results$Gamma_Theta$P[,2]), 4)
# #           part_cor   p_value
# #   ret_subj  -0.130         0
# #   risk_subj  1.000       NaN
# #   mroi_obj  -0.116         0
# #   var_obj    0.483         0

lubridate::hm(as.POSIXlt("2019-01-01 01:00:00"))
 - lubridate::hm(as.POSIXct("2019-01-01 12:00:00"))

hour(as.POSIXct("2019-01-01 01:00")) - hour(as.POSIXct("2019-01-01 22:00"))