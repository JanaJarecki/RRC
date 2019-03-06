library(data.table)
library(cogsciutils)
# library(ordinal)
# library(lme4)
# library(mlVAR) # Partial correlation networks in hierarchical designs
# library(stargazer) # LaTex Tables
# library(papaja)
# library(reghelper)

# z-standardise or not?
z <- TRUE

# Read the data
d <- fread("../../data/processed/study2.csv") # Change this directory
d[, var_obj := var_obj/100]
# dem <- fread("../../data/processed/study1_demographics.csv")
# dem[, mean(age)]
vars <- 'risk_subj'
if (z) {
  v <- c(vars, 'ret_subj')
  d[, c(v) := lapply(.SD, as.numeric), .SDcols = v]
  d[, c(v) := lapply(.SD, function(z) c(scale(z))), .SDcols = v, by = .(quest, id)]
}


# ------------------------------------------------------------------------
# Correlations between objective variance and risk
m <- ifelse(z, "pearson", "spearman")
d[, .(cor = round(cor(risk_subj, var_obj, method = m), 2)), by = .(graph, quest)][order(-cor)]
# Print for manuscsript
apa_table(d[, apa_print(cor.test(risk_subj, var_obj, method = m))[c(1,2)], by = .(graph, quest)][order(-estimate)],
  caption = "Correlations between perceived risk and objective risk (variance)",
  note = "Correlations from pooled data.")


# Regression modelling
mrisk  <- lmer(var_obj ~ risk_subj  + (1 | id), data = d, REML = FALSE)
mvar   <- lmer(var_obj ~ var_subj   + (1 | id), data = d, REML = FALSE)
mpred  <- lmer(var_obj ~ pred_subj  + (1 | id), data = d, REML = FALSE)
mfluct <- lmer(var_obj ~ fluct_subj + (1 | id), data = d, REML = FALSE)
anova(mfluct, mvar, mpred, mrisk)
betas <- unlist(lapply(list(mfluct, mvar, mpred, mrisk), function(z) round(coef(beta(z))[2,1], 2)))
betas
akaike.weights <- function(models, digits = 2) {
  aics <- ceiling(unlist(lapply(models, AIC)))
  aic.deltas <-  aics - min(aics)
  aic.weights <- exp(- 0.5 * aic.deltas) / sum( exp(- 0.5 * aic.deltas)  )
  return(aic.weights)
}
# Table for paper
stargazer(mfluct, mvar, mpred, mrisk,
  label = "table:tab1",
  title  = "Linear mixed model results, dependent variable: objective variance of shown stocks",
  covariate.labels = c("Fluctuation", "Variability", "Predictability", "Risk"),
  ci = TRUE,
  digits = 2,
  digit.separator = "",
  initial.zero = FALSE,
  nobs = FALSE,
  dep.var.caption = paste0("Unstandardized b coefficients [range of dep. var. ", paste(round(d[, range(var_obj)], 2), collapse=" $-$ "), "]"),
  dep.var.labels.include = FALSE,
  notes.align = "l",
  report = c("vcst*"),
  align = FALSE,
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = "all",
  add.lines = list(
    c("AIC weight$^{+}$", sprintf("%.2f", akaike.weights(list(mfluct, mvar, mpred, mrisk)))),
    c("AIC", ceiling(unlist(lapply(list(mfluct, mvar, mpred, mrisk), AIC)))),
    c("BIC", ceiling(unlist(lapply(list(mfluct, mvar, mpred, mrisk), BIC)))),
    c("LogLik", floor(unlist(lapply(list(mfluct, mvar, mpred, mrisk), logLik)))),
    c("Random slope", rep("no", 4)),
    c("Random intercept", rep("participant", 4))
    ),
  notes = c(
    "$^{+}$Relative evidence strength, range 0-1 {\\citep{Wagenmakers2004}}",
    "Predictors are z-transformed."
    ),
  out = "C:/Users/Jana Jarecki/Google Drive/tab1.tex"
)



# ------------------------------------------------------------------------
#  Partial correlation network
# Read the data without z-standardized variables
dd <- fread("../../data/processed/study1.csv") # Change this directory
# Fit the model
fits <- lapply(vars, function(x) mlVAR(dd, vars = c('ret_subj', x, 'mroi_obj', 'var_obj'), idvar = "id", lags = 0))
alpha <- .01
lapply(fits, function(x) (x$results$Gamma_Theta$P < alpha) | t( x$results$Gamma_Theta$P ) < alpha)
lapply(fits, function(x) summary(x, show  = c("contemporaneous", "between"))[[1]])
options(scipen = 999)
round(fits[[2]]$results$Gamma_Theta$P,4)
summary(fits[[2]], show = 'contemporaneous')