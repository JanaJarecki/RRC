library(data.table)
library(lme4)
library(mlVAR) # Partial correlation networks in hierarchical designs
library(stargazer) # LaTex Tables
library(xtable)
library(papaja)
library(reghelper)
library(MicroMacroMultilevel) # predict group-level DV from individual-level IV

# z-standardise or not?
z <- FALSE

# Read the data
d <- fread("../../data/processed/study1.csv", key = 'id', colClasses = list(factor = c('id', 'index')))
# Mean-center the subjective ratings (1-7 Likert-type scale) 
vars <- c('fluct_subj','risk_subj','var_subj','pred_subj','ret_subj')
d[, c(paste0(vars, '_c')) := lapply(.SD, function(x) x-mean(x)), .SDcols = vars, by = id]
vars <- vars[-5] # delete the return variable

# # z-standardize
# if (z) {
#   v <- c(vars, 'ret_subj')
#   d[, c(v) := lapply(.SD, as.numeric), .SDcols = v]
#   d[, c(v) := lapply(.SD, function(z) c(scale(z))), .SDcols = v, by = id]
# }


# ------------------------------------------------------------------------
# Perceived risk correlated with objective variance
# ------------------------------------------------------------------------
m <- ifelse(z, "pearson", "spearman") # correlation method
round(d[, lapply(.SD, cor, y = var_obj), .SDcols = vars], 2)
d[, lapply(lapply(.SD, cor.test, y = var_obj), apa_print), .SDcols = vars] # Print for manuscsript


# ------------------------------------------------------------------------
# Linear mixed-effects modeling
# ------------------------------------------------------------------------
# Design: 1 x 4 (question wording) within-subject design
# Variables:
#    var_obj: objective variance of stocks (group-level)
#    xxx_subj_c: subjective ratings (4 per participant)
# long format data
dl <- melt(d, id = c('id', 'index','var_obj','mroi_obj','ret_subj'), measure = paste0(vars, '_c'), variable.name = 'question', value.name = 'perception')
dl[, question := factor(question, levels = paste0(vars, '_c'), labels = c('Fluctuation', 'Risk', 'Variability', 'Predictability'))]
# Change the coding to effects coding (aka deviance coding)
contrasts(dl$question) = contr.sum(4) # set contrast coding
# Predictability will be left out

# 1. Test which complexity is justified
# In the models we always include the random slope for the interaction with
# the within-subjects factor
mfull <- lmer(var_obj ~ question:perception + mroi_obj + ret_subj + (question:perception + ret_subj|id), data = dl)
# Obtain the generalized variance-inflation factor (gVIF), squared.
# Squaring allowes for common guidelines: VIF<5 indicates okay-ish collinearity
max((car::vif(mfull)[,3])^2)
mqp <- lmer(var_obj ~ question:perception + mroi_obj + ret_subj + (question:perception|id), data = dl)
max((car::vif(mqp)[,3])^2)
mfe <- lm(var_obj ~ question:perception + mroi_obj + ret_subj, data = dl)
max((car::vif(mfe)[,3])^2)
anova(mfull, mqp, refit = FALSE)

# Table for the appendox
stargazer(mfull, mqp, mfe)

# Trend Analysis
dl[question == 'Predictability', perception := -perception]
emm <- emmeans(mfull, ~question:perception)
pairs(emm)

trnd <- emtrends(mfull, ~question, var = 'perception')
prs <- pairs(trnd)
apa_table(test(prs)[, -4], digits = 3)

# We set up a structural equation model in openMX here
# First we make the necessary variables numeric
# TODO figure this out!!
# contrasts(d$question) = contr.sum(4) # set contrast coding
dl[, idn := as.numeric(as.factor(id), ordered = TRUE)]
dl[, indexn := as.numeric(as.factor(index), ordered = TRUE)]
# d[, questionn := ]


lmerfit <- lmer(ret_subj ~ perception + (perception | id) + (perception | index), data = dl)

library(OpenMx)

# Subjects Model
idModel <- mxModel(
  'idModel', type = 'RAM',
  latentVars = c('slope', 'intercept'),
  mxData(observed = data.frame(idn = unique(dl$idn)),
         type = 'raw', primaryKey = 'idn'),
  mxPath(from = c('intercept', 'slope'), arrows = 2, values = 1),
  mxPath(from = 'intercept', to = 'slope', arrows = 2, values = .25, labels = 'cov1')
  )

indexModel <- mxModel(
  'indexModel', type = 'RAM',
  latentVars = c('slope', 'intercept'),
  mxData(observed = data.frame(indexn = unique(dl$indexn)),
    type = 'raw', primaryKey = 'indexn'),
  mxPath(from = c('intercept', 'slope'), arrows = 2, values = 1),
  mxPath(from = 'intercept', to = 'slope', arrows = 2, values = .25, labels = 'cov2'))

# Main Model
retsubjModel <- mxModel(
  model = 'retsubjModel', type = 'RAM', idModel, indexModel,
  manifestVars = 'ret_subj', # DV/outcome
  latentVars = 'perception', # IV/explanatory
  mxData(dl, 'raw', sort = FALSE), # -> DV
  mxPath(from = 'one', to = 'ret_subj'), 
  mxPath(from = 'one', to = 'perception',  # -> IV
         arrows = 1, free = FALSE,
         labels='data.perception'), 
  mxPath(from = 'perception', to = 'ret_subj', # IV -> DV
         arrows = 1, free = TRUE), 
  mxPath(from = 'ret_subj',
        arrows = 2,
        values = 1),
  mxPath(from = c('idModel.intercept', 'idModel.slope'), to = 'ret_subj',
         free = FALSE, values = c(1,NA),
         #labels = c(NA, 'data.perception'), 
         joinKey='idn'),
  mxPath(from = c('indexModel.intercept', 'indexModel.slope'), to = 'ret_subj',
         free = FALSE, values = c(1,NA),
         #labels = c(NA, 'data.perception'), 
         joinKey='indexn'))

mxfit <- mxRun(retsubjModel)

omxCheckCloseEnough(logLik(lmerfit), logLik(mxfit), 1e-4)
summary(lmerfit)
summary(mxfit)

library('plot_setup.R')
# Fit individual models
mrisk  <- lmer(var_obj ~ risk_subj  +  mroi_obj + ret_subj + (1 + risk_subj | id), data = d)
mvar   <- lmer(var_obj ~ var_subj   +  mroi_obj + ret_subj + (1 + risk_subj | id), data = d)
mpred  <- lmer(var_obj ~ pred_subj  +  mroi_obj + ret_subj + (1 + risk_subj | id), data = d)
mfluct <- lmer(var_obj ~ fluct_subj +  mroi_obj + ret_subj + (1 + risk_subj | id), data = d)
anova(mfluct, mvar, mpred, mrisk, refit = FALSE)
betas <- unlist(lapply(list(mfluct, mvar, mpred, mrisk), function(z) round(coef(beta(z))[2,1], 2)))
betas

