source('tab_setup.R')
tn <- 5
stargazer(m,
  label = 'tab:study1_lm_results',
  title  = 'Hierarchical linear model results, dependent variable: objective variance of shown stocks',
  #covariate.labels = c('Fluctuation', 'Variability', 'Predictability', 'Risk'),
  ci = TRUE,
  digits = 3,
  digit.separator = '',
  initial.zero = FALSE,
  nobs = FALSE,
  dep.var.caption = paste0('Unstandardized b coefficients [range of dep. var. ', paste(round(d[, range(var_obj)], 3), collapse=' $-$ '), ']'),
  dep.var.labels.include = FALSE,
  notes.align = 'l',
  report = c('vcst*'),
  align = FALSE,
  star.char = c('*', '**', '***'),
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = 'all',
  add.lines = list(
    c('AIC weight$^{+}$', sprintf('%.2f', akaike.weights(list(mfluct, mvar, mpred, mrisk)))),
    c('AIC', ceiling(unlist(lapply(list(mfluct, mvar, mpred, mrisk), AIC)))),
    c('BIC', ceiling(unlist(lapply(list(mfluct, mvar, mpred, mrisk), BIC)))),
    c('LogLik', floor(unlist(lapply(list(mfluct, mvar, mpred, mrisk), logLik))))
    ),
  notes = c(
    '$^{+}$Relative evidence strength, range 0-1 {\\citep{Wagenmakers2004}}',
    'Predictors are z-transformed',
    'Model: random intercept and slope by subject; fit: restricted maximum likelihood.'
    )
  ,out = paste0('../tables/tab', tn, '.tex')
)
