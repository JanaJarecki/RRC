library(data.table)
library(mlVAR)
source("fig_setup.R") # for plotting

# -----------------------------------------------------------------------------
# ROBUSTNESS CHECK I -  graph literacy
# -----------------------------------------------------------------------------
# Change the directory if needed, this path is relative to THIS file
d <- fread("../../data/processed/study1.csv")
vars <- paste0(c('risk', 'fluct', 'var', 'pred'), '_subj') # store variables
# Re-estimate network this time with graph literacy (glscore_calc)
med_gls <- d[!duplicated(id), median(glscore_calc)]
# describe graph literacy
d[!duplicated(id), .(mean(glscore_calc), median(glscore_calc), min(glscore_calc), max(glscore_calc))]
# table graph literacy above and below median graph literacy
d[!duplicated(id), table(glscore_calc > med_gls)]
n_high_gls <- d[!duplicated(id)][glscore_calc > med_gls, .N]
n_low_gls <- d[!duplicated(id)][glscore_calc <= med_gls, .N]
# Partial correlation networks
corelations_high_gls <- lapply(vars, function(i) mlVAR(
  data = d[glscore_calc > med_gls],
  vars = c('var_obj', 'ret_subj', 'mroi_obj', i),
  idvar = "id",
  lags = 1,
  temporal = 'fixed'))
corelations_low_gls <- lapply(vars, function(i) mlVAR(
  data = d[glscore_calc <= med_gls],
  vars = c('var_obj', 'ret_subj', 'mroi_obj', i),
  idvar = "id",
  lags = 1,
  temporal = 'fixed'))
plots_high_gls <- lapply(1:length(corelations_high_gls), function(z) plot_net(corelations_high_gls[[z]], Title = sub("Perceived ", paste(letters[z], ". "), make_labels(vars[z], " ")), mlvar = TRUE, alpha = 0.0125))
plots_low_gls <- lapply(1:length(corelations_low_gls), function(z) plot_net(corelations_low_gls[[z]], Title = sub("Perceived ", paste(letters[z], ". "), make_labels(vars[z], " ")), mlvar = TRUE, alpha = 0.0125))

# If we want to ignore the within-subject structure of the data, uncomment these lines
# corelations_high_gls <- lapply(vars, function(i) cor_auto(d[glscore_calc > med_gls, c('var_obj', 'ret_subj', 'mroi_obj', i), with = FALSE]))
# plots_high_gls <- lapply(1:4, function(z) plot_net(corelations_high_gls[[z]], Title = sub("Perceived ", paste(LETTERS[z], " "), make_labels(vars[z], " "))))
# corelations_low_gls <- lapply(vars, function(i) cor_auto(d[glscore_calc <= med_gls, c('var_obj', 'ret_subj', 'mroi_obj', i), with = FALSE]))
# plots_low_gls <- lapply(1:4, function(z) plot_net(corelations_low_gls[[z]], Title = sub("Perceived ", paste(LETTERS[z], " "), make_labels(vars[z], " "))))

# Save plot
png('../figures/sfig1.png', width = 1600, height = 1000, units = "px", res = 220, pointsize = 10)
plot.new()
layout(mat = matrix(c(1,2,3,4,9,9,9,9,5,6,7,8), 3, byr=T), heights = c(4, 1, 4))
par(family = "Roboto Condensed", oma = c(0, 0, 2.5, 0))
for (i in 1:4) {
  plot(plots_low_gls[[i]], oma=c(100,0,7,0))
}
for (i in 1:4) {
    plot(plots_high_gls[[i]], mar=c(0,0,10,0))
}
title(paste0("Low Graph Literacy (n=", n_low_gls, ")"), outer=TRUE, cex.main = 2.3, family = "Roboto Condensed", font=2)
mtext(paste0("High Graph Literacy (n=", n_high_gls, ")"), side = 3, line = -22, outer = TRUE, cex = 1.7, family = "Roboto Condensed", font = 2)
dev.off();




# -----------------------------------------------------------------------------
# ROBUSTNESS CHECK II - graph literacy
# -----------------------------------------------------------------------------
# Change the directory if needed, this path is relative to THIS file
d <- fread("../../data/processed/study1.csv")
vars <- paste0(c('risk', 'fluct', 'var', 'pred'), '_subj') # store variables
d[, stockmarket_exp := rowMeans(cbind(experience_stockmarket_subj, experience_swiss_stockmarket_subj))]
d[, .(MED=median(stockmarket_exp), M=mean(stockmarket_exp), SD=sd(stockmarket_exp), MAX=max(stockmarket_exp), MIN=min(stockmarket_exp))][, lapply(.SD, round, 2)]

# Re-estimate network this time with graph literacy (glscore_calc)
med_exp <- d[!duplicated(id), median(stockmarket_exp)]
# table above and below median
split_exp <- 2
n_high_exp <- d[!duplicated(id)][stockmarket_exp >= split_exp, .N]
n_low_exp <- d[!duplicated(id)][stockmarket_exp < split_exp, .N]
# Partial correlation networks
corelations_high_exp <- lapply(vars, function(i) mlVAR(
  data = d[stockmarket_exp >= split_exp],
  vars = c('var_obj', 'ret_subj', 'mroi_obj', i),
  idvar = "id",
  lags = 1,
  temporal = 'fixed'))
corelations_low_exp <- lapply(vars, function(i) mlVAR(
  data = d[stockmarket_exp < split_exp],
  vars = c('var_obj', 'ret_subj', 'mroi_obj', i),
  idvar = "id",
  lags = 1,
  temporal = 'fixed'))
plots_high_exp <- lapply(1:length(corelations_high_exp), function(z) plot_net(corelations_high_exp[[z]], Title = sub("Perceived ", paste(letters[z], ". "), make_labels(vars[z], " ")), mlvar = TRUE, alpha = 0.0125))
plots_low_exp <- lapply(1:length(corelations_low_exp), function(z) plot_net(corelations_low_exp[[z]], Title = sub("Perceived ", paste(letters[z], ". "), make_labels(vars[z], " ")), mlvar = TRUE, alpha = 0.0125))
# The following code calculates without the within-data structure

# corelations_high_exp <- lapply(vars, function(i) cor_auto(d[stockmarket_exp >= split_exp, c('var_obj', 'ret_subj', 'mroi_obj', i), with = FALSE]))
# plots_high_exp <- lapply(1:4, function(z) plot_net(corelations_high_exp[[z]], Title = sub("Perceived ", paste(LETTERS[z], " "), make_labels(vars[z], " "))))
# corelations_low_exp <- lapply(vars, function(i) cor_auto(d[stockmarket_exp < split_exp, c('var_obj', 'ret_subj', 'mroi_obj', i), with = FALSE]))
# plots_low_exp <- lapply(1:4, function(z) plot_net(corelations_low_exp[[z]], Title = sub("Perceived ", paste(LETTERS[z], " "), make_labels(vars[z], " "))))

# Save plot
png('../figures/sfig2.png', width = 1600, height = 1000, units = "px", res = 220, pointsize = 10)
plot.new()
layout(mat = matrix(c(1,2,3,4,9,9,9,9,5,6,7,8), 3, byr=T), heights = c(4, 1, 4))
par(family = "Roboto Condensed", oma = c(0, 0, 2.5, 0))
for (i in 1:4) {
  plot(plots_low_exp[[i]], oma=c(100,0,7,0))
}
for (i in 1:4) {
    plot(plots_high_exp[[i]], mar=c(0,0,10,0))
}
title(paste0("Little Stock Market Experience (n=", n_low_exp, ")"), outer=TRUE, cex.main = 2.3, family = "Roboto Condensed", font=2)
mtext(paste0("Medium/high Stock Market Experience (n=", n_high_exp, ")"), side = 3, line = -22, outer = TRUE, cex = 1.7, family = "Roboto Condensed", font = 2)
dev.off();