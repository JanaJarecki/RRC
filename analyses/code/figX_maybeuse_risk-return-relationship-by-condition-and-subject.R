# -----------------------------------------------------------------------------
# Plot of perceived risk-return relationship by condition and subject
# -----------------------------------------------------------------------------
source('fig_setup.R')
library(data.table)

# Read the data
d <- fread('../../data/processed/study2.csv')

# Center the data
d[, risk_subj := risk_subj - 4]
d[, ret_subj := ret_subj - 4]
d[, affect_mw := rowMeans(cbind(affect_gut_subj, affect_interessant_subj, affect_stark_subj, affect_aktiv_subj) - 3)]

# z-standardize the data
if (z) {
  v <- c('risk_subj', 'ret_subj')
  d[, c(v) := lapply(.SD, as.numeric), .SDcols = v]
  d[, c(v) := lapply(.SD, function(z) c(scale(z))), .SDcols = v, by = .(quest, id)]
}

# Plot for Study 1
ggplot(dl, aes(perception, var_obj)) +geom_line(stat='smooth',method = 'lm', alpha = 0.07, size = .8, aes(group = index)) +geom_line(stat='smooth', method='lm', size = 1, color = '#4DB3C1') +facet_wrap(~question, scales = 'free') +theme(legend.position = 'none') +scale_x_continuous('Perceived Risk', expand=c(0,0)) +scale_y_continuous('Perceived Return', expand=c(0,0)) +themejj(facet=TRUE) +theme(aspect.ratio = 1) +labs(caption = 'Note: values centered at the scale midpoint and z-standardized')

# Plot for Study 2
ggplot(d, aes(risk_subj, ret_subj)) +geom_line(stat='smooth',method = 'lm', alpha = 0.07, size = .8, aes(group = id)) +geom_line(stat='smooth', method='lm', size = 1, color = '#4DB3C1') +facet_grid(graph~quest, scales = 'free') +theme(legend.position = 'none') +scale_x_continuous('Perceived Risk', expand=c(0,0)) +scale_y_continuous('Perceived Return', expand=c(0,0)) +themejj(facet=TRUE) +theme(aspect.ratio = 1) +labs(caption = 'Note: values centered at the scale midpoint and z-standardized')