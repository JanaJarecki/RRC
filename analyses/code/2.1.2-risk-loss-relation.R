library(data.table)
library(papaja)
library(corx)


# z-standardise or not?
z <- FALSE

# Read the data
d <- fread("../../data/processed/study2.csv", key = 'id', colClasses = list(factor = c('id', 'index', 'quest'), double = "perception"))
# Mean-center the perceived ratings (1-7 Likert-type scale) 
d[, perception := perception - mean(perception), by = .(id, quest)]
d[, quest := factor(quest, levels = c("risk", "risk (variance)", "fluctuation"))]
d[, avgloss_obj := avgloss_obj/100]
d[, evloss_x_ploss_obj := avgloss_obj * ploss_obj]
# # z-standardize
# if (z) {
#   v <- c(vars, 'ret_subj')
#   d[, c(v) := lapply(.SD, as.numeric), .SDcols = v]
#   d[, c(v) := lapply(.SD, function(z) c(scale(z))), .SDcols = v, by = id]
# }

# ------------------------------------------------------------------------
# Objective correlation between loss and return
# ------------------------------------------------------------------------
d[!duplicated(index), apa_print(cor.test(mroi_obj, avgloss_obj))$estimate]
d[!duplicated(index), apa_print(cor.test(mroi_obj, ploss_obj))$estimate]
d[!duplicated(index), apa_print(cor.test(mroi_obj, evloss_x_ploss_obj))$estimate]



# ------------------------------------------------------------------------
# Perceived risk correlated with different measures of loss
# ------------------------------------------------------------------------
tab <- d[quest == "risk"& graph == TRUE][, .SD, .SDcols = c("perception", "mroi_obj", "avgloss_obj", "ploss_obj", "evloss_x_ploss_obj")]
cor_tab <- corx(tab, triangle = "lower", stars = c(0.05, 0.01, 0.001), describe = c(`$M$` = mean, `$SD$` = sd))
row.names(cor_tab$apa) <- paste(paste0("(", 1:5, ")"), c("Perceived Risk", "Return", "Mean(loss)", "P(loss)", "EV(loss | loss)"))
cat(papaja::apa_table(cor_tab$apa,
  caption = "Simple Correlation of Perceived Risk with Objective Measures of Loss",
  #col.names = c("Perceived Risk", "Return", "Mean(loss)", "P(loss)", "EV(loss | loss)", "$M$", "$SD$"),
  col_spanner = list("Objective" = c(2, 6)),
  note = "* p < .05; ** p < .01; *** p < .001. Perceived values mean-centered.",
  escape = F))

tab <- d[quest == "risk"& graph == FALSE][, .SD, .SDcols = c("perception", "mroi_obj", "avgloss_obj", "ploss_obj", "evloss_x_ploss_obj")]
cor_tab <- corx(tab, triangle = "lower", stars = c(0.05, 0.01, 0.001), describe = c(`$M$` = mean, `$SD$` = sd))
row.names(cor_tab$apa) <- paste(paste0("(", 1:5, ")"), c("Perceived Risk", "Return", "Mean(loss)", "P(loss)", "EV(loss | loss)"))
cat(papaja::apa_table(cor_tab$apa,
  caption = "Simple Correlation of Perceived Risk with Objective Measures of Loss",
  #col.names = c("Perceived Risk", "Return", "Mean(loss)", "P(loss)", "EV(loss | loss)", "$M$", "$SD$"),
  col_spanner = list("Objective" = c(2, 6)),
  note = "* p < .05; ** p < .01; *** p < .001. Perceived values mean-centered.",
  escape = F))