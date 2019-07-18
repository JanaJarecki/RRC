library(kableExtra)
library(knitr)
source('fig_setup.R')
options("xtable.table.placement" = "H")
options("xtable.caption.placement" = "top")
options("xtable.include.rownames" = FALSE)
options("xtable.booktabs" = TRUE)


get_input_for_table <- function(fit) {
  pmat <- round(fit$results$Gamma_Theta$P, 3)
  pcormat <- round(fit$results$Theta$pcor$mean, 2)
  boldmat <- (pmat < alpha & !is.na(pmat))
  smallpmat <- pmat == 0 & !is.na(pmat)

  pcormat[boldmat] <- paste0('\\textbf{', pcormat[boldmat], '}')
  pmat[] <- paste('$ p=', sprintf('%.2f', pmat), '$')
  pmat[smallpmat] <- "$p<.001$"

  diag(pmat) <- '-'
  diag(pcormat) <- ''
  pmat[upper.tri(pmat)] <- ''
  pcormat[upper.tri(pcormat)] <- ''
  tab <- matrix(paste0(pcormat, ', ', pmat), ncol = 4, dimnames = list(make_labels(rownames(pmat), " "), make_labels(rownames(pmat), " ")))
  tab[tab==', -'] <- ''
  tab[tab==', '] <- ''
  return(tab)
}

cat('defined function get_input_for_table()\n')