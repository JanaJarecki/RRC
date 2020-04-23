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
  tab <- matrix(paste0(pcormat, ', ', pmat), ncol = ncol(pmat), dimnames = list(make_labels(rownames(pmat), " "), make_labels(rownames(pmat), " ")))
  tab[tab==', -'] <- ''
  tab[tab==', '] <- ''
  return(tab)
}

get_input_for_table <- function(fit) {
  pmat <- round(fit$results$Gamma_Theta$P, 3)
  pcormat <- round(fit$results$Theta$pcor$mean, 2)
  new_order <- c(5,2,1,3,4)
  pmat <- pmat[new_order, new_order]
  pcormat <- pcormat[new_order, new_order]

  starmat <- array(0, dim(pmat))
  starmat[pmat < 0.05 & !is.na(pmat)]  <- 1
  starmat[pmat < 0.01 & !is.na(pmat)]  <- 2
  starmat[pmat < 0.001 & !is.na(pmat)] <- 3
  stars <- c("*", "**", "***")
  res <- pcormat
  res[] <- sprintf("% .2f", res)
  res[] <- gsub("^ ", "\\\\hphantom{-}", res)
  for (i in 1:3) {
    res[starmat == i] <- paste0(res[starmat == i], "^", "{", stars[i], "}")
  }  
  res[] <- paste0("$", res, "$")
  diag(res) <- ''
  res[upper.tri(res)] <- ''
  rownames(res) <- paste(1:nrow(pmat), make_labels(rownames(pmat), " "))
  colnames(res) <- 1:nrow(pmat)
  res <- res[, -ncol(res)]
  return(res)
}

cat('defined function get_input_for_table()\n')