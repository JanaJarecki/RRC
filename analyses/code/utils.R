# This function calculates variance-inflation factors
# for lme models
# vif.lme <- function (fit) {
#     ## adapted from rms::vif
#     v <- vcov(fit)
#     nam <- names(fixef(fit))
#     ## exclude intercepts
#     ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
#     if (ns > 0) {
#         v <- v[-(1:ns), -(1:ns), drop = FALSE]
#         nam <- nam[-(1:ns)] }
#     d <- diag(v)^0.5
#     v <- diag(solve(v/(d %o% d)))
#     names(v) <- nam
#     v }

# This function calculates standardized beta-weights
# for lmer models
lm.beta.lmer <- function(mod) {
   b <- fixef(mod)[-1]
   sd.x <- apply(getME(mod,"X")[,-1],2,sd)
   sd.y <- sd(getME(mod,"y"))
   b*sd.x/sd.y
}