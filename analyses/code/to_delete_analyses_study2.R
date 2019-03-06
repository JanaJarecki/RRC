library(data.table)
library(ordinal)
d <- fread("../../data/processed/study2.csv")
d[, var_obj := var_obj/100]
d[, risk_subj := factor(risk_subj, ordered = TRUE)]

# With graph
dg <- d[graph == TRUE]
risk_model <- clmm(risk_subj ~ var_obj + (var_obj | id), data = dg[quest == "risk"])
riskvar_model <- clmm(risk_subj ~ var_obj + (1 + var_obj | id), data = dg[quest == "risk (variance)"], link = "probit")
fluct_model <- clmm(risk_subj ~ var_obj + (1 + var_obj | id), data = dg[quest == "fluctuation"], link = "probit")



# Without graph
dng <- d[graph == FALSE]
risk_model <- clmm(risk_subj ~ var_obj + (1  + var_obj | id), data = dng[quest == "risk"], link = "probit")
riskvar_model <- clmm(risk_subj ~ var_obj + (1 + var_obj | id), data = dng[quest == "risk (variance)"], link = "probit")
fluct_model <- clmm(risk_subj ~ var_obj + (1 + var_obj | id), data = dng[quest == "fluctuation"], link = "probit")