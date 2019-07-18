# Table for supplement
# Stud<y 1 stimuli: swiss stocks and ROIs
library(data.table)
source("tab_setup.R")


# Save aggregated data
d <- fread("../../data/stockmarket_study1_swiss/processed/ROI_2002_2015.csv")
# Make wide data
dd <- dcast(d, firm ~ year, value.var = 'ROIyearly')
dd[, EV := mean(unlist(.SD)), .SDcols = 2:14, by = firm]
dd[, Var := mean((unlist(.SD) - EV)^2), .SDcols = 2:14, by = firm]

# Check against the ROIs in the main dataset
# RODO: Here is an inconsistency

roi_main_data <- fread("../../data/processed/study1.csv")
dd[roi_main_data[!duplicated(index), .(mroi_obj, var_obj), by = index], on = c(firm='index')]

print(xtable(dd, caption = "Stocks used in Study 1 and their yearly ROIs", digits = 3))