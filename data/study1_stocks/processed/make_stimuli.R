library(data.table)
# SET THE WORKING DIRECTORY TO THE LOCATION OF THIS FILE
# --------------------------------------------------------------------------
# Rstudio: setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


stocks <- fread("all_stocks.csv")
stocks[, as.list(range(year)), by = firm]

dt <- fread("../../../data/study1_stocks/raw/ABBN.csv", select = 1:3)
setnames(dt, c("date", "start", "end"))
dt[, date := as.Date(date, format = "%d.%m.%Y")]
dt[, buy := end]
dt[, sell := dt[.(date = date+365), buy, roll = -Inf, rollends = FALSE, mult = "first", on="date"]]

dt[date == "2015-03-25" | date == "2016-03-29"]
