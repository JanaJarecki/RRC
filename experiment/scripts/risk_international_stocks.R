library(data.table)
library(ggplot2)
library(themejj); theme_set(themejj())
library(ggsavejj)
library(fasttime)
library(lubridate)


d <- fread("../4-Data/international_stocks_ROI.csv")
d[, date := as.Date(date)]
setkey(d, index, date)

# helper variables
indices <- d[, unique(index)]
from <- 2009
to <- 2015
d <- d[year(date) >= from & year(date) <= to]
d[, year := year(date)]

ggplot(d[, sd(mean_roi_peryear, na.rm=TRUE), by = list(index, year)], aes(x=year,y=V1,shape=index)) +geom_point()

# Aggregate ROI: this time - as opposed to study 1 - we use the mean ROI because it correlates almost perfectly with the ROI on the firs day of each year for each firm
d <- d[, .(mean_roi_peryear = mean(mean_roi_peryear, na.rm = T)), by = list(index, year)]


# ggplot(tmp, aes(x=first_day_roi, y=mean_roi)) +geom_point() +facet_wrap(~index) +themejj(facet=T) +geom_abline() +coord_fixed() +xlim(-x,x) +ylim(-x,x)
    

# d[, mean(mean_roi_peryear, na.rm = T), by = list(index, year)]

# Save aggregated data
write.table(d, "../4-Data/international_ROI_2009_2015.csv", sep=";", row.name= F)






d <- fread("../4-Data/international_ROI_2009_2015.csv")
d <- d[!index %in% c("jse","DAXK","MERV","Nikkei225")]
d[, date := as.Date(date)]

startyears <- 2009

for (sy in startyears)
{
    dv <- d[!is.na(mean_roi_peryear) & year > sy, list(SD = sd(mean_roi_peryear), EV = mean(mean_roi_peryear), EV_Loss = mean(abs(mean_roi_peryear[mean_roi_peryear < 0])) * sum(mean_roi_peryear < 0)/.N), by = index]
    dv
    # & !index %in% c("MERVAL", "RTS")

    # Risk-return correlation
    pV <- ggplot(dv, aes(SD,EV)) +geom_point(size = 2, color = "mediumpurple1") +geom_text(aes(label=index), hjust = 0, size = 2) +stat_summary(fun.data=mean_cl_normal) +geom_smooth(method='lm', alpha = .1, color = "mediumpurple3") +ggtitle("Risk (Variance) Return Relationship International")
    pL <- ggplot(dv, aes(SD,EV_Loss)) +geom_point(size = 2, color = "mediumpurple1") +geom_text(aes(label=index), hjust = 0, size = 2) +stat_summary(fun.data=mean_cl_normal) +geom_smooth(method='lm', alpha = .1, color = "mediumpurple3") +ggtitle("Loss (EV) Risk Relationship International")
    pLR <- ggplot(dv, aes(EV,EV_Loss)) +geom_point(size = 2, color = "mediumpurple1") +geom_text(aes(label=index), hjust = 0, size = 2) +stat_summary(fun.data=mean_cl_normal) +geom_smooth(method='lm', alpha = .1, color = "mediumpurple3") +ggtitle("Loss (EV) Return Relationship International")
    library(egg)
    pC <- grid.arrange(pV,pL,pLR,nrow=2,top=paste0(sy, " - 2017"))
    ggsavejj(paste0("risk_return_international_", sy, "-2017"), pC)
}