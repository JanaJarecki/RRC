library(data.table)
library(ggplot2)
library(themejj)
library(fasttime)
library(ggsavejj)
theme_set(themejj())
library(extrafont)
# library(devtools)
# install_github("janajarecki/ggsavejj", force=TRUE)
folder <- "../3-Experiment/Stimuli/Stocks"



# ######################################################################
# Preprocess the data
# filenames <- list.files("../4-Data/SwissMarket", pattern="*.csv", full.names=TRUE)

# ldf <- lapply(filenames, function(filename) {
#     dt <- fread(filename, colClasses = list("numeric" = 2:6), col.names = c("date", "start", "end", "dayMin", "dayMax", "revenue"))
#     dt <- dt[, lapply(.SD, function(x) as.numeric(gsub("'","",x))), by=date]
#     dateformat <- ifelse(dt[1, nchar(date)] == 8, "%d.%m.%y", "%d.%m.%Y")
#     dt[, date := as.Date(date, dateformat)]
#     firm <- tail(strsplit(sub(".csv","",filename), "/")[[1]], 1)
#     dt[, firm := firm]
#     return(dt)
#     })
# d <- rbindlist(ldf)
# d <- d[date <= "2016-10-26"]
# write.table(d, "../4-Data/data_risk_CH.csv", sep=";", row.names = F)
# #######################################################################


# #######################################################################
# # Calculate the ROI
# d <- fread("../4-Data/data_risk_CH.csv")
# date1 <- d[, date]
# d[, date := fastPOSIXct(date, "GMT", 3)]
# setkey(d, firm, date)

# library(lubridate)

# # Calculate ROI for the specified value between each date in dateVector and the targetdate. If the targetdate is not a sales date (i.e. does not exist in dateVector) one day later than targetdate is used as sales date, if that does not exist, another day later is used, etc.
# getValue <- function(value, dateVector, targetdate)
# {
#     dateVector <- trunc(dateVector, "day")
#     targetdate <- trunc(targetdate, "day")

#     if (max(dateVector) < targetdate)
#     {
#         return(NA)
#     }

#     if (sum(dateVector == targetdate) == 0)
#     {
#         out <- getValue(value, dateVector, targetdate = as.Date(targetdate) + days(1))
#     }
#     else {
#         out <- value[dateVector == targetdate]        
#     }
#     return(out)
# }

# getROI <- function(buy, sell)
# {
#     out <- (sell - buy) / buy
#     return(out)
# }
# getROI <- Vectorize(getROI)

# # Time difference
# d[, plus1day := date + days(1), by = date]
# d[, plus1month := date %m+% months(1), by = date]
# d[, plus1year := date %m+% years(1), by = date]

# # ROI
# d[, ROIdaily := getROI(start, sapply(plus1day, function(x) getValue(start, as.Date(date), x))), by = firm]
# d[, ROIyearly := getROI(start, sapply(plus1year, function(x) getValue(start, as.Date(date), x))), by = firm]
# d[, ROImonthly := getROI(start, sapply(plus1month, function(x) getValue(start, as.Date(date), x))), by = firm]

# # Save
# write.table(d, file = "../4-Data/data_risk_ROI.csv", row.names=F, sep=";")
#######################################################################
d <- fread("../4-Data/data_risk_ROI.csv")
d[, date := fastPOSIXct(date, "GMT", 3)]
setkey(d, firm, date)

# helper variables
firms <- d[, unique(firm)]
from <- 2003
to <- 2015
d <- d[year(date) >= from & year(date) <= to]
d[, year := year(date)]

# Get trading for first date of the year
d <- d[, list(date = as.POSIXct(paste0(year,"-06-01")), ROIyearly = ROIyearly[date==min(date)]), by = list(firm, year)]

# Save aggregated data
write.table(d, "../4-Data/data_risk_ROI_2002_2015.csv", sep=";", row.name= F)



dv <- d[, paste("Var = ", round(sd(ROIyearly),2), "EV = ", round(mean(ROIyearly),2)), by = firm]

plotit <- function(i)
{
    minmax <- c(-1,1) * d[, ceiling(max(abs(get(i)), na.rm=T)  * 10)/10 ]

    sd <- d[, list(sd = sd(get(i))), by = firm]

    title = ifelse(i=="ROIyearly", "Jährliche Rendite von 20 Aktien bei Verkauf 2015", i)

    p <- ggplot(d, aes_string(x="year", y=i)) +
    geom_hline(yintercept=0, color="grey50", size = .2) +
    # geom_bar(stat="identity") +
    geom_point() +
    facet_wrap(~firm, scales="free_x") +
    themejj(facet=T) +
    ggtitle(title) +
    # scale_x_datetime("", date_labels = "%b", date_breaks="1 month",expand=c(0,0)) +
    # scale_x_datetime("", date_labels = "%Y") +
    scale_x_continuous("") +
    scale_y_continuous("Rendite nach einem Jahr", limits=minmax, labels=scales::percent, breaks = c(0, seq(minmax[1], minmax[2], .1), 1)) +
    themejj(base_size = 18, facet = T) +
    theme(axis.text.x = element_text(hjust=.5), axis.ticks.x=element_blank(), strip.text = element_blank()) +
    ggtitle("Jahresrendite zum ersten Handelstag") +
    # geom_text(data = dv, aes(x = as.POSIXct("2012-06-01"), y = rep(minmax[2],20)-.1, label = V1))
    geom_text(data = dv, aes(x = 2012, y = rep(minmax[2],20)-.1, label = V1))

    print(p)

    ggsave(file.path(folder, "All_Stocks.png"),p, s = 1, w = 14)

}
plotit("ROIyearly")




d[, ROIyearly2 := ifelse(ROIyearly > 1, 1, ifelse(ROIyearly < -1, -1, ROIyearly))]

firms <- firms[!firms=="JuliusBaerGrpN"]




sapply(seq_along(firms), function(j)
{
    # y-axis ranges
    minmax <- c(-1,1) * (d[, ceiling(max(abs(ROIyearly2), na.rm=T)  * 10)/10] -.05)
    minmax <- c(-1,1)

    # one-firm dataset
    tmp <- d[firm==firms[j]]

    # plot
    p <- ggplot(tmp, aes_string(x="year", y="ROIyearly2")) +
        themejj(base_size = 13, facet = TRUE) +
        theme(axis.text.x = element_text(hjust=0),
              axis.ticks.x=element_blank(),
              strip.text=element_blank(),
              axis.line.x = element_blank(),
              panel.grid.major = element_line(color="grey70", size = .002, linetype = 3)) +
        geom_hline(yintercept=0, color="black", size=.04) +
        geom_point(size=2) +
        facet_wrap(~firm, scales="free_x") +
        scale_x_continuous("", breaks = d[, unique(year)]) +
        scale_y_continuous("Rendite nach einem Jahr", limits=minmax, labels=scales::percent, breaks = c(-seq(0, minmax[2], .50), seq(0, minmax[2], .50), limits = minmax), expand=c(0,0.05)) +
        theme(axis.text.x = element_text(angle=90))

    sd <- tmp[year(date) >= from & year(date) <= to, sd(ROIyearly)]

    filename <- paste0("s","_",round(sd,4)*100,"_",firms[j],".png")

    ggsave(file.path(folder,filename), p, s = 1, w=5, h=3)

    unipark <- data.frame(paste0(j,';',j,';<img src="http://ww2.unipark.de/uc/rrc/images/Stocks/', filename, '" width="500" alt="Aktienrendite" title="Aktienrendite" border="0">;;;;;4'))

    write.table(unipark, file.path(folder,"uniparkImport.txt"), append = j>1, row.names=F, col.names=F, quote = F)
})






uniparkorder <- data.table(
    firm=firms,
    uniparkorder = 1:19,
    key = "firm")

d <- d[uniparkorder]

write.table(d, "../4-Data/data_stocks_study1.csv", sep=",", row.names=F)

# Correlations between daily, yearly, mothly ROI
# correlations <- melt(d[, list(
#     dayMonth = cor(ROIdaily,ROImonthly,use="pairwise.complete.obs"),
#     dayYear = cor(ROIdaily,ROIyearly,use="pairwise.complete.obs"),
#     monthYear = cor(ROImonthly,ROIyearly,use="pairwise.complete.obs")
#     ),
#     by=firm], id.var="firm", variable.name = "type", value.name = "correlation")

# ggplot(correlations, aes(x=type,y=correlation,color=firm)) +geom_point()
# ggsavejj("correlations")


# Risk Return Correlation
for (i in intervalls)
{    
    ggplot(d[, list(EV = mean(get(i), na.rm=TRUE), Risk = sd(get(i), na.rm=TRUE)), by = firm]
        , aes(x=EV, y=Risk))  +
    geom_point() +
    stat_smooth(method = "lm", col = "blue", alpha = .1) +
    theme(axis.text=element_text(angle=90)) +ggtitle(paste("RRC",i))
ggsavejj(paste0("RRC",i), s = 1.1)
}