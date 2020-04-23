library(dplyr)
library(tidyr)
library(data.table) # package for cool data processing
library(pdfetch)

index_abbreviations <- matrix( c(         
     "ASX"          , "^AORD",    8313426, "INDEXASX:XAO",
     "BSE"          , "^BSESN",   8313421, "INDEXBOM:SENSEX",
     "DAXK"         , "^GDAXIP",  1966970, "INDEXDB:DAXK",        
     "DJIA"         , "^DJI",     324977 , "NYSEARCA:DIA",   
     "EuroStoxx50"  , "XSSX.MI",  193736 , "TLV:INDEU.11" ,      
     "FSSTI"        , "^STI",     8313425, "INDEXFTSE:WISGP",
     "FTSE100"      , "^FTSE",    1918069, "INDEXFTSE:UKX",     
     "HSI"          , "^HSI",     8313314, "INDEXHANGSENG:HSI",     
     "IDX"          , NA,         8313316, "NYSEARCA:IDX",
     "ISE100"       , "XU100.IS", 8313320, "INDEXIST:XU100",         
     "JSE"          , NA,         8313322, NA,
     "KLSE"         , "^KLSE",    29831406, "KLSE:0820EA",      
     "MERV"         , "^MERV",    8384721,  "BCBA:IMV",   
     "Nikkei225"    , "^N225",    60972397, "INDEXNIKKEI:NI225",     
     "OBX"          , "OBX.OL",   15321315, "STO:DNBNOR-OBXO",      
     "OMXS30"       , "^OMX",     248271,   "INDEXNASDAQ:OMXS30",   
     "RTSI"         , "RTS.RS",   8313419,  "MCX:RTSI",     
     "SMI20"        , "^SSMI",    1555183,  "INDEXSWX:SMI",   
     "SSEC"         , "^SSEC",    16440923, "SHA:000001",     
     "TSX"          , "^GSPTSE",  8384722,  "INDEXTSI:OSPTX",      
     "WIG20"        , NA,         7907746,  "WSE:WIG20",
      "MXX"         , NA,         8313319,   NA),
     nrow = 22, byrow = T, dimnames = list(NULL,c("original", "yahoo","onvista", "google")))
index_abbreviations
write.table(index_abbreviations, "../../data/raw/nternational_acronyms.csv", row.names=FALSE)

### Get data from YAHOO
dt <- pdfetch_YAHOO(na.omit(index_abbreviations[, "yahoo"]), fields = c("open", "high", "low", "close"), from = start, to = end, interval = "1d")

# Reformate it to long format
longdt <- data.table(index(dt), stack(as.data.frame(coredata(dt))))
longdt <- as.data.table(longdt %>%
  separate(ind, c("yahoo"), sep = "[.]close|[.]open|[.]high|[.]low", extra = "drop", remove = FALSE))
longdt <- as.data.table(longdt %>%
  separate(ind, c("tmp", "tmp2", "column"), sep = "[.]", extra = "merge", fill = "left"))
longdt[, c("tmp", "tmp2")  := NULL]
dt <- dcast(longdt, V1 + yahoo ~ column, value.var = "values")
setnames(dt, "V1", "date")

# Rename the yahoo ids to standard index names
rename <- function(name, n, mat = index_abbreviations) {
    idx <- apply(mat[,n,drop=F], 2, function(x) which(x==name))
    return(unname(mat[idx,1]))
}
rename <- Vectorize(rename, vectorize.args = "name")


dt[, index := rename(yahoo)]

# make column names stanardized
setnames(dt, c("date", "yahoo_id", "end", "high", "low", "start", "index"))

# Check
yahoo[, as.list(range(year(date[!is.na(open)]))), by = index]

# save
write.table(dt, "../4-Data/internatinal_stocks_yahoo.csv", sep = ";", row.names = F)


dt[, as.list(range(date[!is.na(close)])), by = index]
dt[complete.cases(dt) & open==close & close==high & high==low, .N, by=index]


## Get data from ONVISTA
fetch_ONVISTA <- function(notationId)
{
    tmp = data.table()
    tmp <- lapply(seq(1990,2017,5), function(y)
    {
        url_to_csv <- paste0("https://www.onvista.de/onvista/boxes/historicalquote/export.csv?notationId=", notationId, "&dateStart=01.01.", y, "&interval=Y5")
        tmp_new <- fread(url_to_csv, select = 1:5, col.names = c("date","start","high","low","end"))
        return(tmp_new)
    })
    tmp <- rbindlist(tmp)
    tmp[, onvista := notationId]
    return(tmp)
}

onvista <- fetch_ONVISTA("8313319")

onvista <- lapply(index_abbreviations[, "onvista"], fetch_ONVISTA)
onvista <- rbindlist(onvista)
# Rename index to original name
onvista[, index := rename(onvista, 3)]
onvista[, date := as.Date(date, format = "%d.%m.%Y")]
# char to numeric values
onvista[, c("start","end","high","low") := lapply(.SD, function(x) as.numeric(gsub(",",".",gsub("\\.","",x)))), .SDcols = c("start","end","high","low")]
# Checks
onvista[, as.list(range(year(date[!is.na(open)]))), by = index]
onvista[start==end & end==high & high==low, .N, by = index]
# Save
write.table(onvista, "../4-Data/ipv_onvista.csv", sep = ";", row.names = F)
write.table(onvista, "../4-Data/internatinal_stocks_onvista.csv", sep = ";", row.names = F)


# Get data from google
library(rvest)
fetch_GOOGLE <- function(id)
{
    if(is.na(id)) return(NA)
    URL = paste0("https://www.google.com/finance/historical?q=", id,"&startdate=Jan%201%2C%201990&enddate=Aug%201%2C%202017&num=200&start=")
    # initialize
    moredata <- TRUE
    start <- 0
    dt <- data.table()
    # get data from html
    while(moredata)
    {
        webpage <- read_html(paste0(URL, start))
        webtables <- html_nodes(webpage, "table")
        if(length(webtables) >= 4) {
            tmp <- as.data.table(html_table(webtables[[4]]))
            dt <- rbind(dt, tmp)
            moredata <- nrow(tmp) == 200
            start <- start + 200            
        } else {
            break
        }
    }

    if (nrow(dt)==0) return(dt)

    setnames(dt, c("date", "start", "high", "low", "end", "volume"))
    dt[, volume := NULL]
    dt[, google_id := id]
    mylocale <- Sys.getlocale
    Sys.setlocale("LC_TIME", "English")
    dt[, date := as.Date(date, format = "%b %d, %Y")]
    cols <- c("start","high","low","end")
    dt[, (cols) := lapply(.SD, gsub, pattern = ",", replacement = ""), .SDcols = cols]
    dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
    return(dt[])
}
google <- lapply(index_abbreviations[, "google"], fetch_GOOGLE)
google <- rbindlist(google)
google[, index := NULL]
google[, index := rename(google_id, 4)]
# Checks
google[, as.list(range(year(date[!is.na(open)]))), by = index]
google[start==end & end==high & high==low, .N, by = index]
# Save
write.table(google, "../4-Data/internatinal_stocks_google.csv", sep = ";", row.names = F)


# # Sanity checks: are the data equal
# library(scales)
# check <- merge(yahoo, onvista, by = c("index", "date"), all = T, suffix = c(".yahoo", ".onvista"))
# check <- check[, .(abs(start.yahoo-start.onvista) >1), by = index]
# check[, .(yahoo.onvista=percent(mean(V1, na.rm=T)), sum(!is.na(V1))), by = index]

# # Merge data after error checking
# mdata <- merge(yahoo,onvista,by=c("date","index"),suffix=c("_yahoo","_onvista"), all=T)
# mdata[, .N, by = index]
# errors <- mdata[, start_onvista == end_onvista & end_onvista == high_onvista & high_onvista == low_onvista & start_yahoo == end_yahoo & end_yahoo == high_yahoo & high_yahoo == low_yahoo]
# errors_or_nas <- errors | mdata[, is.na(start_yahoo) & is.na(end_yahoo) & is.na(start_onvista) & is.na(end_onvista)]

# mdata[, sum(start_onvista == end_onvista & end_onvista == high_onvista & high_onvista == low_onvista & start_yahoo == end_yahoo & end_yahoo == high_yahoo & high_yahoo == low_yahoo, na.rm=T), by = index]
# mdata[!errors_or_nas, as.list(range(year(date))), by = index]


# # Get data from finanzen.net

# thePage <- readLines("http://www.finanzen.net/index/OMX/Historisch")
# begin_str <- "<table class=\"table\"><colgroup><col class=\"col-date-extended\"><col><col><col><col></colgroup><tr><th>Datum</th><th>Er&ouml;ffnung</th><th>Schluss</th><th>Tageshoch</th><th>Tagestief</th></tr>"
# end_str <- 
# grep(begin_str, thePage)
# head(thePage[28])



# fread("http://www.google.com/finance/historical?q=NASDAQ:ADBE&startdate=Jan+01%2C+2009&enddate=Aug+2%2C+2012&output=csv")

# fread("http://www.google.com/finance/historical?q=INDEXDB:DAXK&startdate=Jan+01%2C+2009&enddate=Aug+2%2C+2012&output=csv")



# library(XML)
# library(RCurl)
# library(rlist)
# tables <- readHTMLTable("https://www.onvista.de/onvista/boxes/historicalquote/export.csv?notationId=8313426&dateStart=01.01.1990&interval=Y5")

# tables <- list.clean(tables, fun = is.null, recursive = FALSE)
# tables
# n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# tables[[which.max(n.rows)]]


# library(rvest)
# url <- "https://www.onvista.de/onvista/boxes/historicalquote/export.csv?notationId=8313426&dateStart=01.01.1990&interval=Y5"
# population <- url %>%
#   read_html() %>%
#   html_nodes(xpath='//*[@id="historic-price-list"]/table[1]') %>%
#   html_table()
# population
# population <- population[[1]]
# population





# getSymbols("INDEXSTOXX:SX5E", src = "google")


# as.data.table(XSSX.MI)


# library(openxlsx) # package to read data from xlxs
# library(fasttime)
# out <- "../3-Experiment/Stimuli - Experiment 2/Stocks" # output folder

# janinedata <- fread("../4-Data/data_janine.csv")
# janadata <- fread("../4-Data/data_jana.csv")

# # Recode dates for merging
# janinedata[, date := as.Date(date)]
# janadata[, date := as.Date(date)]
# # janadata[index=="DAXK", end := gsub("x",".",gsub("\\.","",gsub(",","x",end)))]
# # janadata[, end := as.numeric(end)]

# # janadata[index=="DAXK", low := gsub("x",".",gsub("\\.","",gsub(",","x",low)))]
# # janadata[, low := as.numeric(low)]

# # janadata[index=="DAXK", high := gsub("x",".",gsub("\\.","",gsub(",","x",high)))]
# # janadata[, high := as.numeric(high)]

# # janadata[index=="DAXK", start := gsub("x",".",gsub("\\.","",gsub(",","x",start)))]
# # janadata[, start := as.numeric(start)]
# # write.table(janadata, "../4-Data/data_jana.csv", sep = ";", row.names = F)


# # Initial checks: null variance in a day's data?
# janadata[start==end & end==high & high==low, .N, by=index] # 1 and 3 cases: OK
# janinedata[start==end & end==high & high==low, .N, by=index] # 2 to 17 cases: OK

# # Merge data
# setkey(janadata, index, date)
# setkey(janinedata, index, date)
# alldata <- merge(janadata, janinedata, all = T, suffixes = c("jana", "janine"))
# alldata[, index :=
#     ifelse(index=="DAX", "DAXK",
#     ifelse(index=="DowJones","DJAI",
#     ifelse(index=="Merval", "MERV",
#     ifelse(index=="JakartaStockExchangeComposite","IDX",
#     ifelse(index=="OMXStockholm30","OMXS30",index)))))]
# alldata[is.na(endjana) & !is.na(endjanine), endjana := endjanine]
# alldata[is.na(endjana) & !is.na(endjanine), valorjana := valorjanine]
# alldata[is.na(endjana) & !is.na(endjanine), isinjana := isinjanine]
# alldata[is.na(endjana) & !is.na(endjanine), sourcelinkjana := sourcelinkjanine]
# alldata[is.na(endjanine) & !is.na(endjana), endjanine := endjana]
# alldata[is.na(endjanine) & !is.na(endjana), valorjanine := valorjana]
# alldata[is.na(endjanine) & !is.na(endjana), isinjanine := isinjana]
# alldata[is.na(endjanine) & !is.na(endjana), sourcelinkjanine := sourcelinkjana]

# # Checks
# alldata[, length(unique(index)) ] # 21 indices
# alldata[, min(date), by=index] # Jan 2009 except a few
# alldata[index=="FTSE100", all.equal(endjana, endjanine, tol = .001)] # TRUE
# alldata[index=="OMXStockholm30", all.equal(endjana, endjanine, tol = .001)] # TRUE

# alldata[, end := endjana]
# alldata[, sourcelink := ifelse(is.na(sourcelinkjana), sourcelinkjanine, sourcelinkjana)]
# alldata[, isin := ifelse(is.na(isinjana), isinjanine, isinjana)]
# alldata[, valor := ifelse(is.na(valorjana), valorjanine, valorjana)]

# write.table(alldata[, c("index","date","end","sourcelink","valor","isin","wkn"), with = F], sep=";", "../4-Data/international_stocks.csv", row.names = F)


# #######################################################################
# # Calculate the ROI
library(lubridate) # for working with date/time nicely
library(fasttime) # for fast conversion into time-format

# # load data
d <- fread("../../data/study2_stocks/international_stocks.csv")

# reformat variables
d[, date := as.Date(date)] # char to time format

# # Calculate ROI for the specified value between each date in dateVector and the targetdate. If the targetdate is not a sales date (i.e. does not exist in dateVector) one day later than targetdate is used as sales date, if that does not exist, another day later is used, etc.
getValue <- function(value, dateVector, targetdate)
{
    # returns the value 'value' on the day targetdate. If dateVector contains the targetdate, value at the targetdate is returned. If targetdate does not exist in dateVector (targetdate is a Sunday or holiday), then return the closest value for the next AFTER targetdate in dateVector.

    dateVector <- trunc(dateVector, "day")
    targetdate <- trunc(targetdate, "day")

    if (max(dateVector) < targetdate)
    {
        return(NA)
    }

    if (sum(dateVector == targetdate) == 0)
    {
        out <- getValue(value, dateVector, targetdate = as.Date(targetdate) + days(1))
    }
    else {
        out <- value[dateVector == targetdate]        
    }
    return(out)
}


getROI <- function(buy, sell)
{
    out <- (sell - buy) / buy
    return(out)
}
getROI <- Vectorize(getROI)

# Time difference
d[, plus1year := date %m+% years(1), by = date]
nrow(d[end<0]) # 0

# Checks
d[, as.list(range(plus1year)), by = index]

# ROI
d[, ROIyearly := getROI(end, sapply(plus1year, function(x) getValue(end, as.Date(date), x))), by = index]

# Save
write.table(d, file = "../../study2_stocks/_roi.csv", row.names=FALSE, sep=";")

######################################################################