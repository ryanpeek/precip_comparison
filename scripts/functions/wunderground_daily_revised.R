# Wunderground Function update:

#' @param station station name
#' @param sta_type station type "airport" or "weatherstation"
#' @param wx_date Date object or character string 
#' @param fmt if wx_date is not a Date object and the character string
#'        is not in "%Y-%m-%d" format, then specify the format here
#' @return data.frame of readings
get_wx <- function(station="EKAH", sta_type="airport", wx_date=Sys.Date(), fmt="%Y-%m-%d") {
  
  require(httr)
  require(readr)
  
  if (inherits(wx_date, "character")) {
    wx_date <- as.Date(wx_date, fmt)
  }
  
  wx_base_url <- paste0("https://www.wunderground.com/history/", sta_type, "/%s/%s/DailyHistory.html")
  wx_url <- sprintf(wx_base_url, station, format(wx_date, "%Y/%m/%d"))
  
  res <- httr::GET(wx_url, query=list(MR=1, format=1))
  dat <- httr::content(res, as="text")
  
  dat <- gsub("<br />", "", dat)
  dat <- read.table(text=dat, sep=",", header=TRUE,
                    na.strings=c("-", "N/A", "NA"), stringsAsFactors=FALSE)
  
  # saner column names
  
  cols <- colnames(dat)
  
  # via http://stackoverflow.com/a/22528880/1457051
  cols <- gsub("([a-z])([A-Z])", "\\1_\\L\\2", cols, perl=TRUE)
  cols <- sub("^(_[a-z])", "\\L\\1", cols, perl=TRUE)
  cols <- tolower(gsub("\\.", "_", cols))
  
  readr::type_convert(setNames(dat, cols)) # more robust than type.convert()
  
}

tdy <- get_wx()

str(tdy)
## 'data.frame': 36 obs. of  14 variables:
##  $ time_cest            : chr  "12:00 AM" "12:20 AM" "12:50 AM" "1:00 AM" ...
##  $ temperature_f        : num  51 50 48.2 47 46.4 44.6 44 44.6 44.6 44 ...
##  $ dew_point_f          : num  41 41 39.2 39 39.2 39.2 38 39.2 39.2 38 ...
##  $ humidity             : int  60 71 71 67 76 81 71 81 81 73 ...
##  $ sea_level_pressure_in: num  30.1 30.1 30.1 30.1 30.1 ...
##  $ visibility_mph       : num  28 6.2 6.2 28 6.2 6.2 7 6.2 6.2 28 ...
##  $ wind_direction       : chr  "WNW" "West" "West" "West" ...
##  $ wind_speed_mph       : chr  "2.3" "2.3" "2.3" "2.3" ...
##  $ gust_speed_mph       : logi  NA NA NA NA NA NA ...
##  $ precipitation_in     : logi  NA NA NA NA NA NA ...
##  $ events               : logi  NA NA NA NA NA NA ...
##  $ conditions           : chr  NA "Unknown" "Unknown" NA ...
##  $ wind_dir_degrees     : int  300 270 270 270 270 270 270 280 280 270 ...
##  $ date_utc             : POSIXct, format: "2016-06-08 22:00:00" "2016-06-08 22:20:00" ...

a_yr_ago <- get_wx(wx_date="2015-06-09")

a_yr_ago <- get_wx(station = "KCADAVIS17", sta_type = "weatherstation", wx_date="2015-06-09")

str(a_yr_ago)
