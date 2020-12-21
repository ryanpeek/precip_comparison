## WUNDERGROUND DAILY DATA FROM (from Dylan, http://casoilresource.lawr.ucdavis.edu/drupal/node/991)
## see 'download_wunderground.r' script

#   TUO: KCAGROVE6, MTS026, MD2740
#   COLFAX/NFA: KCAGOLDR3, KCAWEIMA5, 
#   MFA: KCACOOL6
#   MFY: KCAALLEG2
#   RUB/RALSTON: KCAFORES14
#   FORESTHILL: KCAFORES6, KCAFORES12, KCAFORES21, KCAFORES22
#   MALAKOFF: MNVYC1
#   NFY: Saddleback, CA Headwaters of Goodyear Ck: MSLEC1
#   DAV: KCADAVIS24, KCADAVIS17
#https://www.wunderground.com/weatherstation/WXDailyHistory.asp?ID=KCAANGEL4&month=5&day=1&year=2011&format=1

wunder_daily <- function(site, station, date, save=FALSE)
{
  
  # load packages
  if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
  if(!require(lubridate)) { install.packages("lubridate"); require(lubridate)}
  
  # get base web address
  base_url <- 'https://www.wunderground.com/weatherstation/WXDailyHistory.asp?'

  # parse date
  m <- as.integer(month(date))
  d <- as.integer(day(date))
  y <- year(date)
  
  # compose final url
  final_url <- paste(base_url,
                     'ID=', station,
                     '&month=', m,
                     '&day=', d, 
                     '&year=', y,
                     '&format=1', sep='')
  
  # reading in as raw lines from the web server
  # contains <br> tags on every other line
  u <- url(final_url)
  the_data <- readLines(u)
  close(u)
  
  # only keep records with more than 5 rows of data
  if(length(the_data) > 5 )
  {
    # remove the first and last lines
    the_data <- the_data[-c(1, length(the_data))]
    
    # remove odd numbers starting from 3 --> end
    the_data <- the_data[-seq(3, length(the_data), by=2)]
    
    # extract header and cleanup
    the_header <- the_data[1]
    the_header <- make.names(strsplit(the_header, ',')[[1]])
    
    # convert to CSV, without header
    tC <- textConnection(paste(the_data, collapse='\n'))
    the_data <- read.csv(tC, as.is=TRUE, row.names=NULL, header=FALSE, skip=1)
    close(tC)
    
    # remove the last column, created by trailing comma
    the_data <- the_data[, -ncol(the_data)]
    
    # assign column names
    names(the_data) <- the_header
    
    # convert Time column into properly encoded date time
    the_data$Time <- as.POSIXct(strptime(the_data$Time, format='%Y-%m-%d %H:%M:%S'))
    
    # remove UTC and software type columns
    the_data$DateUTC.br. <- NULL
    the_data$SoftwareType <- NULL
    
    # sort and fix rownames
    the_data <- the_data[order(the_data$Time), ]
    row.names(the_data) <- 1:nrow(the_data)
    
    # Add a station ID col and SITE
    the_data$station <- station
    the_data$site <- site
    # add time zone
    the_data$Time<-lubridate::with_tz(the_data$Time, tzone="America/Los_Angeles")
    the_data$timesnap<-round_date(the_data$Time, unit = "15 mins") # to snap to nearest 15 min
    the_data<-arrange(the_data, Time)
    
    # done
    return(the_data)
  }
  
  d1<-paste0(site,"_",station)
  assign(d1,the_data, envir=.GlobalEnv) # print to workspace
  
  if(save){
    # save to file
    save(the_data, file=paste0("data/wunderground/",
                               site, "_",station,"_",min(year(the_data$Time)),
                               "-",max(year(the_data$Time)),".rda"))
    cat("Data saved here:", "\n", getwd(),"/data/wunderground")
  }
  
}

