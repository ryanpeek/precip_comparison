## Pull Wunderground Data:
## 2015-12-29 R. Peek

# Set up function to pull data from wunderground at a sub hourly timestamp
# aggregate to hourly or daily and plot/save data

# LOAD FUNCTIONS ----------------------------------------------------------

source("scripts/functions/wunderground_daily.R") # scrape data
source("scripts/functions/wunderground_clean.R") # clean data

# DEFINE STATIONS ---------------------------------------------------------

#   TUO: KCAGROVE6,KCAGROVE11[2013,2015], KCAGROVE19[2015], MTS026, MD2740
#   NFA/COLFAX: KCAGOLDR3, KCAWEIMA5, 
#   MFA: KCACOOL6
#   MFY: KCAALLEG2
#   RUB/RALSTON: KCAFORES14
#   FORESTHILL: KCAFORES6,KCAFORES9 (from 2008-Feb), KCAFORES12, KCAFORES21, KCAFORES22
#   MALAKOFF: MNVYC1
#   NFY: Saddleback, CA Headwaters of Goodyear Ck: MSLEC1
#   STAN: MSPWC1  
#   DAVIS: KCADAVIS17 (slide hill park)

# SCRAPE WUNDERGROUND DATA ------------------------------------------------
site <- 'DAVIS'
station<-'KCADAVIS17' # station name here
start<-'2013-01-01' 
end<-'2016-11-15'

# create vector of dates: daily
date.range <- seq.Date(from=as.Date(start), to=as.Date(end), by='1 day')

# create vector of dates: annually
#date.range <- seq.Date(from=as.Date(start), to=as.Date(end), by='1 year')

# make a list based on the date.range
l <- vector(mode='list', length=length(date.range)) # pre-allocate list

# use wunderground_daily function to loop through dates
for(i in seq_along(date.range)) {
  print(date.range[i])
  l[[i]] <- wunder_daily(station, date.range[i])
  l[[i]]$station <- station # add station
}

### output is a list of dataframes, each frame is daily data
### if you get "Error in `*tmp*`[[i]] : subscript out of bounds"
### it just means there is not data available for a given day(s), script will
### still run.

# WUNDER CLEAN & PLOT FUNCTION --------------------------------------------

# use wunderground_clean function to combine list and clean data
wunder_clean(data = l, # data
             interval = 15, # minutes
             saveHrly = FALSE, # save csv of hourly only
             saveDaily = FALSE, # save csv of daily only
             saveALL =  FALSE # save full dataset
             # default will still output all 3 to global environment
            )

# save as RData file (can combine all three above into one .rda file)
save(list = ls(pattern = "KCA*"), file = paste0("data/wunderground/",site,"_",station,"_2016.rda"))
readr::write_rds(paste0(site,"_15"), path=paste0("data/wunderground/",site,"_",station, "_2016.rds"))

# remove values/files
rm(l,i, start, end, date.range) 


# USING REVISED SCRIPT PLUS PURRR -----------------------------------------

library(purrr)

rng <- map_df(seq(as.Date("2015-12-01"), as.Date("2015-12-04"), "1 day"),
              function(x) { get_wx(wx_date=x) })
