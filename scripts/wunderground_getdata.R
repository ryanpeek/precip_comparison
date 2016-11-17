## Pull Wunderground Data:
## 2016-11-15 R. Peek

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

# PICK STATION AND DATES --------------------------------------------------

## These will be appended on to file names
site <- 'RUB' # site name
station<-'KCAFORES14' # station name here
start<-'2016-11-05' # this took user: 87.844 system: 11.139 elapsed: 2565.785
end<-'2016-11-15'

# SCRAPE WUNDERGROUND DATA ------------------------------------------------

## Using `purrr` framework
library(purrr, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(lubridate, warn.conflicts = F)

ptm <- proc.time() # total time to run
wdat <- map_df(seq(as.Date(start), as.Date(end), "1 day"),
              function(x) { wunder_daily(station = station, date = x) })
proc.time() - ptm # total time that has elapsed


# FOR LARGE FILES: Write Compressed Files ---------------------------------

# write to a compressed file
library(readr, warn.conflicts = F)
readr::write_rds(x = wdat, path = paste0("data/wunderground/",site,"_",station,"_",start,".csv.xz"), compress="xz")

wdat<-read_rds(path = "data/wunderground/RUB_KCAFORES14_2016-11-05.csv.xz")

# WUNDER CLEAN & PLOT FUNCTION --------------------------------------------

# use `wunder_clean`` function to combine list and clean data
wunder_clean(data = wdat, # data
             saveHrly = TRUE, # save csv of hourly only
             saveDaily = TRUE, # save csv of daily only
             save15 =  TRUE # save full dataset
             # default will still output all 3 to global environment
            )

# save as RData file (can combine all three above into one .rda file)
save(list = ls(pattern = "KCA*"), file = paste0("data/wunderground/",site,"_",station,"_",start,".rda"), compress="xz")

# remove values/files
rm(l,i, start, end, date.range) 
