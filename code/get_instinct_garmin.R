# get garmin gpx and plot

library(sf)
library(fs)
library(here)
library(glue)
library(dplyr)
library(lubridate)
library(mapview)
mapviewOptions(fgb = FALSE)

# package to interpret fit files
#remotes::install_github("grimbough/FITfileR")
library(FITfileR)

# Get Data ----------------------------------------------------------------

# instinct activity
instinct_dir <- "/Volumes/GARMIN/Garmin/Activity/"
fs::dir_exists(instinct_dir)

# ls files
fs::dir_ls(instinct_dir, glob="*.fit")

# get most recent
most_rec <- fs::dir_info(instinct_dir, glob="*.fit") %>% 
  arrange(desc(modification_time)) %>% 
  select(path, size, modification_time, birth_time) %>% 
  slice_max(modification_time, n=2)
  # slice_head(n=2)

# read in
df <- FITfileR::readFitFile(most_rec$path[2])
df

# get loc/timestamps
df_recs <- records(df)
# see lists of records
vapply(df_recs, FUN = nrow, FUN.VALUE = integer(1))

df_all <- records(df) %>% bind_rows() %>% 
  arrange(timestamp) %>% 
  # drop nas
  filter(!is.na(position_lat)) %>% 
  # make PST
  mutate(timestamp = with_tz(timestamp, tzone="America/Los_Angeles"))

# make sf
df_sf <- df_all %>% 
  st_as_sf(coords=c("position_long", "position_lat"), remove=FALSE, crs=4326) %>% 
  # convert to miles
  mutate(distance_mi = round((distance * 3.28084)/5280, 2),
         elev_ft = round(enhanced_altitude*3.28084, 2))

mapview(df_sf, zcol="distance_mi", layer.name="Distance (mi)") +
  mapview(df_sf, zcol="enhanced_speed", layer.name="Speed") 
  


