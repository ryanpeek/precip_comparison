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
df <- FITfileR::readFitFile(most_rec$path[1])
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
  
# look at average
df_sf <- df_sf %>% 
  mutate(hr_60 = zoo::rollmean(heart_rate, k = 60, fill = NA),
         hr_zone = cut(heart_rate, breaks = seq(60,180,10)))

# look at HR
library(ggplot2)
ggplot(df_sf) + 
  geom_point(aes(x = timestamp, y = heart_rate, fill = hr_zone), 
             pch=21, color="gray40", alpha=0.8, size=3) +
  scale_fill_viridis_d("HR Zone", option = "A") +
  #scale_fill_brewer(palette = "PRGn") +
  geom_line(aes(x=timestamp, y=hr_60), color="gray80", lwd=1.5, alpha=0.8) +
  theme_classic()
