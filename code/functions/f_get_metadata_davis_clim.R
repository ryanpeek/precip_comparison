# get metadata for sensors

library(readr)
library(dplyr)
library(glue)

# get metadata
f_get_metadata_davis_clim <- function(outdir="data_raw"){
  metadat <- read_delim("http://apps.atm.ucdavis.edu/wxdata/metadata/sensor_info_by_id.txt", trim_ws = TRUE,
                        delim="|", skip = 3,
                        col_names = c("x1", "sensor_id", "metric_id",
                                      "station_id", "metric_name",
                                      "metric_units", "x2")) %>% 
    select(-(starts_with("x"))) %>% 
    filter(!is.na(sensor_id))
  write_csv(metadat, glue("{outdir}/sensor_info_by_id.csv"))
}
f_get_metadata_davis_clim()
