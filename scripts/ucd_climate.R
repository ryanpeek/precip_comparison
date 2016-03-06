# GET UCD CLIMATE 

# climate data from UC Climate (http://169.237.140.1/calludt.cgi/WXDATAREPORT)
# http://apps.atm.ucdavis.edu/wxdata/data/

library(lubridate); library(dplyr); library(ggplot2); library(stringr)

# get historical climate data
dav <- read.csv("./data/climate_Davis_historical.csv", skip = 53) %>% 
  select(Station:min,Soil.max, min.1) %>% 
  filter(!is.na(Air.max))

# need to split out date/time
dav$Time<-ifelse(nchar(dav$Time)<4,str_pad(dav$Time, side = "left", width = 4, pad="0"),  dav$Time)
dav$Time<-paste0(dav$Time,"00") # add zeros to make HMS
dav$datetime<-ymd_hms(paste0(dav$Date, dav$Time))
dav$Y<-year(dav$datetime)
dav$M<-month(dav$datetime)
dav$DOY<-yday(dav$datetime)


dav2 <- dav %>% 
  select(datetime, Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M, Station) %>% 
  summarize("totPPT"=sum(Precip),
            "avgAir.max"=mean(Air.max))


ggplot(data = dav2, aes(x = Y, y = totPPT, group=Station)) +
  geom_jitter(alpha = 0.3, color = "blue3") +
  geom_violin(fill="blue", alpha=0.7) + theme_bw() + facet_grid(.~M)


ggplot(data = dav2, aes(x = Y, y = avgAir.max, group=Station)) +
  geom_jitter(alpha = 0.3, color = "blue3") +
  geom_violin(fill="blue", alpha=0.7) + theme_bw() + facet_grid(.~M)
