# GET UCD CLIMATE 

# pull data from UC Davis site:
# climate data from UC Climate (http://ipm.ucanr.edu/WEATHER/index.html)

# Archived Data: http://apps.atm.ucdavis.edu/wxdata/data/


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate) 
library(stringr)
library(glue)
library(janitor)


# Get Data ----------------------------------------------------------------

# get historical climate data
dav <- read_csv("data/climate_Davis_historical_1970_2020.csv", skip = 55) %>% 
  clean_names() %>% 
  # select cols
  select(station:precip, air_max:min, evap, solar) %>% 
  # drop one NA
  filter(!is.na(date))

#summary(dav)


# Clean Data --------------------------------------------------------------

# need to clean and fix time & date
dav <- dav %>% 
  mutate(time = ifelse(nchar(time) < 4, str_pad(time, side = "left", width = 4, pad="0"),  time),
         datetime = ymd_hm(paste0(as.character(date), time)), .after=date,
         M = month(datetime))

# add DOY
dav <- dav %>% wateRshedTools::add_WYD("datetime")

# add decade
dav$decade <- cut(x = dav$WY, 
                  include.lowest = T, dig.lab = 4, 
                  breaks = c(1969, 1979, 1989, 1999, 2009, 2019),
                  labels = c('1970s', '1980s','1990s','2000s','2010s'))

# clean 
dav_feb <- dav %>% 
  group_by(WY, M) %>% 
  filter(M==2) %>% # limit to FEB 
  summarize("totPPT_mm"=sum(precip),
            "avgPPT_mm"=mean(precip),
            "maxPPT_mm"=max(precip),
            "minPPT_mm"=min(precip),
            "avgAir_max"=mean(air_max),
            "maxAir_max"=max(air_max),
            "avgAir_min"=mean(min),
            "minAir_min"=min(min))

# add decade
dav_feb$decade <- cut(x = dav_feb$WY, 
                  include.lowest = T, dig.lab = 4, 
                  breaks = c(1969, 1979, 1989, 1999, 2009, 2019),
                  labels = c('1970s', '1980s','1990s','2000s','2010s'))


# Plot Feb Precip -------------------------------------------------------------

# precip in FEB
ggplot() +
  geom_crossbar(data=dav_feb, aes(x=WY, y=avgPPT_mm, ymax=maxPPT_mm, ymin=minPPT_mm, group=WY), alpha= 0.5, color="cyan4") +
  theme_bw() +
  facet_grid(M~.)

# precip plot
ggplot(data = dav_feb, aes(x = as.factor(WY), y = totPPT_mm, group=WY)) +
  geom_point(data=dav_feb[dav_feb$WY==2016,], aes(x=as.factor(WY), y=totPPT_mm, group=WY), color="maroon", alpha=0.8) +
  geom_smooth(data=dav_feb, aes(x=as.factor(WY), y= totPPT_mm, group=M), alpha=0.3) +
  geom_point(data=dav_feb, aes(x=as.factor(WY),y=totPPT_mm, group=WY), color="gray20", size=2.7)+
  geom_point(data=dav_feb[dav_feb$WY==2016,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="maroon", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==1983,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="maroon", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==1998,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="maroon", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==2017,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="maroon", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==1988,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="lightpink", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==1992,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="lightpink", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==2003,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="lightpink", size=4) +
  geom_point(data=dav_feb[dav_feb$WY==2010,], aes(x=as.factor(WY),y=totPPT_mm, group=WY), pch=21, fill="lightpink", size=4) +
  geom_text(data=dav_feb[dav_feb$WY==2016 | dav_feb$WY==1983 | dav_feb$WY==1988 | dav_feb$WY==1998 | dav_feb$WY==1992 | dav_feb$WY==2003 | dav_feb$WY==2010 | dav_feb$WY==2017,], 
             aes(label=WY, x=as.factor(WY), y=totPPT_mm), size=3, vjust = -0.90, nudge_y=-0.5, fontface = "bold")+
  ylab(paste("Total Precipitation (mm)")) + theme_bw() + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle(label = "February Precip (mm) in Davis: 1971-2020") + 
  annotate("text", x=as.factor(2008), y=290, size=2, label="Data Source: http://atm.ucdavis.edu/weather/")

ggsave(filename = "figs/Feb_ppt_Davis_1971-2020.png", width = 8, height=5, units = "in", dpi = 300)


# Plot Feb Airtemp --------------------------------------------------------

# airtemp in FEB
ggplot() +
  geom_point(data=dav_feb, aes(x=WY, y=avgAir_max, group=WY), color="maroon", alpha=0.8) +
  geom_ribbon(data=dav_feb, aes(x=WY, ymax=avgAir_max, ymin=avgAir_min, group=WY), color="gray20", alpha=0.7)+
  geom_smooth(data=dav_feb, aes(x=WY, y= avgAir_max)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  labs(title = "February Air Temperature in Davis: 1971-2020", caption="Data Source: http://atm.ucdavis.edu/weather/", 
       x="", y=expression(paste("Air Temp (", degree, "C)")))+
  theme_bw()

ggsave(filename = "figs/Feb_airtemp_Davis_1971-2020.png", width = 8, height=6, units = "in", dpi = 300)

# do some stats by decade to look for changes in ppt
library(coin) 
head(dav_feb)

# monte carlo sampling of precip vs decade using FEB only
oneway_test(totPPT_mm ~ decade,
            data=dav_feb, 
            distribution='approximate')

# monte carlo sampling of avgAir_max vs decade using FEB only
oneway_test(avgAir_max ~ decade, 
            data=dav_feb, 
            distribution='approximate')

