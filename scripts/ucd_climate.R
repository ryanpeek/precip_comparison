# GET UCD CLIMATE 

# climate data from UC Climate (http://169.237.140.1/calludt.cgi/WXDATAREPORT)
# http://apps.atm.ucdavis.edu/wxdata/data/

library(lubridate); library(dplyr); library(ggplot2); library(stringr)

# get historical climate data
dav <- read.csv("./data/climate_Davis_historical.csv", skip = 53)
dav %<>% 
  select(Station:min,Soil.max, min.1) %>% 
  filter(!is.na(Air.max))

# need to split out date/time
dav$Time<-ifelse(nchar(dav$Time)<4,str_pad(dav$Time, side = "left", width = 4, pad="0"),  dav$Time)
dav$Time<-paste0(dav$Time,"00") # add zeros to make HMS
dav$datetime<-ymd_hms(paste0(dav$Date, dav$Time))
dav$Y<-year(dav$datetime)
dav$M<-month(dav$datetime)
dav$DOY<-yday(dav$datetime)

dav1 <- dav %>% 
  select(datetime, Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M, Station) %>% 
  filter(M==2) # limit to FEB 
glimpse(dav1)
dav1$decade <- cut(x = dav1$Y,include.lowest = T,dig.lab = 4, breaks = c(1980, 1990, 2000, 2010, 2020), labels = c('1980s','1990s','2000s','2010s'))

  
dav2 <- dav %>% 
  select(datetime, Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M, Station) %>% 
  filter(M==2) %>% # limit to FEB 
  summarize("totPPT_mm"=sum(Precip),
            "avgAir.max"=mean(Air.max))

ppt <- dav %>% 
  select(datetime, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M) %>% 
  filter(M==2) %>% # limit to FEB 
  summarize("totPPT_mm"=sum(Precip),
            "avgPPT_mm"=mean(Precip),
            "maxPPT_mm"=max(Precip),
            "minPPT_mm"=min(Precip))

h(ppt)
ppt$decade <- cut(x = ppt$Y,include.lowest = T,dig.lab = 4, breaks = c(1980, 1990, 2000, 2010, 2020), labels = c('1980s','1990s','2000s','2010s'))
h(ppt)

# precip in FEB
ggplot(data = ppt, aes(x = as.factor(Y), y = avgPPT_mm, group=Y)) +
  geom_crossbar(data= ppt, aes(x=as.factor(Y), ymax=maxPPT_mm, ymin=minPPT_mm, group=as.factor(Y)), alpha= 0.5, color="red2")+
  #geom_point(data=ppt[ppt$Y==2016,], aes(x=as.factor(Y), y=maxPPT_mm, group=as.factor(Y)), fill="blue", alpha=0.7) + theme_bw() +
  facet_grid(M~.)

# precip plot
ggplot(data = ppt, aes(x = as.factor(Y), y = totPPT_mm, group=Y)) +
  geom_point(data=ppt[ppt$Y==2016,], aes(x=as.factor(Y), y=totPPT_mm, group=Y), color="maroon", alpha=0.8) +
  geom_smooth(data=ppt, aes(x=as.factor(Y), y= totPPT_mm, group=M), alpha=0.3) +
  geom_point(data=ppt, aes(x=as.factor(Y),y=totPPT_mm, group=Y), color="gray20", size=2.7)+
  geom_point(data=ppt[ppt$Y==2016,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="maroon", size=4) +
  geom_point(data=ppt[ppt$Y==1983,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="maroon", size=4) +
  geom_point(data=ppt[ppt$Y==1998,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="maroon", size=4) +
  geom_point(data=ppt[ppt$Y==1988,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="lightpink", size=4) +
  geom_point(data=ppt[ppt$Y==1992,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="lightpink", size=4) +
  geom_point(data=ppt[ppt$Y==2003,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="lightpink", size=4) +
  geom_point(data=ppt[ppt$Y==2010,], aes(x=as.factor(Y),y=totPPT_mm, group=Y), pch=21, fill="lightpink", size=4) +
  geom_text(data=ppt[ppt$Y==2016 | ppt$Y==1983 | ppt$Y==1988 | ppt$Y==1998 | ppt$Y==1992 | ppt$Y==2003 | ppt$Y==2010,], 
             aes(label=Y, x=as.factor(Y), y=totPPT_mm), size=3, vjust = -0.90, nudge_y=-0.5, fontface = "bold")+
  ylab(paste("Total Precipitation (mm)")) + theme_bw() + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle(label = "February Precip (mm) in Davis: 1981-2016") + 
  annotate("text", x=as.factor(2008), y=290, size=2, label="Data Source: http://atm.ucdavis.edu/weather/")

library(svglite) # much higher res and is vector scalable graphics
ggsave(filename = "./plots/Feb_ppt_Davis_1981-2016.svg", width = 8, height=5, units = "in", dpi = 150)


# airtemp in FEB
ggplot(data = dav1, aes(x = as.factor(Y), y = Air.max, group=Station)) +
  geom_jitter(data=dav1[dav1$Y==2016,], aes(x=as.factor(Y), y=Air.max, group=Y), color="maroon", alpha=0.8) +
  geom_boxplot(data= dav1, aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), color="gray20", alpha=0.7)+
  geom_boxplot(data=dav1[dav1$Y==2016,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="maroon", alpha=0.7) +
  geom_boxplot(data=dav1[dav1$Y==1983,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="maroon", alpha=0.7) +
  geom_boxplot(data=dav1[dav1$Y==1998,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="maroon", alpha=0.7) +
  geom_boxplot(data=dav1[dav1$Y==1988,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="lightpink", alpha=0.5) +
  geom_boxplot(data=dav1[dav1$Y==1992,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="lightpink", alpha=0.5) +
  geom_boxplot(data=dav1[dav1$Y==2003,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="lightpink", alpha=0.5) +
  geom_boxplot(data=dav1[dav1$Y==2010,], aes(x=as.factor(Y), ymax=Air.max, ymin=min, group=Y), fill="lightpink", alpha=0.5) +
  geom_smooth(data=dav1, aes(x=as.factor(Y), y= Air.max, group=Station)) +
  ylab(expression(paste("Air Temp (", degree, "C)"))) + theme_bw() + xlab("") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
  ggtitle(label = "February Air Temperature in Davis: 1981-2016") + 
  annotate("text", x=as.factor(2010), y=2, size=2, label="Data Source: http://atm.ucdavis.edu/weather/")

library(svglite) # much higher res and is vector scalable graphics
ggsave(filename = "./plots/Feb_airtemp_Davis_1981-2016.svg", width = 8, height=5, units = "in", dpi = 150)

# ggsave(filename = "./plots/Feb_airtemp_Davis_1981-2016.png", width = 6, height=5, units = "in", dpi = 150)

# lumped
ggplot(data = dav2, aes(x = Y, y = avgAir.max, group=Station)) +
  geom_jitter(alpha = 0.3, color = "blue3") +
  geom_boxplot(fill="blue", alpha=0.7) + theme_bw()

# air only for all but 2016, J-F-M
dav %>% 
  select(Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(M) %>% 
  filter(M<4 & Y<2016) %>% # limit to JAN-FEB 
  summarize(#"totPPT_mm"=sum(Precip),
            #"avgPPT_mm"=mean(Precip),
            "avgAir"=mean(c(Air.max,min)),
            "avgAirmax"=mean(Air.max),
            "avgAirmin"=mean(min)) %>% 
  as.data.frame()



# do some stats by decade to look for changes in ppt
library(coin) 
h(ppt)
h(dav1)

# monte carlo sampling of precip vs decade using FEB only
oneway_test(totPPT_mm ~ decade,data=ppt,distribution='approximate')

# monte carlo sampling of Air.max vs decade using FEB only
oneway_test(Air.max ~ decade,data=dav1[dav1$M==2,],distribution='approximate')
