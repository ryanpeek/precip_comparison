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

dav1 <- dav %>% 
  select(datetime, Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M, Station) %>% 
  filter(M==2) # limit to FEB 
  
dav2 <- dav %>% 
  select(datetime, Station, Precip:min.1, Y, M, DOY) %>% 
  group_by(Y, M, Station) %>% 
  filter(M==2) %>% # limit to FEB 
  summarize("totPPT_mm"=sum(Precip),
            "avgAir.max"=mean(Air.max))
glimpse(dav1)

# precip in FEB
ggplot(data = dav2, aes(x = Y, y = totPPT, group=Station)) +
  geom_point(data= dav2, aes(x=Y, y=totPPT, group=Station), alpha= 0.5, color="red2")+
  geom_point(data=dav2[dav2$Y==2016,], aes(x=Y, y=totPPT, group=Station), fill="blue", alpha=0.7) + theme_bw() #+ facet_grid(.~M)

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
h(dav2)


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
