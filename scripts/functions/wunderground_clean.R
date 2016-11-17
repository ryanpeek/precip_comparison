## Wunder_clean
## R. Peek 2015-Dec
## Function to clean data from wunderground download 
## from the wunder_daily script

# Run "wunder_daily" script first, then feed data into this script

wunder_clean <- function(data, 
                         saveHrly=TRUE, # save rda of hourly
                         saveDaily=TRUE, # save rda of daily
                         save15=TRUE) # save rda of all data
{
  
  
  # load packages
  if(!require(lubridate)) { install.packages("lubridate"); require(lubridate)}
  if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
  
  wdat<-data
  sta.ID<-unique(wdat$station)
  site.ID<-unique(wdat$site)
  
  # Add other columns for processing
  wdat$year<-year(wdat$Time)
  wdat$mon<-month(wdat$Time)
  wdat$yday<-yday(wdat$Time)
  wdat$hour<-hour(wdat$Time)
  wdat$Time<-lubridate::with_tz(wdat$Time, tzone="America/Los_Angeles") # convert to UTC/America/Los_Angeles
  
  # MAKE HOURLY/DAILY -------------------------------------------------------
  
  wdat.hr <- wdat %>%
    filter(TemperatureF > -999) %>% # get rid of bad temp data
    group_by(year, mon, yday, hour)%>%
    select(TemperatureF:PressureIn,WindDirectionDegrees:Humidity) %>% 
    summarize_each(funs(mean)) %>% 
    mutate("datetime"=ymd_hms(strptime(paste0(year,"-", mon,"-", yday, " ",
                                              hour,":00"),format = "%Y-%m-%j %H:%M"))) %>%
    select(datetime,year,mon,yday,hour,TemperatureF:Humidity) %>% 
    as.data.frame()

  # Make Daily dataset
  wdat.dy <- wdat %>%
    filter(TemperatureF > -999) %>% # get rid of bad temp data
    group_by(year, mon, yday)%>%
    select(TemperatureF:PressureIn,WindDirectionDegrees:Humidity) %>% 
    summarize_each(funs(mean,max,min)) %>% 
    mutate("date"=as.Date(strptime(paste0(year,"-", mon,"-", yday),format = "%Y-%m-%j"))) %>%
    as.data.frame()

  
  # SAVE DATA ---------------------------------------------------------------
  
  if(saveHrly){
    save(wdat.hr, file=paste0("data/wunderground/",site.ID, "_",sta.ID,"_",min(wdat$year),"-",max(wdat$year),"_hr.rda"))
    cat("Data saved here:", "\n", getwd(),"/data/wunderground")
  }
  if(saveDaily){
    save(wdat.dy, file=paste0("data/wunderground/",site.ID, "_",sta.ID,"_",min(wdat$year),"-",max(wdat$year),"_dy.rda"))
    cat("Data saved here:", "\n", getwd(),"/data/wunderground")
  }

  if(save15){
    save(wdat, file=paste0("data/wunderground/",site.ID, "_",sta.ID,"_",min(wdat$year),"-",max(wdat$year),".rda"))
    cat("Data saved here:", "\n", getwd(),"/data/wunderground")
  }

  # PLOTS -------------------------------------------------------------------
  if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}
  
  # Plot the Hourly 
  print(m.met<-ggplot()+
          geom_point(data=wdat.hr,aes(x=datetime,y=((TemperatureF-32)/(1.8))),col="blue")+
          geom_line(data=wdat.hr,aes(x=datetime,y=((DewpointF-32)/(1.8))),col="maroon", alpha=0.3)+
          xlab("") + ylab(expression(paste("Temperature (",degree,"C)")))+theme_bw()+
          ggtitle("Air Temperature (blue) and Dewpoint (maroon)"))
  
  Pause <- function () { 
    cat("Hit <enter> to continue...")
    readline()
    invisible()
  }
  
  Pause()  
  
  # Plot the Daily 
  print(d.met<-ggplot()+
          geom_ribbon(data=wdat.dy,aes(date,ymax = ((TemperatureF_max-32)/1.8),ymin=((TemperatureF_min-32)/1.8)),col="gray30", alpha=0.2)+
          geom_point(data=wdat.dy,aes(date,((TemperatureF_mean-32)/1.8)),col="blue")+
          geom_line(data=wdat.dy,aes(date,((TemperatureF_mean-32)/1.8)),col="blue2", alpha=0.4)+
          xlab("") + ylab(expression(paste("Temperature (",degree,"C)"))) + theme_bw() +
          ggtitle("Average Daily Air Temperature with Max/Min"))
  
  Pause()
  
  # assign to local/Global Environment
  d1<-paste0(site,"_",sta.ID,"_15")
  assign(d1,wdat, envir=.GlobalEnv) # print to workspace
  
  d2<-paste0(site,"_",sta.ID,"_hr")
  assign(d2,wdat.hr, envir=.GlobalEnv) # print to workspace
  
  d3<-paste0(site,"_",sta.ID,"_dy")
  assign(d3,wdat.dy, envir=.GlobalEnv) # print to workspace
  
  cat("All finished... data in current environment \n")
  rm(wdat.dy, wdat.hr)
}  