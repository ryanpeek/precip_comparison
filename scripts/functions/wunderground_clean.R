## Wunder_clean
## R. Peek 2015-Dec
## Function to clean data from wunderground download 
## from the wunder_daily script

# Run "wunder_daily" script first, then feed data into this script

wunder_clean <- function(data, 
                         interval=15, # data interval in min
                         saveHrly=FALSE, # save csv of hourly
                         saveDaily=FALSE, # save csv of daily
                         saveALL=FALSE) # save csv of all data
{
  
  
  # load packages
  if(!require(lubridate)) { install.packages("lubridate"); require(lubridate)}
  if(!require(dplyr)) { install.packages("dplyr"); require(dplyr)}
  
  wdat<-bind_rows(data)
  sta.ID<-unique(wdat$station)
  
  # Add other columns for processing
  wdat$year<-year(wdat$Time)
  wdat$mon<-month(wdat$Time)
  wdat$yday<-yday(wdat$Time)
  wdat$hour<-hour(wdat$Time)
  wdat$Time<-lubridate::with_tz(wdat$Time, tzone="America/Los_Angeles") # convert to UTC/America/Los_Angeles
  
  # function to snap to nearest 15 minutes
  ## Snap to nearest 15 minute period
  snaptime<-function(x,interval.min){
    x <- round(as.numeric(x)/(interval*60))*(interval*60)
    xx <- format(strptime("1970-01-01", "%Y-%m-%d", tz="UTC") + x)
    return(xx)
  }  
  
  wdat$timesnap<-ymd_hms(snaptime(wdat$Time, interval.min=interval)) # to snap to nearest 15 minutes
  wdat$timesnap<-with_tz(wdat$timesnap, tzone="America/Los_Angeles") # convert time
  str(wdat)
  
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
  #s(wdat.hr)
  
  # Make Daily dataset
  wdat.dy <- wdat %>%
    filter(TemperatureF > -999) %>% # get rid of bad temp data
    group_by(year, mon, yday)%>%
    select(TemperatureF:PressureIn,WindDirectionDegrees:Humidity) %>% 
    summarize_each(funs(mean,max,min)) %>% 
    mutate("date"=as.Date(strptime(paste0(year,"-", mon,"-", yday),format = "%Y-%m-%j"))) %>%
    #select(date,year,mon,yday,TemperatureF:Humidity) %>%
    as.data.frame()
  #s(wdat.dy)
  
  
  # SAVE DATA ---------------------------------------------------------------
  
  # if(!require(readr)) { install.packages("readr"); require(readr)}
  # 
  # # set dir
  # locdir<-"./data/processed/wunderground"
  # cat("\n","Use current dir to save file or choose dir? current (y) or choose (n)?", "\n\n")
  # reply<-scan(what="character",n=1)
  # if(reply=="y"){
  #   ### Set directory for saving to csv
  #   cat("Using current working directory: \n", getwd(), "\n\n")
  # } else {
  #   if(reply=="n"){
  #     cat("Type path to directory you want save file: \n")
  #     ldir<-scan(what="character",nlines = 1)
  #     setwd(ldir)
  #     cat("Working dir now: \n ", getwd(), "\n\n")
  #   } else {
  #     cat("Error: please only use 'n' or 'y'")
  #   }
  # }
  # 
  # if(saveHrly){
  #   write_csv(wdat.hr, path=paste0(getwd(),"/",station,"_",start,"-",end,"_hr.csv"))
  #   #saveRDS(wdat, file=paste0("./data/processed/wunderground/",station,"_",start,"-",end,"_hr.rds"))
  #   print(paste("Hourly data saved here: \n",getwd(),sep=""))
  # }
  # 
  # 
  # if(saveDaily){
  #   write_csv(wdat.dy, path=paste0(getwd(),"/",station,"_",start,"-",end,"_dy.csv"))
  #   #saveRDS(wdat, file=paste0("./data/processed/wunderground/",station,"_",start,"-",end,"_dy.rds"))
  #   print(paste("Daily data saved here: \n",getwd(),sep=""))
  # }
  # 
  # 
  # if(saveALL){
  #   write_csv(wdat, path=paste0(getwd(),"/",station,"_",start,"-",end,".csv"))
  #   #saveRDS(wdat, file=paste0("./data/processed/wunderground/",station,"_",start,"-",end,".rds"))
  #   print(paste("Full dataset saved here: \n",getwd(),sep=""))
  # }
  
  # PLOTS -------------------------------------------------------------------
  if(!require(ggplot2)) { install.packages("ggplot2"); require(ggplot2)}
  
  # Plot the Hourly 
  print(m.met<-ggplot()+
          geom_point(data=wdat.hr,aes(x=datetime,y=((TemperatureF-32)/(1.8))),col="blue")+
          geom_line(data=wdat.hr,aes(x=datetime,y=((DewpointF-32)/(1.8))),col="maroon", alpha=0.3)+
          xlab("") + ylab(expression(paste("Temperature (",degree,"C)")))+theme_bw()+
          ggtitle("Air Temperature (blue) and Dewpoint (maroon)"))
  Pause()  
  
  # add precip if you want:
  #   ppt<-wdat[,c(1,13)]
  #   ppt$Time<-as.Date(ppt$Time)
  #   wdat.dy<-merge(wdat.dy, ppt, by.x="date", by.y="Time")
  
  # Plot the Daily 
  print(d.met<-ggplot()+
          geom_ribbon(data=wdat.dy,aes(date,ymax = ((TemperatureF_max-32)/1.8),ymin=((TemperatureF_min-32)/1.8)),col="gray30", alpha=0.2)+
          geom_point(data=wdat.dy,aes(date,((TemperatureF_mean-32)/1.8)),col="blue")+
          geom_line(data=wdat.dy,aes(date,((TemperatureF_mean-32)/1.8)),col="blue2", alpha=0.4)+
          xlab("") + ylab(expression(paste("Temperature (",degree,"C)"))) + theme_bw() +
          ggtitle("Average Daily Air Temperature with Max/Min"))
  
  Pause()
  
  d1<-paste0(sta.ID,"_15")
  assign(d1,wdat, envir=.GlobalEnv) # print to workspace
  
  d2<-paste0(sta.ID,"_hr")
  assign(d2,wdat.hr, envir=.GlobalEnv) # print to workspace
  
  d3<-paste0(sta.ID,"_dy")
  assign(d3,wdat.dy, envir=.GlobalEnv) # print to workspace
  
  cat("All finished... data in current environment \n")
  
}  