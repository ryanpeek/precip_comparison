##################################################################################
##            RClimate.txt                                                      ##
##  Collection of Climate data analysis functions to assist with                ##
##  climate data analysis. These functions developed and maintained by          ##
##  D Kelly O'Day, http://chartsgraphs.wordpress.com                            ##
##  Data files and RClimate scripts stored at http://processtrends.com          ##
##  Users are asked to credit RClimate when using scripts or plots              ##
##  Orig June 14, 2010                                                          ##
##  Updated 10/26/10: snap()                                                    ## 
##          11/3/10:  Added func_NINO34()                                       ## 
##           11/3/10: Added mon_name[x]                                         ##
##  updated 11/11/10: ggplot2 & reshape library loads                           ##
##          11/12/10: Resolved header issue in Nino34 data file                 ##
##          11/13/10: Added func_PDO()                                          ##
##          11/13/10: Added func_AMO()                                          ##
##          11/13/10: Standardized function names & capitalization              ##
##          11/19/10: Added PDO, AMO, NINO34 to LOTA series csv file            ##
##                    Added PDO, AMO, NINO34 to plot_series(x) function         ##
##          11/21/10: plot_series()function uses alpha names like "GISS"        ##
##           1/ 3/11: Revised Hadley link & script for quicker updates          ##
##           1/ 7/11: Revised UAH link to use 30 yr baselinse series(5.4)       ##
##           1/19/11: Added func_AT() for MLO Atmos Trans monthly series        ##
##           1/19/11: Added func_SATO() for NASA's SATO (Up to 12/99)           ##
##           1/19/11: Added func_AO() for Arctic Oscil monthly series           ##
##           1/19/11: Added func_VEI() for Volcano VEI time series              ##
##           1/19/11: Added func_CO2() for MLO CO2 monthly series               ##
##           1/19/11: Added func_TLS() for RSS monthly TLS series               ##
##           1/21/11: Added func_SSN() for Sunspot Number series since 1750     ##
##           1/21/11: Added func_MEI() for 2nd ENSO series - data from 1950     ##
##           2/19/11: Changed MEI link - Climate Explorer - NOAA table messy    ##
##           2/19/11: Changed SSN link - Cliamte Explorer - NOAA table messy    ##
##           2/28/11: Changed RSS link to version 3.3                           ##
##           2/28/11: Changed Nino34 link for data after Dec, 2010              ##
##           3/09/11: Adjusted PDO file readlines for 2011                      ##
##           3/09/11: Updated RSS Land & Ocean TLS link to v3.3                 ##
##           7/17/11: Updated GISS - change in source file notes format         ##  
##################################################################################
  library(ggplot2)
  library(reshape)
 
  R_list <- c("R_list", "func_GISS", "func_HAD", "func_NOAA", "func_RSS", "func_UAH","func_LOTA",
   "func_SSTA", "func_AMO", "func_NINO34", "func_MEI","func_PDO", "func_AO", "func_MLOAT","func_SATO", "func_CO2",
   "func_VEI", "func_TLS", "func_SSN",
   "snap", "func_yr_mn", "func_dt_2_yf","func_yr_mn_2_yf","plot_series(x)", "mon_name[x]")
   series_names<- c("GISS", "HAD", "NOAA", "RSS", "UAH", "PDO", "AMO", "NINO34", "MEI", "SSTA", "AO", "AT", "SATO",
                    "CO2", "TLS", "VEI", "SSN")

################################# Global TEMPERATURE ANOMALY DATA SERIES ####################

##########  GISS LOTA ##################################################
# GISS Monthly Temp Anomaly Trend - Land & Sea C - 1880 - latest month
# Function to retrieve GISS monthly LOTA produce long format data.frame
 func_GISS <- function() { 
   url <- c("http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt")
   file <- c("GLB.Ts+dSST.txt")
   download.file(url, file)
## Cleanup File
  rows <- length(readLines(file)) - 10
# Read file as  char vector, one line per row, Exclude first 3 rows 
  lines <- readLines(file, n=rows)[10:rows]
  lines2 <- gsub("\\*{3,5}", " NA", lines, perl=TRUE)
#Convert the character vector to a dataframe
  df <- read.table(
  textConnection(lines2), header=F, colClasses = "character")
  closeAllConnections()
# We are only interested in the montly data in first 13 columns
  df <- df[,1:13]
  names(df) <- c("Year", "Jan", "Feb", "Mar", "April", "May", "June", "July", "Aug","Sep", "Oct", "Nov","Dec")
# Convert all variables (columns) to numeric format
  df <- colwise(as.numeric) (df)
# Remove rows where Year=NA from the dataframe
  df <- df [!is.na(df$Year),] 
# Convert from wide format to long format
  dfm <- melt(df, id.var="Year", variable_name="Month")
  GISS_mo_num <- unclass(dfm$Month)
  GISS_mo_frac <- as.numeric((unclass(dfm$Month)-0.5)/12)
  GISS_yr_frac <- dfm$Year + GISS_mo_frac
  GISS_anom <- dfm$value/100
  GISS_yr_mn <- func_yr_mn(GISS_yr_frac)
  dfm <- data.frame(dfm, GISS_mo_num, GISS_yr_frac, GISS_yr_mn,  GISS_anom)
  dfm <- dfm[order(dfm$GISS_yr_frac), ]
  dfm <- dfm[!is.na(dfm$GISS_anom),]
  names(dfm) <- c("year", "month", "value","mo_num", "yr_frac", "yr_mn", "GISS")
  dfm<- dfm[,c(5,6,7)]
 return(dfm)
 }
##########################################################################################

########## HADCRUT3 LOTA ###############################################
## Download and process HADCRUT3 Data File ###############
##  1/3/11 - changed source file link beciase orig file update delayed
##  Use HADCrut3 recommended link @ http://hadobs.metoffice.com/hadcrut3/diagnostics/
  func_HAD <- function() {  
    h_link <- "http://hadobs.metoffice.com/hadcrut3/diagnostics/global/nh+sh/monthly"
    h <- read.table(h_link, as.is=T)
    h <- h[,c(1,2)]
    names(h) <- c("yr_mon", "HAD")
  
    h$yr <- as.numeric(substring(h$yr_mon, 1, 4))
    h$mon <- as.numeric(substring(h$yr_mon, 6,7))
    h$yr_frac <- h$yr + (h$mo-0.5)/12 

  HAD_df <- data.frame(h$yr_frac, h$HAD)
  names(HAD_df) <- c("yr_frac", "HAD")

    HAD_df <- subset(HAD_df, HAD_df$yr_frac >1880 & HAD_df$HAD != 0.000)
   names(HAD_df) <- c("yr_frac", "HAD")
   return(HAD_df)
  }
##########################################################################################

##########  NOAA LOTA ##################################################
 func_NOAA <- function() {	
    #link_NOAA <- "ftp://ftp.ncdc.noaa.gov/pub/data/anomalies/monthly.land_and_ocean.90S.90N.df_1901-2000mean.dat"
	link_NOAA <- "ftp://ftp.ncdc.noaa.gov/pub/data/anomalies/monthly.land_ocean.90S.90N.df_1901-2000mean.dat"
      NOAA_in <- read.table(link_NOAA, skip = 0, sep = "", dec=".",
             row.names = NULL, header = FALSE,
             as.is = T,
             colClasses = c(rep("numeric",3)),
             comment.char = "#", na.strings = c("*", "-",-99.9, -999.9),
             col.names = c("NOAA_yr", "NOAA_mo", "NOAA"))
  yr_frac <- NOAA_in$NOAA_yr + (NOAA_in$NOAA_mo-0.5)/12     # yr_frac 
  yr_mn <- func_yr_mn(yr_frac)
  NOAA_in <- data.frame(yr_frac, yr_mn, NOAA_in$NOAA)
  NOAA_df <- subset(NOAA_in, NOAA_in$NOAA > -999)
  #NOAA_num <- nrow(NOAA_df)
  #NOAA_yf <- NOAA_df[NOAA_num,1]
  names(NOAA_df) <- c("yr_frac", "yr_mn", "NOAA")
  return(NOAA_df)
   }
##########################################################################################

##########  RSS  LOTA ##################################################
  func_RSS <- function() {
#  link_RSS<- "http://www.remss.com/data/msu/monthly_time_series/RSS_Monthly_MSU_AMSU_Channel_TLT_Anomalies_Land_and_Ocean_v03_2.txt"
  link_RSS <- "http://www.remss.com/data/msu/monthly_time_series/RSS_Monthly_MSU_AMSU_Channel_TLT_Anomalies_Land_and_Ocean_v03_3.txt"
  mo_RSS_in <- read.table(link_RSS,
             skip = 3, sep = "", dec=".",
             row.names = NULL, header = FALSE,
             as.is = T,
             colClasses = c(rep("numeric",11)),
             comment.char = "#", na.strings = c("*", "-",-99.9, -999.9),
             col.names = c("yr", "mo", "RSS", "s20_n20", "n20_n825", "s70_s20",
             "n60_n825", "R", "USA", "e_n825", "s70_e"))
    yr_frac <- mo_RSS_in$yr + (mo_RSS_in$mo-0.5)/12     # yr_frac simplifies calcs
    yr_mn <- func_yr_mn(yr_frac)
## Create new RSS_df data frame  
    RSS_df <- data.frame(yr_frac,yr_mn, mo_RSS_in[3])
   return(RSS_df)
}
##########################################################################################

########## UAH  LOTA ##################################################
  func_UAH <- function() {
	UAH_link <- "http://vortex.nsstc.uah.edu/public/msu/t2lt/tltglhmam_5.6"
	UAH_file <- c("tltglhmam_5.6")
	 download.file(UAH_link, UAH_file)
## The first 5 rows and the last row of the textfile contain text information, not data 
## Find out the number of rows in a file, and exclude the last 1,it has text
    UAH_rows <- length(readLines(UAH_file)) - 1
## Read file as  char vector, one line per row, Exclude first 5 rows 
    UAH_lines <- readLines(UAH_file, n=UAH_rows)[6:UAH_rows]
## Data Manipulation, R vector with data lines. 
## Use regexp to replace all the occurences of **** with NA
    lines2 <- gsub("\\*{3,5}", " NA", UAH_lines, perl=TRUE)
##Convert the character vector to a dataframe
    UAH_df <- read.table(
      textConnection(lines2), header=F, colClasses = "character")
    closeAllConnections()
## We are only interested in the most recent montly data columns 1:3
    UAH_df <- UAH_df[,1:3]
## Convert all variables (columns) to numeric format
    UAH_df <- colwise(as.numeric) (UAH_df)
    UAH_yr_frac <- UAH_df[1]+  +(UAH_df[2]-0.5)/12
    UAH_yr_mn <- func_yr_mn(UAH_yr_frac$V1)
    UAH_df <- data.frame(UAH_yr_frac, UAH_yr_mn, UAH_df)
    #UAH_num <- nrow(UAH_df)
   # UAH_yf <- UAH_df[UAH_num,1]
    UAH_df <- UAH_df[,c(1,2,5)]
   names(UAH_df) <- c("yr_frac", "yr_mn", "UAH")
   UAH_df <- subset(UAH_df, UAH_yr_frac >= 1979)
   return(UAH_df)
  }
#########################################################################################

########## 5 LOTA, SSTA, 3 Oscillation Series #############################
 func_LOTA <- function() { 
  # Returns: 5 monthly climate global land - ocean temperature anomaly (C) time series: GISS, HAD, NOAA, RSS,UAH
  #            monthly SSTA
  #          3 monthly oscillation indexes: PDO, AMO, NINO34
  link <- "http://processtrends.com/Files/RClimate_consol_temp_anom_latest.csv"
  lota <- read.csv(link)
  return(lota)
  }
#######################################################################################

########## SSTA function ##################################################

func_SSTA <- function() {
 link <- "ftp://ftp.ncdc.noaa.gov/pub/data/anomalies/monthly.ocean.90S.90N.df_1901-2000mean.dat"
 source_df <- read.table(link, na.strings=-999.0000)
 names(source_df) <- c("yr", "mo","ssta")
 yr_frac <- source_df$yr + (source_df$mo-0.5)/12
 ssta_df <- data.frame(yr_frac, source_df$ssta)
 names(ssta_df) <- c("yr_frac", "SSTA")
 ssta_df <- subset(ssta_df, ssta_df$SSTA > -10)
 return(ssta_df)
 }
#######################################################################################

########## RSS Land & Ocean TLS function #############################################

func_TLS <- function() {
 # original link was to land only; revised to Land & Ocean
 #link <- "http://www.remss.com/data/msu/monthly_time_series/RSS_Monthly_MSU_AMSU_Channel_TLS_Anomalies_Land_v03_3.txt"
 link <- "http://www.remss.com/data/msu/monthly_time_series/RSS_Monthly_MSU_AMSU_Channel_TLS_Anomalies_Land_and_Ocean_v03_3.txt"
 TLS_df <- read.table(link,skip=3, header=F)
 TLS_df <- TLS_df[,c(1,2,3)]
 TLS_df$yr_frac <- TLS_df[,1] + (TLS_df[,2]-0.5)/12
 TLS_df <- TLS_df[,c(4,3)]
 names(TLS_df) <- c("yr_frac", "TLS")
 return(TLS_df)
 }
########## MLO Atmospheric Transmission  ############################

func_MLOAT <- function() {
 ## MLO Atmospheric Transmission file does not include last 12 months
   link <- "http://cmdl1.cmdl.noaa.gov:8000/www/trans/mlo_transmission.html"
   file <- c("at_raw.txt")
   download.file(link, file)
   rows <- length(readLines(file)) 
   lines <- readLines(file, n=rows)[20:rows-3]
   df <- read.table(
     textConnection(lines), header=F, na.strings = c("0", "0.000","0.0000"),colClasses = "character")
   closeAllConnections()
   df <- df[,c(2,3)]
 # Convert all variables (columns) to numeric format
   MLOAT_df <- colwise(as.numeric) (df)
   names(MLOAT_df) <- c("yr_frac", "AT")
   return(MLOAT_df)
 }
#######################################################################################

########## SATO function ################################################
func_SATO <- function() {
 sato_link <- "http://data.giss.nasa.gov/modelforce/strataer/tau_line.txt"
   file <- c("temp.txt")
   download.file(sato_link, file)
   rows <- length(readLines(file)) 
   lines <- readLines(file, n=rows-1)[9:rows-1]
   SATO_df <- read.table(textConnection(lines), header=F)
   closeAllConnections()
   SATO_df <- SATO_df[,1:2]
   names(SATO_df) <- c("yr_frac", "SATO")
   return(SATO_df)
   }

################################ Climate Oscillations ##########################

##########  AMO  ##################################################
 func_AMO <- function() {
 ## Download and process AMO Data File ###############
  a_link <- "http://www.esrl.noaa.gov/psd/data/correlation/amon.us.long.data"
  a_file <-    c("amo_latest.txt")
  download.file(a_link, a_file)
  ## Find number of rows in the file
    (a_rows <- length(readLines(a_file)))
  ## Read file as  char vector, one line per row, Exclude first row 
    a_lines <- readLines(a_file, n=a_rows)
    num_a <- a_rows - 4
    a_lines_2 <- a_lines[2:num_a]
  ##Convert the character vector to a dataframe using fixed width format (fwf)
    a_df <- read.table(
     textConnection(a_lines_2), header=F, skip=0, colClasses = "numeric")
    closeAllConnections()
    names(a_df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ## Convert from wide format to long format
    dfa <- melt(a_df, id.var="Year", variable_name="Month")
    names(dfa) <- c("yr", "mon", "AMO")
  ## dfa$Month is factor, Convert to month number & calc yr_frac
    amo_mo_num <- unclass(dfa$mon)
    amo_mo_frac <- as.numeric( (amo_mo_num-0.5)/12   )
    yr_frac <- as.numeric(dfa$yr) + amo_mo_frac
   dfa <- data.frame(yr_frac,  dfa)
   dfa <- subset(dfa, dfa$AMO> -90)
   dfa <- dfa[order(dfa$yr_frac),]
   dfa <- subset(dfa, dfa$yr_frac > 1856)
   dfa <- dfa[,c(1,4)]
   return(dfa)
}
##########################################################################

########## AO function ##################################################

 func_AO <- function() {
 link <- "http://www.cpc.noaa.gov/products/precip/CWlink/daily_ao_index/monthly.ao.index.b50.current.ascii"
  AO_df <- read.table(link, skip = 0, na.strings = c("-0.99900E+34"), header=F)
  names(AO_df)<- c("yr", "mo", "AO")
  yr_frac <- as.numeric(AO_df$yr) + ((AO_df$mo-0.5)/12)
  AO_df <- data.frame(yr_frac, AO_df)
  AO_df <- subset(AO_df, AO_df$AO != "NA")
  AO_df <- AO_df[,c(1,4)]
  return(AO_df)
  }
#######################################################################################

############# MEI function ##################################
func_MEI <- function() {
# mei_url <- "http://www.esrl.noaa.gov/psd/people/klaus.wolter/MEI/table.html"
  mei_url <- "http://climexp.knmi.nl/data/imei.dat"
  file <- c("mei_temp")
  mdf <- read.table(mei_url, sep="", skip=3, na.strings = -999.900)
  names(mdf) <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12")
  dfm <- melt(mdf, id.var="Year", variable_name="Month")
  dfm$yr_frac <- dfm$Year + (as.numeric(dfm$Month)-0.5)/12
  mei_df <- data.frame(dfm$yr_frac, dfm$value)
  names(mei_df) <- c("yr_frac", "MEI")
  mei_df <- mei_df[order(mei_df$yr_frac,decreasing=F),]
  return(mei_df)
  }
#######################################################################################

############# NINO34 function ##################################
func_NINO34 <- function() {
 ## 2/28/11: change in data archive: climexp.knmi.nl site data only goes to Dec, 2011
 ## Need to use NOAA site for data From Jan, 2011 on: 
##  NOAA site: http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices
 link_1 <-   "http://climexp.knmi.nl/data/inino5.dat"
  in_data <- read.csv(link_1,
             skip = 1, sep = "", dec=".", as.is = T,
             colClasses=rep("numeric",13),
             comment.char = "#", na.strings = c("*", "-","-99.9", "-999.90"),
             col.names = c("Yr","Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug",
                         "Sept", "Oct", "Nov", "Dec"))
# Use reshape - melt function to convert from wide format to long format
## Specify id var, in this case year
### Specify measure variable as either names or col numbers: in this case use col numbers
  long <- melt(in_data, id.var=c("Yr"), measure.var=c(2:13))
  long <- colwise(as.numeric) (long)
  names(long) <- c("yr", "mon", "nino34")
  yr_frac <- as.numeric(long$yr) + (as.numeric(long$mon-0.5)/12)
  long <- data.frame(yr_frac, long)
  long <- long[order(long$yr_frac),]
  long <- subset(long, long$nino34 != "NA")
  NINO34_df <- long[,c(1,4)]
  names(NINO34_df) <- c("yr_frac", "NINO34")
 ## Data from January, 2011 on

  link_2 <- "http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices"
  recent_nino <- read.table(link_2,header=T, sep="")
  recent_nino <- recent_nino[,c(1,2,10)]
  recent_nino <- subset(recent_nino, recent_nino$YR>= 2011)
  recent_nino$yr_frac <- as.numeric(recent_nino$YR) + (as.numeric(recent_nino$MON-0.5)/12)
  recent_nino_df <- data.frame(recent_nino$yr_frac, recent_nino$ANOM.3)
  names(recent_nino_df) <- c("yr_frac", "NINO34")
  NINO34_m_df <- merge(NINO34_df, recent_nino_df, all=T, sort = T)
  return(NINO34_m_df)
  }
###################################################################################

##########  PDO  ##################################################
func_PDO <- function() {
  ## monthly pdo data set starts in 1900
  url <- "http://jisao.washington.edu/pdo/PDO.latest"
  file <- c("pdo_latest.txt")
  download.file(url, file) 
## Read & Cleanup File
   rows <- length(readLines(file)) -1
# Read file as  char vector, one line per row, Exclude first 32 rows 
  lines <- readLines(file, n=rows)[32:rows]
  lines2 <- lines[1:112]   # delete documentation lines end of file
#Convert the character vector to a dataframe using fixed width format (fwf)
  df <- read.fwf(
  textConnection(lines2), widths = c(4,-3, rep(7,12)), header=F, skip=0, colClasses = "numeric")
  closeAllConnections()
 names(df) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# Convert from wide format to long format
  dfm <- melt(df, id.var="Year", variable_name="Month")
  names(dfm) <- c("Year", "Month", "pdo")
# dfm$Month is factor, Convert to month number & calc yr_frac
    pdo_mo_num <- unclass(dfm$Month)
    pdo_mo_frac <- as.numeric( (pdo_mo_num-0.5)/12   )
    yr_frac <- as.numeric(dfm$Year) + pdo_mo_frac
# build consolidated data.frame 
  pdo_df <- data.frame(yr_frac,  dfm$pdo)
  pdo_df <- pdo_df[order(pdo_df$yr_frac), ]
  names(pdo_df) <- c("yr_frac", "PDO")
 pdo_df <- pdo_df[!is.na(pdo_df$PDO),]
  return(pdo_df)
}
########
########## MLO CO2 function ##############################################

func_CO2 <- function() {
   link <-url("ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt")
   CO2_df <- read.table(link,
             sep = "", row.names = NULL,header = F,colClasses = rep("numeric", 7),
             comment.char = "#", na.strings = -99.99)
  names(CO2_df) <- c("Yr", "Mo", "yr_dec", "CO2", "Trnd", "X", "dys")
  CO2_df$yr_frac <- CO2_df$Yr + (CO2_df$Mo-0.5)/12  
  CO2_df <- CO2_df[,c(8,4)]
  return(CO2_df)
 }
#######################################################################################

#######################################################################################

########## Volcano  function ##############################################
func_VEI <- function() {
# Original Data Source:http://www.volcano.si.edu/world/largeeruptions.cfm
 link_vol <- "http://processtrends.com/files/volcanoes.csv"
 vol <- read.csv(link_vol, sep=",",header=F, as.is=T)
 names(vol) <- c("vol_name", "date", "VEI")
 n_vol<- length(vol$vol_yr)
 v_yr <- as.numeric(format(as.Date(as.character(vol$date),format="%m/%d/%Y"),format="%Y"))
 v_mo <- as.numeric(format(as.Date(as.character(vol$date),format="%m/%d/%Y"),format="%m"))
 vol$yr_frac <- as.numeric(v_yr + (v_mo-0.5)/12)
 VEI_df <- data.frame(vol$yr_frac, vol$vol_name, vol$VEI)
 names(VEI_df) <- c("yr_frac", "Volcano", "VEI")
 return(VEI_df)
 }


########## Sunspot Number  function ##########################################
func_SSN <- function() {
 #ssn_url <- "ftp://ftp.ngdc.noaa.gov/STP/SOLAR_DATA/SUNSPOT_NUMBERS/INTERNATIONAL/monthly/MONTHLY"
 ssn_url <- "http://climexp.knmi.nl/data/isunspots.dat"
 sdf <- read.table(ssn_url, header=F)
 sdf <- colwise(as.numeric) (sdf)
  names(sdf) <- c("Year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11","12")
  dfm <- melt(sdf, id.var="Year", variable_name="Month")
  dfm$yr_frac <- dfm$Year + (as.numeric(dfm$Month)-0.5)/12
  ssn_df <- data.frame(dfm$yr_frac, dfm$value)
  names(ssn_df) <- c("yr_frac", "SSN")
  ssn_df <- ssn_df[order(ssn_df$yr_frac,decreasing=F),]
  ssn_df <- subset(ssn_df, ssn_df$SSN >-10)
  return(ssn_df)
  }


###############################################################################
###############################################################################
plot_series <- function(id)       {
  st <- c(rep(1880,3),rep(1979,2), 1900, rep(1880,3))
  num <- which(series_names==id)
  series_type <- c(rep("Global Temperature Anomaly -C",5), rep("Oscillation Index", 3),"Anomaly - C")
  d_col = num[1] + 2                # define lota column to plot
  series_id <- paste(series_names[num], " ", series_type[num], sep="")
  lota <- func_LOTA()
  series_data <- lota[,c(1,d_col)]
  cat("\n", "You entered", series_id, "\n") 
  st_yr <- st[num]
  series_data <- subset(series_data, series_data$yr_frac> st_yr)
  series_ts <- ts(series_data,start=st_yr, freq=12)
  ma_13 <- filter(series_ts, rep(1/13,13), sides=1)
  main_title <- series_id
  plot(series_data[,1], series_data[,2], type = "l", col = "grey", xlab="", las=1,
    main = main_title, ylab = series_type[num]  )
  points(ma_13[,1],ma_13[,2], col = "red", type = "l")

## highlight last point 
    last_pt_yr_frac <- lota[nrow(lota),1]
    last_pt_yr_mn <- lota[nrow(lota), 2]
    last_pt_val <- lota[nrow(lota),2 + num]
    last_pt_yr = as.integer(last_pt_yr_frac) # round down fractional year
    last_pt_mon = last_pt_yr_mn - last_pt_yr*100 # extract month
    last_pt_month <- mon_name[last_pt_mon]
    last_pt_note <- paste(last_pt_month, ", ", last_pt_yr, " @ ", last_pt_val, sep="")
    points(last_pt_yr_frac, last_pt_val, type="p", pch=16, col="blue", cex=1.2)

  legend("topleft", c("Monthly Avg", "13 Month Moving avg", last_pt_note), col = c("grey", "red", "blue"),
       text.col = "black", lty = c(1,1,0),pch = c(0,0,16),pt.cex=c(0,0,1),
       merge = F, bg = "white", bty="o", box.col = "grey", cex = .7)
  }
#######################################################################################

#######################################################################################
##################################################################

##################################################################################
######################## RClimate tools ##########################################


####  ann_avg function ##############################################
   # Calculate Annual Average From Monthly Time Series
    ann_avg<-function(x) {
	n<-length(x)
	m<-n-12*floor(n/12)
	if(m>0) x<-c(x, rep(NA,12-m))
	years<-length(x)/12
	x<-array(x,dim=c(12,years))
	annavg<-apply(x,2,mean,na.rm=T)
	return(annavg)
	}
##################################################################################

####  snap function ##############################################
   ## Function to provide data.frame snapshot
   # 1st 4 rows; middle 5 rows; last 4 rows
  snap <- function(x) {
  nx <- nrow(x)
  mid <- as.integer(nx/2)
  p_seq <- c(seq(1,4,1), seq(mid-2,mid+2,1), seq(nx-3,nx,1))
  print(x[p_seq,])
     }
##################################################################################

####  date to yr_frac function ##############################################
 func_dt_2_yf <- function(dt){  
   ## converts dt(as.Date) to yr_frac)
   yr <- as.numeric(format(dt, format="%Y"))
   mo <- as.numeric(format(dt, format="%m"))
   dy <- as.numeric(format(dt, format="%d"))
   yr_frac <- as.numeric(yr + (mo-1)/12 + (dy/30)/12)
return(yr_frac)}
##################################################################################


####  year & month number to yr_frac function ##################################
 func_yr_mn_2_yf <- function(yr, mo){  
   ## converts yr & mo number to yr_frac)
   yr_frac <- as.numeric(yr + (mo-0.5)/12)
return(yr_frac)}
##################################################################################



####  yr_mn function ##############################################
  ## to get yr_mn from yr_frac
  # y_f is yr_frac vector 
   func_yr_mn <- function(y_f) {  
    yr <- as.integer(y_f) 
    ## Each month is 1/12 or 0.083 of calandar year
    inc <- 1/12
    mo <- ceiling((y_f-yr)/(inc)) 
    mo_char <-  formatC(mo,width=2,flag='0') 
    yr_mn <- as.numeric(as.character(paste(yr, mo_char, sep="")  ))
    return(yr_mn)
 }
##################################################################################

mon_name <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")


