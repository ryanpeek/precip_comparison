# Read NRCS SNOTEL climate data from website in tab delimited format

library(tidyverse)
# read data table from the NRCS SNOTEL site
# snotel metadata: 
# https://www.wcc.nrcs.usda.gov/ftpref/data/water/wcs/gis/data/sntl_data.readme
snotel_meta <- read_delim("https://www.wcc.nrcs.usda.gov/ftpref/data/water/wcs/gis/data/sntl_data.readme",delim = ": ", skip = 10, trim_ws = TRUE,
                          col_names = c("col_number", "metric_name", "metric_detail"))

data <- read.table("https://www.wcc.nrcs.usda.gov/ftpref/data/water/wcs/gis/data/sntl_data.csv", header=T ,sep="\t")	

w <- colnames(data)
sitename <- substr(w[1], 1, nchar(w[1])-5)

d <- data$CACROWDER.FLAT.date													
f <- ifelse(nchar(d)<6, paste(substr(d,4,5),"-", substr(d, 2, 3), "-", substr(d, 0, 1), sep=""), 	#a new R object is created using a logical statement which converts the NRCS date format to a text string which can be understood by R
	paste(substr(d,5,6), "-", substr(d, 3,4), "-", substr(d, 1,2), sep=""))

data$date <- as.Date(f, "%y-%d-%m")												#takes the R object 'f', converts it to the native R date format, and adds it as a new column to the 'data' dataframe
data$mo <- as.numeric(format(data$date, format="%m"))									#creates a new column in the 'data' dataframe which inlcudes the numeric month
data$yr <- as.numeric(format(data$date, format="%Y"))
data$moyr <- paste(data$yr, "-",data$mo, sep="")

head(data)																#prints to the R console the first column heading s and first five data rows to check if data was read in correctly

data$prcp[is.na(data$prcp)] <-0												#replaces all NAs with 0s in the data$prcp column

new.data <- na.omit(data)													#creates a new data object with the NAs omitted (omits entire row when an NA is encountered)

pag <- aggregate(data$prcp, by = list(data$moyr), FUN = "sum")							#computes the monthly precipitation sum
  pag$yr <- substr(pag$Group.1, 1, 4)											#adds a year column to the 'pag' object
  pag$mo <- as.numeric(ifelse(nchar(pag$Group.1)<7, substr(pag$Group.1, 6,6), 				#adds a month column to the 'pag' object
	substr(pag$Group.1, 6, 7)))
pagavg <- aggregate(pag$x, by = list(pag$mo), FUN = "mean")								#creates a new 

tavg <- aggregate(new.data$tavg, by = list(new.data$mo), FUN = "mean")						#computes monthly mean of daily average temperature
tmin <- aggregate(new.data$tmin, by = list(new.data$mo), FUN = "mean")
tmax <- aggregate(new.data$tmax, by = list(new.data$mo), FUN = "mean")


par(mfrow=c(2,2))															#creates a two by two plot matrix which is filled with subsequent plots

  plot(data$prec~data$date, type="l",lty=1, main=paste("NRCS SNOTEL SITE:",sitename, 			#plot accumulated precipitation
	"\n", "Water Year Accumulated Precipitation"), ylab="Precipitation (inches)", xlab="Year")
  boxplot(pag$x~pag$mo, main="Average Monthly Precipitation", 							#boxplot average monthly precipitation
	ylab="Precipitation (inches)", xlab="Month")		 
	lines(pagavg)														#adds a line depicting the monthly precipitation means calculated in 'pagavg'
  plot(tavg, main="Average Monthly Temperature", ylab="Temperature (Farenheit)", xlab="Month",		#plot monthly average temperature with monthly average highs and lows shown as lines
	ylim=c(-5,30))
	lines(tmin)
	lines(tmax)
	lines(tmax)

	
	
