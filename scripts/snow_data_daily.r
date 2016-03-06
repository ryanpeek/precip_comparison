##Download daily avg gage data from USGS and plot graphs for multiple years
##by R Peek 02/23/15

library(lattice)
library(latticeExtra)
library(plyr)   

# load custom functions
source(paste0(root, '/functions/getCDEC_snow.R'))

# current data are:
yr <- 2014
mo <- 'Jan'

## Typical CDEC web request:
# http://cdec.water.ca.gov/cgi-progs/snowQuery?course_num=129&month=(All)&start_date=2010&end_date=2011&csv_mode=Y&data_wish=Retrieve+Data

# all courses in my area
course.list <- data.frame(course_number=c(129, 323, 131, 132, 134, 345, 344, 138, 139, 384, 140, 142, 143, 145, 152))

# get historic data
d.long_term <- ddply(course.list, .(course_number), .progress='text', getMonthly, start_yr=1900, end_yr=2015)

# get CURRENT data
d.CURR <- ddply(course.list, .(course_number), .progress='text', getMonthly, start_yr=yr, end_yr=yr)

## get long-term average, by course / month, using the Adjusted 
d.long_term.avg <- ddply(d.long_term, .(course_number, month), summarise,
                         avg.SWE=mean(SWE, na.rm=TRUE),
                         avg.density=mean(density, na.rm=TRUE),
                         avg.Depth=mean(Depth, na.rm=TRUE),
                         no.yrs=length(na.omit(SWE))
                         )

# join current data with long term averages
# keep only data that exists in both tables
d.merged <- join(d.CURR, d.long_term.avg, by=c('course_number', 'month'), type='left')

# compute pct of normal of depth, SWE, density
d.merged$pct_of_normal_Depth <- with(d.merged, (Depth / avg.Depth) * 100.0)
d.merged$pct_of_normal_SWE <- with(d.merged, (SWE / avg.SWE) * 100.0)
d.merged$pct_of_normal_density <- with(d.merged, (density / avg.density) * 100.0)


## using customized boxplot !!
## compare with Jan, Feb, March, April, May
# new color scheme
tps <- list(box.dot=list(col='black', pch='|'), box.rectangle=list(col='black'), 
            box.umbrella=list(col='black', lty=1), plot.symbol=list(col='black', cex=0.33))
trellis.par.set(tps)

## compare this year's data with long-term variability
p1 <- xyplot(factor(course_number) ~ SWE | month, data=d.CURR, fill='RoyalBlue', pch=21, cex=0.75, ylab='')
p2 <- bwplot(factor(course_number) ~ SWE | month, data=d.long_term, 
             stats=custom.bwplot, xlab='Snow Water Eqivalent (in)', strip=strip.custom(bg=grey(0.85)), 
             layout=c(5, 1), as.table=TRUE, scales=list(x=list(alternating=3)), 
             subset=month %in% c('January','February','March','April','May'))

## adjust panel heights
p2 <- update(p2, panel.height=list(y=c(2,3,1), unit='null'))

# combine figures
p3 <- p2 + p1

# add title, and legend
main.title <- paste(mo, yr, 'Snow Survey')
p3 <- update(p3, main=main.title, 
             key=list(corner=c(0.97,-0.15), points=list(pch=21, fill='RoyalBlue', cex=1), 
                      text=list(paste('Current (', mo, ' ', yr, ') SWE', sep='')), between=1))

# make legend sub-plot
set.seed(101010)
x <- rnorm(100)
x.stats <- custom.bwplot(x)
p4 <- bwplot(x, stats=custom.bwplot, scales=list(draw=FALSE), 
             xlab=list('Percentiles', cex=0.75), ylab='', ylim=c(0.5, 1.75))
p4 <- p4 + layer(panel.text(x=x.stats$stats, y=1.5, label=c('5%','25%','50%','75%','95%'), cex=0.75))

# make a file name with the current month and year
file_name <- paste('figures/', mo, '_', yr, '.pdf', sep='')

# save to file

# pdf(file=file_name, width=12, height=6)
trellis.par.set(tps)
print(p3, more=TRUE, position=c(0,0.08,1,1))
print(p4, more=FALSE, position=c(0.125,0,0.425,0.175))
# dev.off()


## make a table summarising results
file_name <- paste('tables/', mo, '_', yr, '.csv', sep='')
new.order <- order(d.merged$watershed)
write.csv(d.merged[new.order, c('course_number','watershed','course','elevation','Depth', 'pct_of_normal_Depth','SWE','pct_of_normal_SWE', 'density', 'pct_of_normal_density')], file=file_name, row.names=FALSE)
