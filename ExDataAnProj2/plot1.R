###Code for plot 1
### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
### Using the base plotting system, make a plot showing the total PM2.5 emission from
### all sources for each of the years 1999, 2002, 2005, and 2008.
## Read the required .RDS files
unzip("exdata_data_NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
# get total emission per year
total<-tapply(NEI$Emissions,NEI$year,sum)
#get plot information
year<-names(total)
emissions<-unname(total)
emissions<-emissions/100000
#Plot png file
png("plot1.png")
plot(emissions~year,
     ylab="Total PM2.5 emissions / 100000 tons",
     col="red",
     xlim=c(1998,2009),
     main = "Total PM2.5 emissions in the US from 1999 to 2008")
points(emissions~year,col="blue",type="l")
dev.off()