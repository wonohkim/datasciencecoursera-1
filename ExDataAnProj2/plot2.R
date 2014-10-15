###Code for plot 2

### Have total emissions from PM2.5 decreased in the Baltimore City, 
### Maryland (fips == "24510") from 1999 to 2008? 
### Use the base plotting system to make a plot answering this question.

## Read the required .RDS files
NEI <- readRDS("summarySCC_PM25.rds")

# get baltimore information 
baltimore<-subset(NEI,fips == "24510")

# get total emission por year
total<-tapply(baltimore$Emissions,baltimore$year,sum)


#get plot information
year<-names(total)
emissions<-unname(total)

#Plot png file
png("plot2.png")

plot(emissions~year,
     ylab="Total PM2.5 emissions / tons",
     col="red",
     xlim=c(1998,2009),
     main = "Total PM2.5 emissions in Baltimore City from 1999 to 2008")
points(emissions~year,col="blue",type="l")

dev.off()
