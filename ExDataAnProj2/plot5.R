###Code for plot 5

### How have emissions from motor vehicle sources 
### changed from 1999–2008 in Baltimore City?


## Read the required .RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore information 
baltimore<-subset(NEI,fips == "24510")

# Get vehicle related sources from EI.Sector data  
vehicle<-grep("vehicle",tolower(SCC$EI.Sector))   # Get the row number where the name coal is found
vehicle<-as.character(SCC[vehicle,1])             # Get the SCC numbers for the identified rows
data<-data.frame()                          # Loop to extract all observations that have a SCC number related to coal combustion
for (i in 1:length(vehicle)) {
    temp<-baltimore[baltimore$SCC==vehicle[i],]
    data<-rbind(data,temp)
}

# get total emission por year
total<-tapply(baltimore$Emissions,baltimore$year,sum)

#get plot information
year<-names(total)
emissions<-unname(total)

#Plot png file
png("plot5.png")

plot(emissions~year,
     ylab="Total PM2.5 emissions / tons",
     col="red",
     xlim=c(1998,2009),
     main = "Total PM2.5 emissions in Baltimore City from motor vehicles")
points(emissions~year,col="blue",type="l")

dev.off()

