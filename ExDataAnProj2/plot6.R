###Code for plot 6

### Compare emissions from motor vehicle sources in Baltimore City 
### with emissions from motor vehicle sources in Los Angeles County, California 
### (fips == "06037"). Which city has seen greater changes over time in motor vehicle 
### emissions?

## Open ggplot2
library(ggplot2)

## Read the required .RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# get baltimore information 
baltimore<-subset(NEI,fips == "24510")
LA<-subset(NEI,fips == "06037")
cities<- rbind(baltimore,LA)

# Get vehicle related sources from EI.Sector data  
vehicle<-grep("vehicle",tolower(SCC$EI.Sector))   # Get the row number where the name coal is found
vehicle<-as.character(SCC[vehicle,1])             # Get the SCC numbers for the identified rows
data<-data.frame()                          # Loop to extract all observations that have a SCC number related to coal combustion
for (i in 1:length(vehicle)) {
    temp<-cities[cities$SCC==vehicle[i],]
    data<-rbind(data,temp)
}

# get total emission por year for each type
types<-split(data,data$fips)
total<-data.frame()
for (i in 1:length(types)) {
    each<-tapply(types[[i]]$Emissions,types[[i]]$year,sum)
    each<-cbind(rep(names(types)[i],length(each)),names(each),unname(each))
    total<-rbind(total,each)
}
names(total)<-names(data[c(1,6,4)])
total$fips<-gsub("24510","Baltimore City",total$fips,)
total$fips<-gsub("06037","Los Angeles County",total$fips,)
total$fips<-as.factor(total$fips)

#Plot png file
png("plot6.png")

plot<-ggplot(total,aes(total$year,as.numeric(as.character(total$Emissions)),group=total$fips))
plot<-plot+geom_line(aes(color=total$fips))+theme_bw()+geom_point(aes(color=total$fips))
plot<-plot+labs(x="Year",
          y="Total PM2.5 emissions / ton",
          title="Total vehicle PM2.5 emissions in Baltimore vs. Los Angeles",
          legend="Type of PM2.5 source") + scale_colour_discrete(name="Region")

print(plot)
dev.off()

