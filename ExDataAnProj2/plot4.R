###Code for plot 4

### Across the United States, 
### how have emissions from coal combustion-related sources changed from 1999–2008?

## Read the required .RDS files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Get coal combustion related sources from EI.Sector data 
# -> I checked every variable and this was the one with fuel comb -coal
# which is the coal combustion-related sources 
coal<-grep("coal",tolower(SCC$EI.Sector))   # Get the row number where the name coal is found
coal<-as.character(SCC[coal,1])             # Get the SCC numbers for the identified rows
data<-data.frame()                          # Loop to extract all observations that have a SCC number related to coal combustion
for (i in 1:length(coal)) {
    temp<-NEI[NEI$SCC==coal[i],]
    data<-rbind(data,temp)
}

# get total emission por year
total<-tapply(data$Emissions,data$year,sum)


#get plot information
year<-names(total)
emissions<-unname(total)
emissions<-emissions/100000

#Plot png file
png("plot4.png")

plot(emissions~year,
     ylab="Total PM2.5 emissions / 100000 tons",
     col="red",
     xlim=c(1998,2009),
     main = "Total PM2.5 emissions in the US from coal combustion")
points(emissions~year,col="blue",type="l")

dev.off()



