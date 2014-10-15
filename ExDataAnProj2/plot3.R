###Code for plot 3

### Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
### variable, which of these four sources have seen decreases in emissions from 1999–2008
### for Baltimore City? 
### Which have seen increases in emissions from 1999–2008? 
### Use the ggplot2 plotting system to make a plot answer this question.


## Open ggplot2
library(ggplot2)

## Read the required .RDS files
NEI <- readRDS("summarySCC_PM25.rds")

# get baltimore information 
baltimore<-subset(NEI,fips == "24510")


# get total emission por year for each type
types<-split(baltimore,baltimore$type)
total<-data.frame()
for (i in 1:length(types)) {
    each<-tapply(types[[i]]$Emissions,types[[i]]$year,sum)
    each<-cbind(rep(names(types)[i],length(each)),names(each),unname(each))
    total<-rbind(total,each)
}
names(total)<-names(baltimore[c(5,6,4)])

#Plot png file
png("plot3.png")

plot<-ggplot(total,aes(total$year,as.numeric(as.character(total$Emissions)),group=total$type))
plot<-plot+geom_line(aes(color=total$type))+theme_bw()+geom_point(aes(color=total$type))
plot<-plot+labs(x="Year",
          y="Total PM2.5 emissions / ton",
          title="Total PM2.5 emissions in Baltimore City per source type",
          legend="Type of PM2.5 source") + scale_colour_discrete(name="Type of PM2.5 source")
print (plot)
dev.off()

