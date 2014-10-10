###Code for plot 2

##CHECK FILES

#This part of the code checks if the file is in the working directory
#If there are no files with the proper name, then grep doesn't return any element
files<-list.files()
name<-"household_power_consumption"
files<-files[grep(name,files)] 

# If "files" has no element, download the file 
# NOTE: HTTP WAS CHANGE TO WORK IN MAC, may require changing for windows
if(length(files)==0) { #download file if not in working directory - http is for mac, maybe https works beeter for windows!!!
    print ("File not found in working directory")
    files<-download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = "./household_power_consumption.zip",method = "auto") 
    paste (name,".zip was downloaded to ",getwd(),sep="")
    files<-paste(name,".zip",sep="")
}

# With a correctly named file in the working directory, if there is only a .zip file, extract it
if (length(files)==1 && files==paste(name,".zip",sep="")) { 
    unzip(paste(name,".zip",sep=""))
    paste(name,".zip was extracted",sep="")
}

## READ THE .TXT TABLE
file<-read.table(paste(name,".txt",sep=""),sep=";",header=TRUE,na.strings="?")

# correct dates format
file$Date<-as.Date(file$Date,format = "%d/%m/%Y")

#get only the required dates
dates<-c("2007-02-01","2007-02-02")
dates<-as.Date(dates,format="%Y-%m-%d")
file<-rbind(file[file$Date==dates[1],],file[file$Date==dates[2],])

#get date-time values
times<-0
for (i in 1:nrow(file)) {
    times[i]<-paste(file$Date[i],file$Time[i])   
}
times<-strptime(times,format="%Y-%m-%d %H:%M:%S")
file<-cbind(file,times)

##plot data to .png (default 480x480)
png("plot2.png")
plot(file$times,file$Global_active_power,type="l",ylab="Global Active Power (kilowatt)",xlab="")
dev.off()


