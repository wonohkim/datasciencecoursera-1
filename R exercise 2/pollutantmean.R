pollutantmean <- function(directory, pollutant, id = 1:332) {
    files<-list.files(directory,full.names=TRUE)
    data<-data.frame()
    for (i in 1:332) {
        data<-rbind(data,read.csv(files[i]))
    }
    subset<-data.frame()
    for (i in id) {
        subset<-rbind(subset,data[data$ID==i,])
    }
    mean(subset[,pollutant],na.rm=TRUE)
}
