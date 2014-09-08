complete <- function(directory, id = 1:332) {
    # read all the files on data
    files<-list.files(directory,full.names=TRUE)
    data<-data.frame()
    for (i in 1:332) {
        data<-rbind(data,read.csv(files[i]))
    }
    #select the ID 
    subset<-data.frame()
    for (i in id) {
        subset<-rbind(subset,data[data$ID==i,])
    }
    #clean the data for subsetting
    nobs<-complete.cases(subset)
    clean<-subset[nobs,]
    #subsetting the data for presentation
    y<-data.frame()
    nr<-data.frame()
    for (i in id) {
        y<-rbind(y,i)
        x<-clean[clean$ID==i,]
        nr<-rbind(nr,nrow(x))
    }   
    output<-cbind(y,nr)
    colnames(output)<-list("id","nobs")
    output
}