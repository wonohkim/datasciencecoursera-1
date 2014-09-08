corr <- function(directory, threshold = 0) {
    files<-list.files(directory,full.names=TRUE)
    data<-data.frame()
    for (i in 1:332) {
        data<-rbind(data,read.csv(files[i]))
    }
    
    output<-complete(directory)
    logic<-output[output$nobs>threshold,]
    x<-logic[,1]
    l<-1:length(x)
    correlation<-vector()
    for (i in l) {
        newdata<-subset(data,ID==x[i])
        nitrate<-newdata$nitrate
        sulfate<-newdata$sulfate 
        if(length(nitrate)>0) {
            if(length(sulfate)>0) {
                correlation[i]<-cor(sulfate,nitrate,use = "complete.obs")
            }
        }
        
    }
    correlation
}