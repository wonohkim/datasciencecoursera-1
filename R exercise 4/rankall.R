rankall <- function(outcome, num="best") {
    
    
    ## Read outcome data
    #open file
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    #select relevant information: names, state and conditions
    data<-data[,c(2,7,11,17,23)]
    #replace conditions names to match the input easily
    names(data)<-c("name","state","heart attack", "heart failure", "pneumonia")
    
    
    ## Check that state and outcome are valid
    #select information of the condition on the selected state
    if(outcome=="heart failure" || 
           outcome=="heart attack" || 
           outcome=="pneumonia") {
        condition<-data[,c("name","state",outcome)]
    }else {
        stop (paste(" invalid outcome"))
    }
    
    ## Return hospital list
    condition[,3]<-as.numeric(condition[,3])
    rank<-condition[order(condition[,3],condition[,1],na.last = NA),]
    
    #set state list
    states<-rank[,2]
    states<-states[!duplicated(states)]
    states<-states[order(states)]
    ## rate
    if (num=="best") {num<-1}
    
    
    hospital<-as.character(data.frame())
    for(i in 1:length(states)) {
        x<-rank[rank[,2]==states[i],]
        if (num=="worst") {
            n<-nrow(x) 
            hospital<-rbind(hospital,x[n,1])
        }else {
            hospital<-rbind(hospital,x[num,1])
        }
    }
    hospital<-cbind(hospital,states)
    hospital<-as.data.frame(hospital)
    names(hospital)<-c("hospital","state")
    hospital
}
