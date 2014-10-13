####Script to analyse run data from the Samsung Galaxy S II UCI HAR dataset
#### as part of the Getting and Cleaning Data course project

#### 1 Merges the training and the test sets to create one data set.
#collected all file names for test and train, open and merge them
#each variable is encoded in a number for later use

##Test files names
folder<-"./UCI HAR Dataset/test"
test<-list.files(folder) # get all file names in the test folder
test<-test[2:4] #  discard Inertial Signals folder name from the list
test<-paste(folder,"/",test,sep="") # change file names to file directory

folder<-"./UCI HAR Dataset/test/Inertial Signals"
test2<-list.files(folder)
test2<-paste(folder,"/",test2,sep="")

file_paths<-append(test,test2) # path to all files

#Read test files with each file correspondind to a differente variable
test_data<-read.table(file_paths[1])
names(test_data)<-1

for (i in 2:length(file_paths)) {
    file<-read.table(file_paths[i])
    names(file)<-rep(i,ncol(file)) #variable encoding number
    test_data<-cbind(test_data,file) 
}
# Add that the sample is part of the test observations
test_data<-cbind(test_data,rep("1",nrow(test_data)))
names(test_data)[ncol(test_data)]<-"sample"


##Train files names
folder<-"./UCI HAR Dataset/train"
train<-list.files(folder)
train<-train[2:4] 
train<-paste(folder,"/",train,sep="") 
    
folder<-"./UCI HAR Dataset/train/Inertial Signals"
train2<-list.files(folder)
train2<-paste(folder,"/",train2,sep="") 

file_paths<-append(train,train2) 
    
#Read train files with each file correspondind to a differente variable
train_data<-read.table(file_paths[1])
names(train_data)<-1
    
for (i in 2:length(file_paths)) {
    file<-read.table(file_paths[i])
    names(file)<-rep(i,ncol(file)) 
    train_data<-cbind(train_data,file) 
}
#Add that the sample is part of the train observations
train_data<-cbind(train_data,as.character(rep("2",nrow(train_data))))
names(train_data)[ncol(train_data)]<-"sample"

##Merge files
data<-rbind(test_data,train_data)

#### 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# take means and standard deviations for the rows of each file using the encoding number
# variable 1 and 4 (subject and activity) don't have mean/deviation, so are done outside the loop
newdata<-data[,names(data)==1]
file<-data[,names(data)==2] # Get all columns encoded by name 2
mean<-rowMeans(file) #take the row means
std<-apply(file,1,sd) #take the row standard deviation
newdata<-cbind(newdata,mean) #save the mean in data frame
newdata<-cbind(newdata,std) #save the deviation in data frame
newdata<-cbind(newdata,data[,names(data)==3])
# loop repeating the mean/deviation for the remaining variable
for (i in 4: as.numeric(names(data)[ncol(data)-1])) {
    file<-data[,names(data)==i]
    mean<-rowMeans(file)
    std<-apply(file,1,sd)
    newdata<-cbind(newdata,mean)
    newdata<-cbind(newdata,std)
}
newdata<-cbind(newdata,data[,names(data)=="sample"]) #add variable sample outside the look

#### 3 Uses descriptive activity names to name the activities in the data set
#replace number for the appropriate names

newdata<-as.data.frame(newdata) # to allow proper manipulation of the data

## Open the activity labels file from UCI HAR dataset
activity_labels<-read.table("./UCI HAR Dataset/activity_labels.txt")

for (i in 1:nrow(activity_labels)) {
    newdata[,4]<-gsub(activity_labels[i,1],activity_labels[i,2],newdata[,4])
}
newdata[,ncol(newdata)]<-gsub(1,"Test",newdata[,ncol(newdata)])
newdata[,ncol(newdata)]<-gsub(2,"Train",newdata[,ncol(newdata)])

#### 4 Appropriately labels the data set with descriptive variable names. 
# Just add names
colnames(newdata)<-c(
    "Subjects",
    "Sets mean","Sets deviation",
    "Activity",
    "Body Acceleration x mean","Body Acceleration x deviation",
    "Body Acceleration y mean","Body Acceleration y deviation",
    "Body Acceleration z mean","Body Acceleration z deviation",
    "Angular Velocity x mean","Angular Velocity x deviation",
    "Angular Velocity y mean","Angular Velocity y deviation",
    "Angular Velocity z mean","Angular Velocity z deviation",
    "Total Acceleration x mean","Total Acceleration x deviation",
    "Total Acceleration y mean","Total Acceleration y deviation",
    "Total Acceleration z mean","Total Acceleration z deviation",
    "Sample")

#### 5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#cycle throught the subjects and then throught the variables to get the mean values

tidy<-data.frame()

for (i in 1:max(newdata$Subjects)) {
    subject<-newdata[newdata$Subjects==i,] # select subject
    activity<-subject$Activity #save activity vector
    sample<-subject[1:6,23] #save sample information
    subject<-subject[,c(1:3,5:22)] #take only the numerical variables
    
    subj<-data.frame(1:6)
    for (j in 1:ncol(subject)) {
            subj_means<-tapply(subject[,j],activity,mean) #get means
            subj[1]<-names(subj_means) #take the names from resulting array and turn tehm into a variable
            subj[j+1]<-subj_means #save the means
    }
    subj[23]<-sample #add the sample information
    tidy<-rbind(tidy,subj) #save information for each subject
}

#rearrange names
names<-c("Activity",names(newdata[c(1:3,5:23)])) 
names(tidy)<-names
data<-tidy[,c(2,23,1,5:22)]

#make .txt file with tidy data
write.table(data,"tidydata.txt",row.name=FALSE)
