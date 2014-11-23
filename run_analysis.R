##Reading Feature and activity Type information data into R
features<-read.table("./data/UCI HAR Dataset/features.txt")
activity_labels<-read.table("./data/UCI HAR Dataset/activity_labels.txt")

## Reading Test data into R
## For loading experiment data 
X_test<-read.table("./data/UCI HAR Dataset/test/X_test.txt")
## For loading Activity Type for each experiment
y_test<-read.table("./data/UCI HAR Dataset/test/y_test.txt")
## For loading subject's Identifier
subject_test<-read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## Reading Training data into R
## For loading experiment data 
X_train<-read.table("./data/UCI HAR Dataset/train/X_train.txt")
## For loading Activity Type for each experiment
y_train<-read.table("./data/UCI HAR Dataset/train/y_train.txt")
## For loading subject's Identifier
subject_train<-read.table("./data/UCI HAR Dataset/train/subject_train.txt")



##This section manipulates the loaded data for 'Test'
## Giving the 561 variables(columns) their logical name
names(X_test) <- features$V2
##We will now add 3 new column in front of the 'Test' data set
## 1. To identify who the subject was for the experiment using subject_test.txt
## 2. To identify which activity the subject was performing using y_test.txt
## 3. To identify whether the experiment was a training or an actual test
## Since we are only reading Test data now the observations should be marked 'Test'
Tst<-rep("Test",length(X_test[,1]))
tdata<- cbind(subject_test$V1,y_test$V1,Tst,X_test)
## Rename columns by putting logical names 
colnames(tdata)[1]<-"Subject_ID"
colnames(tdata)[2]<-"Activity_Type"
colnames(tdata)[3]<-"Test_or_Train"

##This section manipulates the loaded data for 'Training'
## Giving the 561 variables(columns) their logical name
names(X_train) <- features$V2
##We will now add 3 new column in front of the 'Training' data set
## 1. To identify who the subject was for the experiment using subject_train.txt
## 2. To identify which activity the subject was performing using y_train.txt
## 3. To identify whether the experiment was a training or an actual test
## Since we are only reading Training data now the observations should be marked 'Training'
Trst<-rep("Training",length(X_train[,1]))
trdata<- cbind(subject_train$V1,y_train$V1,Trst,X_train)
## Rename columns by putting logical names
colnames(trdata)[1]<-"Subject_ID"
colnames(trdata)[2]<-"Activity_Type"
colnames(trdata)[3]<-"Test_or_Train"

# Merging the Training and Test data by row-wise bind
# Here onwards fmasterd represents both Test and training data
fmasterd <- rbind(tdata,trdata)


# replacing activity number by activity label
# Make Text field into a factor field and change the levels from activity_labels.txt
t<-factor(fmasterd$Activity_Type)
levels(t)<-activity_labels$V2
# Inject the changes back into fmasterd
fmasterd$Activity_Type<-t



## Fmasterd still contains all 561 normally distributed data
## Lets extract the relevant fields using grep and search
## mean() and std() as these are the mean and standard deviation
## definitions as mentioned in features_info.txt
mean_field<-grep("mean\\(\\)",names(fmasterd))
std_field<-grep("std\\(\\)",names(fmasterd))
select_field<-sort(c(1,2,3,mean_field,std_field))
newdata<-fmasterd[,select_field]



##Creating a new dataset with average values for the variables in newdata dataframe
## such that it returns the mean values per subject per activity
## Since there are 30 subjects and 6 activities the returned data frame
## will have 180 rows for the 81 selected columns.
library(reshape2)
newdatamelt<-melt(newdata,id=c("Subject_ID","Activity_Type","Test_or_Train")
                  ,measure.vars=c(names(fmasterd[,mean_field])
                                  ,names(fmasterd[,std_field])))
newdf<-dcast(newdatamelt,Subject_ID + Activity_Type ~ variable,mean)



##Write the new dataframe newdf to a file
write.table(newdf,file = "./data/UCI_Tidydata.txt",row.names=FALSE)
