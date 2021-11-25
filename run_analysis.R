#dplyr-packages=a grammar of data manipulation
install.packages('dplyr')
library(dplyr)

#Download dataset
fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,filename,method="curl")

#Combine all dataframe
features<-read.table("UCI HAR Dataset/features.txt",col.names=c("n","functions"))
activities<-read.table("UCI HAR Dataset/activity_labels.txt",col.names=c("code","activity"))
subject.test<-read.table("UCI HAR Dataset/test/subject.test.txt",col.names="subject")
x.test<-read.table("UCI HAR Dataset/test/x.test.txt",col.names=features$functions)
y.test<-read.table("UCI HAR Dataset/test/y.test.txt",col.names="code")
subject.train<-read.table("UCI HAR Dataset/train/subject.train.txt",col.names="subject")
x.train<-read.table("UCI HAR Dataset/train/x.train.txt",col.names=features$functions)
y.train<-read.table("UCI HAR Dataset/train/y.train.txt",col.names="code")

#Step 1: Merges the training and the test sets to create one data set
x<-rbind(x.train,x.test)
y<-rbind(y.train,y.test)
subject<-rbind(subject.train,subject.test)
combined<-cbind(subject,y,x)

#Step 2: Extracts only the measurements on the mean and standard deviation for each measurement
tidydata<-combined%>%select(subject,code,contains("mean"),contains("std"))

#Step 3: Uses descriptive activity names to name the activities in the data set
tidydata$code<-activities[tidydata$code,2]

#Step 4: Appropriately labels the data set with descriptive variable names
names(tidydata)[2]="activity"
names(tidydata)<-gsub("Acc","Accelerometer",names(tidydata))
names(tidydata)<-gsub("Gyro","Gyroscope",names(tidydata))
names(tidydata)<-gsub("BodyBody","Body",names(tidydata))
names(tidydata)<-gsub("Mag","Magnitude",names(tidydata))
names(tidydata)<-gsub("^t","Time",names(tidydata))
names(tidydata)<-gsub("^f","Frequency",names(tidydata))
names(tidydata)<-gsub("tBody","TimeBody",names(tidydata))
names(tidydata)<-gsub("-mean()","Mean",names(tidydata),ignore.case=TRUE)
names(tidydata)<-gsub("-std()","STD",names(tidydata),ignore.case=TRUE)
names(tidydata)<-gsub("-freq()","Frequency",names(tidydata),ignore.case=TRUE)
names(tidydata)<-gsub("angle","Angle",names(tidydata))
names(tidydata)<-gsub("gravity","Gravity",names(tidydata))

#Step 5: From the data set in step 4,creates a second,independent tidy data set with the average of each variable for each activity and each subject
datareadytoprocess<-tidydata%>%
group_by(subject,activity)%>%
summarise_all(funs(mean))
write.table(datareadytoprocess,"datareadytoprocess.txt",row.name=FALSE)

str(datareadytoprocess)
