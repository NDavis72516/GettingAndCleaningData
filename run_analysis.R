#reading in the metadata from unzipped folder in directory on pc
features<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/features.txt")
activity<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#reading in training data
features_train<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/train/X_train.txt")
activity_train<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/train/subject_train.txt")

#reading in test data
features_test<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/test/X_test.txt")
activity_test<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("C:/Users/davis450/Desktop/Coursera/UCI HAR Dataset/test/subject_test.txt")

#merging training and test data 
subject_full<- rbind(subject_train, subject_test)
activity_full<- rbind(activity_train, activity_test)
features_full<- rbind(features_train, features_test)

#assigning column names
colnames(features_full)<-t(features[2])
colnames(activity_full)<- "Activity"
colnames(subject_full)<-"Subject"

#merging features_full, activity_full, and subject_full into one dataset
complete_dataset<-cbind(features_full, activity_full, subject_full)

#extracting only the measurements on the mean and standard deviation for each measurement
mean_and_std <-grep(".*Mean.*|.*Std.*", names(complete_dataset), ignore.case=TRUE)
required <- c(mean_and_std, 562, 563)
extracted <- complete_dataset[,required]

#using descriptive activity names to name the activities in the data set
extracted$Activity <- as.character(extracted$Activity)
for(i in 1:6){
  extracted$Activity[extracted$Activity ==i] <-as.character(activity[i,2])
}
extracted$Activity<-as.factor(extracted$Activity)              

#Appropriately labeling the data set with descriptive variable names
names(extracted)<-gsub("BodyBody", "Body", names(extracted))
names(extracted)<-gsub("angle","Angle", names(extracted))
names(extracted)<-gsub("gravity", "Gravity", names(extracted))
names(extracted)<-gsub("-mean()", "Mean", names(extracted))
names(extracted)<-gsub("-std()","STD", names(extracted))
names(extracted)<-gsub("-freq()", "Frequency", names(extracted))
names(extracted)<-gsub("tbody", "TimeBody", names(extracted))
names(extracted)<-gsub("^t", "Time", names(extracted))
names(extracted)<-gsub("^f", "Frequency", names(extracted))
names(extracted)<-gsub("Mag", "Magnitude", names(extracted))

#Using dataset above, creates a second independent tidy dataset with the average of each 
#variable and subject
extracted$Subect<-as.factor(extracted$Subject)
extracted<-data.table(extracted)
tidy_dataset<- aggregate(. ~Subject + Activity, extracted, mean)
tidy_dataset<- tidy_dataset[order(tidy_dataset$Subject, tidy_dataset$Activity),]

#Writing the tidy dataset to a txt file
write.table(tidy_dataset, file="Tidy_Dataset.txt", row.names=FALSE)
