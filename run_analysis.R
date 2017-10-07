##1.Merges the training and the test sets to create one data set.
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url,"./data.zip",method="curl")
unzip("data.zip")
list.files()
list.files("./UCI HAR Dataset")

setwd("./UCI HAR Dataset")
train.x <- read.table("./train/X_train.txt")
train.y <- read.table("./train/Y_train.txt")
train.subject <- read.table("./train/subject_train.txt")
test.x <- read.table("./test/X_test.txt")
test.y <- read.table("./test/Y_test.txt")
test.subject <- read.table("./test/subject_test.txt")

trainData <- cbind(train.x,train.y,train.subject)
testData <- cbind(test.x,test.y,test.subject)

fullData <- rbind(trainData,testData)

##2. Extracts only the measurements on the mean and standard deviation for each measurement.
featuresName <- read.table("./features.txt",stringsAsFactors = FALSE) [,2]
featuresIndex <- grep(("mean\\(\\)|std\\(\\)"), featuresName)

finalData <-fullData[,c(featuresIndex,562,563)]
colnames(finalData)<-c(featuresName[featuresIndex],"activity","subject")

##3. Uses descriptive activity names to name the activities in the data set
activityNames <- read.table("./activity_labels.txt")
finalData$activity <- factor(finalData$activity,
                             levels=activityNames[,1],
                             labels=activityNames[,2])

##4. Appropriately labels the data set with descriptive variable names.
names(finalData) <- gsub("\\()","",names(finalData))
names(finalData) <- gsub("^t","time",names(finalData))
names(finalData) <- gsub("^f","frequency",names(finalData))
names(finalData) <- gsub("-mean","Mean",names(finalData))
names(finalData) <- gsub("-std","Std",names(finalData))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
groupData <- finalData %>% 
        group_by(activity,subject)%>% 
        summarize_all(funs(mean))

write.table(groupData,"./MeanData.txt",row.names = FALSE)





