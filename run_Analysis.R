#run_Analysis.R

#Objective 1: Merges the training and the test sets to create one data set.
#Objective 2: Extracts only the measurements on the mean and standard deviation for each measurement. 
#Objective 3: Uses descriptive activity names to name the activities in the data set
#Objective 4: Appropriately labels the data set with descriptive variable names. 
#Objective 5: From the data set in step 4, creates a second, independent tidy data set with the 
#              average of each variable for each activity and each subject.

#check to see if folder called data exists.  if not, then it creates the folder and downloads the
#   zip to data and unzips the files into data's parent folder. 
if(!file.exists("./data")){dir.create("./data")}
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileURL, "data/Dataset.zip", method = "curl")
unlink(temp)
unzip("data/Dataset.zip")

#reads the train data sets into R
subject_train <- read.table("UCI Har Dataset/train/subject_train.txt")
X_train <- read.table("UCI Har Dataset/train/X_train.txt")
y_train <- read.table("UCI Har Dataset/train/y_train.txt")

#reads the test data sets into R
subject_test <- read.table("UCI Har Dataset/test/subject_test.txt")
X_test <- read.table("UCI Har Dataset/test/X_test.txt")
y_test <- read.table("UCI Har Dataset/test/y_test.txt")

#reads features.txt (which provides variable descriptions) into R and then 
#fixes them as colnames in both X files
features <- read.table("UCI Har Dataset/features.txt")
descr <- paste(features[,2])
names(X_test) <- c(descr[1:length(descr)])
names(X_train) <- c(descr[1:length(descr)])             ## Objective 4 Complete

#specifies activities for test set and sets column names
y_test$V1 <- gsub("1", "WALKING", y_test$V1)
y_test$V1 <- gsub("2", "WALKING_UPSTAIRS", y_test$V1)
y_test$V1 <- gsub("3", "WALKING_DOWNSTAIRS", y_test$V1)
y_test$V1 <- gsub("4", "SITTING", y_test$V1)
y_test$V1 <- gsub("5", "STANDING", y_test$V1)
y_test$V1 <- gsub("6", "LAYING", y_test$V1) 
names(y_test) <- c("Activity")
names(subject_test) <- c("Subject_ID")

#specifies activities for train set and sets column names
y_train$V1 <- gsub("1", "WALKING", y_train$V1)
y_train$V1 <- gsub("2", "WALKING_UPSTAIRS", y_train$V1)
y_train$V1 <- gsub("3", "WALKING_DOWNSTAIRS", y_train$V1)
y_train$V1 <- gsub("4", "SITTING", y_train$V1)
y_train$V1 <- gsub("5", "STANDING", y_train$V1)
y_train$V1 <- gsub("6", "LAYING", y_train$V1)
names(y_train) <- c("Activity")                         ## Objective 3 Complete 
names(subject_train) <- c("Subject_ID") 

#merges test set into one train database
Test <- cbind(subject_test, y_test, X_test)

#merges train set into one test database
Train <- cbind(subject_train, y_train, X_train)  

#merges Train and Test databases into one GalaxyS database
GalaxyS <- rbind(Test, Train)
GalaxyS <- GalaxyS[order(GalaxyS[,1]),]  
row.names(GalaxyS) <- NULL                              ## Objective 1 Complete

#Keep only columns which are the Std Dev or Mean of measurements
selectedCols <- intersect(grep("std()|mean()", colnames(GalaxyS)), grep("meanFreq()", 
    colnames(GalaxyS), invert=TRUE))
GalaxySTidy <- cbind(GalaxyS[1], GalaxyS[2], GalaxyS[,selectedCols])     ## Keeps only Std Dev and Mean cols
GalaxySTidy <- tbl_df(GalaxySTidy)                                      
GalaxySTidy <- arrange(GalaxySTidy, Subject_ID, Activity)   ## Sorts by Subject ID and then Activity.
                                                            ##Objective 2 Complete
group_byGalaxySTidy <- group_by(GalaxySTidy, Subject_ID, Activity)
GalaxySAverages <- group_byGalaxySTidy %>% 
    summarise_each(funs(mean))              
a <- "Avg of"
dfGalaxySAverages <- data.frame(GalaxySAverages)
for (i in 3:ncol(dfGalaxySAverages)) {
    names(dfGalaxySAverages)[i] <- paste(a,names(dfGalaxySAverages)[i])
}
## Objective 5 Complete
write.table(dfGalaxySAverages, file="dfGalaxySAverages.txt", row.names=FALSE)























