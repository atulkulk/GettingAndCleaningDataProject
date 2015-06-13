  library(data.table)
  library(plyr)
  
  # 1. Merges the training and the test sets to create one data set.
  # Merge X data
  X_test <- read.table("UCI HAR Dataset/test/X_test.txt")  
  X_train <- read.table("UCI HAR Dataset/train/X_train.txt")  
  X <- rbind(X_test, X_train)
  
  # Merge y data
  y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
  y_train <- read.table("UCI HAR Dataset/train/y_train.txt")  
  y <- rbind(y_test, y_train)
  
  # Merge subject data
  subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")    
  subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")  
  subject <- rbind(subject_test, subject_train)
   
  # Rename the column name from 'V1' to 'subject'
  setnames(subject, "V1", "subject")

  # Read features data
  features <- read.table("UCI HAR Dataset/features.txt")
  
  # Lower the character cases and remove round brackets from the feature names
  featureNames <- gsub("\\(\\)", "", tolower(features[, 2]))
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  # Extract standard deviation and mean feature names
  extractStdMeanFeatures <- grep("(-mean-)|(-mean$)|(-std-)|(-std$)", featureNames)
  
  # Extract standard and mean features data from X
  X <- X[, extractStdMeanFeatures]
  
  # Rename the column names in X to standard/mean feature names
  names(X) <- featureNames[extractStdMeanFeatures]
  
  # Read Activities
  activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  
  # Remove "_" from the activity names and convert all characters to lowercase
  activitylabels[,2] <- gsub("_", "", tolower(activitylabels[,2]))
  
  # Rename the column name from "V1" to "activityId"
  setnames(activitylabels, "V1", "activityId")
  
  # Rename the column name from "V2" to "activity"
  setnames(activitylabels, "V2", "activity")  
  
  # Merge the corresponding data in activity labels based on y's data
  y[,1] = activitylabels[y[,1], 2]  
  
  # Rename the column name from "V1" to "activity"
  setnames(y, "V1", "activity")
  
  # 3. Uses descriptive activity names to name the activities in the data set
  # 4. Appropriately labels the data set with descriptive variable names.
  # Merge the entire data
  subjectXy <- cbind(subject, y, X)
  
  # Write to a file
  write.table(subjectXy, "mergedData.txt", row.names = FALSE)

  # 5. From the data set in step 4, creates a second, independent tidy data set
  # with the average of each variable for each activity and each subject.
  averageData <- ddply(subjectXy, .(subject, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
  
  # Write to a file
  write.table(averageData, "averagedData.txt", row.names = FALSE)