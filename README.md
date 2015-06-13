# Code book
## run_analytics.R
### Merges the training and the test sets to create one data set.
* Merge X data
* Merge y data
* Merge subject data
** Rename the column name from 'V1' to 'subject'   

### Reads features data 
* Lower the character cases and remove round brackets from the feature names    
* Extract standard deviation and mean feature names  
* Extract standard and mean features data from X  
* Rename the column names in X to standard/mean feature names  
  
### Reads Activities data  
* Removes "_" from the activity names and convert all characters to lowercase  
* Renames the column name from "V1" to "activityId"  
* Renames the column name from "V2" to "activity"
  
### Merges the corresponding data in activity labels based on y's data  
* Rename the column name from "V1" to "activity"  
  
### Writes the first tidy data set to mergedData.txt
### Used ply library to apply the function of getting mean/average based on subject & activity
### Writes the second tidy data set to averagedData.txt
