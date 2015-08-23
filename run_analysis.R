
### Run analysis 
### CREATED BY : Simeon Ayo BABATUNDE 
###  - for coursera Getting and Cleaning 

## setwd("E:/Coursera/cleaning data/data")  -- set working directory 


### step 1 : Merging the training and test dataset into one dataset 
##            Read testdata, train data all X, Y and the subject 
##            merging it into a single dataset

#################################
##   reading the test data set  3 Files 

folder <- "UCI HAR Dataset/test/"
file <- paste(folder, "X_test.txt", sep="")
testx <- read.table(file)
file <- paste(folder, "y_test.txt", sep="")
testy <- read.table(file)
file <- paste(folder, "subject_test.txt", sep="")
testsub <- read.table(file)

## reading the training data sets ######
folder <- "UCI HAR Dataset/train/"
file <- paste(folder, "X_train.txt", sep="")
trainx <- read.table(file)
file <- paste(folder, "y_train.txt", sep="")
trainy <- read.table(file)
file <- paste(folder, "subject_train.txt", sep="")
trainsub <- read.table(file)

## Merging the data sets ###
all <- rbind(testx, trainx) ## Test & train is merged all 561 columns resulting 10299 observations
all[, "activity.labels"] <- rbind(testy, trainy)  ## activity labels added 
all[, "subject"] <- rbind(testsub, trainsub)      ## subject added 


### step 2 : Extracts only the measurements on the mean and standard deviation for each measurement. 
##            Read features and filter the data frame to eleminate variable names 
##            other than mean & std. add the column names for activity labels and subject
##            create a logical vector to filter the dataframe
##            using logical vector filter the data frame features to get a filtered data frame
##            using the filtered dataframe and apply it to the all data frame to get the 
##            allmeanstd dataframe 


######  -- loading the features and filtering for mean and standard deviation
file <- "UCI HAR Dataset/features.txt"
features <- read.table(file, stringsAsFactors=FALSE)  ## loading features df
features <- rbind(features, list(562, "activity.labels")) ## adding activity labels name to 562nd subscript
features <- rbind(features, list(563, "subject")) ## adding subject 563rd subscript

## creating logcal vectors for the required columns of features 
index1 <- with(features, grepl("-mean\\(", V2))  ## -- logical vector for the word -mean in the features 
index2 <- with(features, grepl("-std\\(", V2))   ## -- logical vector for the word -std in the features 

## adding last 2 variables as they are for activity and subject should be TRUE
index1[562] <- TRUE
index1[563] <- TRUE

## applying two indexes to features the results will be only mean & std variables 
## and their column position in the all data set.
f1 <- features[(index1 | index2), ]  ## f1 has only the mean & std variable indice
allmeanstd <- all[f1[[1]]]  ##  all mean and std observations alone seperated and has 68 columns

### step 3 & 4 : cleaning up the loaded names and apply the names to the column names
##                of allmeanstd and merge them to the descriptive data set of activity names
#3                at end produces the data frame allmeanstd with names and activity names.
## 


## adding another column in f1 for normalizing the names without braces and dash ## this is to use for labeling the columns 

## creating the additional column in f1 for cleaning it up 
f1[3] <- f1[2]  
## cleaning all - with . and removing all braces 
f1[3] <- lapply(f1[3], function(x) gsub('\\-', '.', gsub('\\(|\\)', '', x)) ) 

## loaing the normalized names into a vector called colnames
## removing row names 
colnames <- f1[3]
row.names(colnames) <- NULL 
colnames <- rbind(colnames)  ## as a data frame

# setting the names of allmeanstd with column names. 
# including activity labels and subject 
names(allmeanstd) <- colnames[[1]]  

## Activity descriptive names reading 
file <- "UCI HAR Dataset/activity_labels.txt"   ## activity labels file
alab <- read.table(file)                        ## reading it as dataset
names(alab) <- c("activity.label", "activity.name") ## changing the heading 

## Uses descriptive activity names to name the activities in the data set
## By using merge command

## Where the activity code is also translated into descriptions
final <- merge(alab, allmeanstd, by.x="activity.label", by.y="activity.labels") 

## using descriptive name and labeling the variables appropriately is done.



### step 5 : Creates a second, independent tidy data set with the average of 
##           each variable for each activity and each subject.
##           
##           dataset used further is called final  which is the result of last expression
##           other datasets are not required for further activity
## 
##           a) using the allmeanstd and reshape2 library 
##           b) using melt making it narrow dataset
##           c) it can be used further to generate a wide tidy dataset 
##           d) or narrow tidy dataset



library(reshape2)  ## this is the library for melt and dcast functions 


## using melt and ids are activity label, activity name and subject.  
##                    by default the rest of the variables are consdered as measures.
##                    in our dataset its true so no need to mention them 
## 

meltfinal   <- melt(final, id=c("activity.label", "activity.name", "subject") ) 

## wide average dataset created with 180 observations with 69 variables [ 3 ids & 66 measures ]
## the dcast uses the mean function which produces the average  of the mearusres on the 3 ids
widetidyaverage <- dcast(meltfinal, activity.label + activity.name + subject ~ variable, mean ) 

## narrow data with 5 variables and 11880 observations, 
##  this dataset has each measure into a single obervation 
##  the mean function produces the aggrated average for activity and subject and variable
##  
narrowtidyaverage <- aggregate(value ~ activity.label + activity.name + subject + variable, data = meltfinal, mean)


## for the purpose of documentation  tidy dataset narrow is choosen. 
##     writing the data to the file

## writing the file for uploading it into project  file name tidy_dataset.txt
write.table(narrowtidyaverage, "tidy_dataset.txt", col.names = FALSE, row.names=FALSE)





