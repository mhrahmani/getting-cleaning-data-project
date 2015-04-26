

# This is an R script to do these tasks:

# Merge the Train and Test data sets into one table using rbind. 

tbl1 <- read.table("train/X_train.txt")
tbl2 <- read.table("test/X_test.txt")
X <- rbind(tbl1, tbl2)

tbl1 <- read.table("train/subject_train.txt")
tbl2 <- read.table("test/subject_test.txt")
S <- rbind(tbl1, tbl2)

tbl1 <- read.table("train/y_train.txt")
tbl2 <- read.table("test/y_test.txt")
Y <- rbind(tbl1, tbl2)

# Pull out the Mean and Standard Deviation measurments for each individual measurment. 

features <- read.table("features.txt")
good_measures <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, good_measures]
names(X) <- features[good_measures, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# rename the activities to meaningful names

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y[,1] = activities[Y[,1], 2]
names(Y) <- "activity"

#labeling the data set with descriptive activity names.

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

# build a tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activities[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "tidy_dataset.txt")