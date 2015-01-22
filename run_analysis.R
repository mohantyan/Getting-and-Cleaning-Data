# 1. Merges the training and the test sets to create one data set.

train <- read.table("train/X_train.txt")
test <- read.table("test/X_test.txt")
X_merge <- rbind(train, test)

train <- read.table("train/subject_train.txt")
test <- read.table("test/subject_test.txt")
S_merge <- rbind(train, test)

train <- read.table("train/y_train.txt")
test <- read.table("test/y_test.txt")
Y_merge <- rbind(train, test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
required_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X_merge <- X_merge[, required_features]
names(X_merge) <- features[required_features, 2]
names(X_merge) <- gsub("\\(|\\)", "", names(X_merge))
names(X_merge) <- tolower(names(X_merge))

# 3. Uses descriptive activity names to name the activities in the data set.

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Y_merge[,1] = activities[Y_merge[,1], 2]
names(Y_merge) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(S_merge) <- "subject"
merged_data <- cbind(S_merge, Y_merge, X_merge)
write.table(cleaned, "merged_data.txt")

# 5. Creates independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(merged_data)[2]
final = merged_data[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    final[row, 1] = uniqueSubjects[s]
    final[row, 2] = activities[a, 2]
    tmp <- merged_data[merged_data$subject==s & merged_data$activity==activities[a, 2], ]
    final[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(final, "merged_data_with_averages.txt")