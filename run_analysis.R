# Merges the training and the test sets to create one data set.

xdata <- rbind(read.table("UCI HAR Dataset/train/X_train.txt"), 
               read.table("UCI HAR Dataset/test/X_test.txt"))
subject <- rbind(read.table("UCI HAR Dataset/train/subject_train.txt"), 
                 read.table("UCI HAR Dataset/test/subject_test.txt"))
ydata <- rbind(read.table("UCI HAR Dataset/train/y_train.txt"), 
               read.table("UCI HAR Dataset/test/y_test.txt"))

# Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("UCI HAR Dataset/features.txt")
indices <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
xdata <- xdata[, indices]
names(xdata) <- tolower(features[indices, 2])
names(xdata) <- gsub("-std\\(\\)", "-std", names(xdata))
names(xdata) <- gsub("-mean\\(\\)", "-mean", names(xdata))

# Uses descriptive activity names to name the activities in the data set

actlabels <- read.table("UCI HAR Dataset/activity_labels.txt")
actlabels[, 2] = tolower(as.character(actlabels[, 2]))

names(ydata) <- "activity"
ydata[, 1] = actlabels[ydata[, 1], 2]


# Appropriately labels the data set with descriptive variable names. 

names(subject) <- "subject"
mergedata <- cbind(subject, ydata, xdata)
write.table(mergedata, "merged_data.txt")

# creates a second, independent tidy data set with the average of each variable for each activity and each subject

unique_subjects = unique(subject)[, 1]
num_subjects = length(unique_subjects)

num_act = length(actlabels[, 1])
num_cols = dim(mergedata)[2]

num_mergedata = num_subjects * num_act
r = mergedata[1:num_mergedata, ]

n = 1
for (s in 1:num_subjects) {
  sub <- unique_subjects[s]
  for (a in 1:num_act) {
    actlabel = actlabels[a, 2]
    r[n, 1] = sub
    r[n, 2] = actlabel
    extracted <- mergedata[mergedata$subject == s & 
                           mergedata$activity == actlabel, ]
    r[n, 3:num_cols] <- colMeans(extracted[, 3:num_cols])
    n = n + 1
  }
}

write.table(r, "2nd_data_set.txt", row.name=FALSE)
