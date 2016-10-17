# the newest version is here:
# https://github.com/wscrdzg/cleaning-data

library(plyr)
library(dplyr)

# 1. Merges the training and the test sets to create one data set.
# import all data
feature <- read.table("./UCI_HAR_Dataset/features.txt")
x_test <- read.table("./UCI_HAR_Dataset/test/X_test.txt")
y_test <- read.table("./UCI_HAR_Dataset/test/y_test.txt")
sub_test <- read.table("./UCI_HAR_Dataset/test/subject_test.txt")
x_train <- read.table("./UCI_HAR_Dataset/train/X_train.txt")
y_train <- read.table("./UCI_HAR_Dataset/train/y_train.txt")
sub_train <- read.table("./UCI_HAR_Dataset/train/subject_train.txt")

colnames(x_test) <- feature[,2]
colnames(x_train) <- feature[,2]

# use one additional variable to distinct test and train data
# test == 0, train == 1
test_or_train0 <- as.data.frame(rep.int(0,2947))
colnames(test_or_train0) <- "test_0_train_1"
test_or_train1 <- as.data.frame(rep.int(1,7352))
colnames(test_or_train1) <- "test_0_train_1"

# add that y_test and y_train, those are the activity label
colnames(y_test) <- "activity_type"
colnames(y_train) <- "activity_type"
x_test1 <- cbind(sub_test,y_test,test_or_train0,x_test)
x_train1 <- cbind(sub_train,y_train,test_or_train1,x_train)

# combine test and train
x_all <- rbind(x_test1,x_train1)
rm(x_test1,x_train1)

# ================== to ===== separate ===== the ===== space =========================

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_all1 <- x_all[,grep("mean()", names(x_all))]
mean_all <- mean_all1[,-grep("meanFreq",names(mean_all1))]
rm(mean_all1)
std_all <- x_all[,grep("std()", names(x_all))]
mean_n_std <- cbind(x_all[,1:2],mean_all,std_all)

# ================== to ===== separate ===== the ===== space =========================

# 3. Uses descriptive activity names to name the activities in the data set.
activities <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
for(i in 1:6){
  mean_n_std$activity_type <- gsub(i,activities[i],mean_n_std$activity_type)
}

# ================== to ===== separate ===== the ===== space =========================

# 4. Appropriately labels the data set with descriptive variable names.
## already did in parts before

# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject. (Total 6*30 = 180 obs.)
mean_n_std1 <- mean_n_std[,-(1:2)]
mean_of_data <- aggregate(mean_n_std1,list(mean_n_std$V1,mean_n_std$activity_type),mean)
write.table(mean_of_data, file = "tidy_data.txt", row.names = F)
