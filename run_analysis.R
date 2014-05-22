## Getting and Cleaning Data - course project file
run_analysis <- function () {
	# Read the training and test data
	train <- read.table("UCI HAR Dataset\\train\\x_train.txt")
	test <- read.table("UCI HAR Dataset\\test\\x_test.txt")

	# base the column names on the translation provided in the features.txt file
	l <- read.table("UCI HAR Dataset\\features.txt")
	names(train) <- l[,2]
	names(test) <- l[,2]

	# Append the volunteer code to each data frame
	v <- read.table("UCI HAR Dataset\\test\\subject_test.txt")
	test$volunteer <- v$V1

	v <- read.table("UCI HAR Dataset\\train\\subject_train.txt")
	train$volunteer <- v$V1

	# Append the activity keys to each data frame
	a <- read.table("UCI HAR Dataset\\test\\y_test.txt")
	test$activitykey <- a$V1

	a <- read.table("UCI HAR Dataset\\train\\y_train.txt")
	train$activitykey <- a$V1

## 1. Merge the training and test data sets	
	d <- rbind(train, test)
	
 
## 2. Extract the measurements on the mean and sd of each variable
# and get the subject and activity columns	
	f1 <- d[1:6] 		# tBodyAcc
	f2 <- d[41:46]		# tGravityAcc
	f3 <- d[81:86]		# tBodyAccJerk
	f4 <- d[121:126]		# tBodyGyro
	f5 <- d[161:166]		# tBodyGyroJerk
	f6 <- d[c(201,202,214,215,227,228,240,241,253,254)] # above 5 Mag
	f7 <- d[266:271]		# fBodyAcc
	f8 <- d[345:350]		# fBodyAccJerk
	f9 <- d[424:429]		# fBodyGyro
	f10 <- d[c(503,504,516,517,529,530,542,543,562,563)] # above 3 Mag + labels

	wd <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10) # combine selected fields

	# clean up the column names
	names(wd) <- tolower(names(wd))
	names(wd) <- gsub("fbody","frequencyBody", names(wd), fixed=TRUE)
	names(wd) <- gsub("tbody","timeBody", names(wd), fixed=TRUE)
	names(wd) <- gsub("tgravity","timeGravity", names(wd), fixed=TRUE)
	names(wd) <- gsub("std","Std", names(wd), fixed=TRUE)
	names(wd) <- gsub("mean","Mean", names(wd), fixed=TRUE)
	names(wd) <- gsub("-","", names(wd), fixed=TRUE)
	names(wd) <- gsub("(","", names(wd), fixed=TRUE)
	names(wd) <- gsub(")","", names(wd), fixed=TRUE)

## 3 & 4. Label and factor the activity names by merging on activity key
	a <- read.table("UCI HAR Dataset\\activity_labels.txt")
	names(a) <- c("activitykey", "activity")
	wd <- merge(x = wd, y = a, by = "activitykey", all = TRUE)

## 5. Create ds with average by variable paritioned by activity and volunteer

	out <- data.frame(t(rep(NA, 4)))
	
	act <- as.character(unique(wd$activity))
	vol <- unique(wd$volunteer)
	var <- names(wd[2:67])
	for (ac in act) {
		for (vo in vol) {
			t <- subset(wd, wd$activity == ac & wd$volunteer == vo)
			for (i in 2:67) {
				v <- names(wd[i])
				m <- mean(t[,i])
				out <- rbind(out, c(vo, ac, v, m))
				#print(c(vo, ac, v, m))
			}
		}
	}
	out <- out[-1,]
	names(out) <- c("Volunteer", "Activity", "Variable", "Average")
	out$Average <- as.numeric(out$Average)

write.table(out, "HAR_tidydata.txt")
#out
}





