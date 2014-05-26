GettingAndClearingData
======================
This document explains how run_analysis.R works:

1. Load required libraries

library("data.table")
library("reshape2")

2. Read subject data

dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

3. Read activity data

dtYTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtYTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

4. Read data files

fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtXTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtXTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

5. Join subject and activity data

dtSubject <- rbind(dtSubjectTrain, dtSubjectTest)
setnames(dtSubject, "V1", "subject")
dtActivity <- rbind(dtYTrain, dtYTest)
setnames(dtActivity, "V1", "activityNum")
dt <- rbind(dtXTrain, dtXTest)
dtSubject <- cbind(dtSubject, dtActivity)
dt <- cbind(dtSubject, dt)
setkey(dt, subject, activityNum)

6. "features.txt" file indicates which measurement execute for each feature

dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

7. Subset variables

select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

8. Descriptive activity names

dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))
dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)
dt$ACTIVITY <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

9. Separate features from featureName

doGrep <- function(regex) {
  grepl(regex, dt$feature)
}

10. Features with 2 categories

n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(doGrep("^t"), doGrep("^f")), ncol = nrow(y))
dt$DOMAIN <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(doGrep("Acc"), doGrep("Gyro")), ncol = nrow(y))
dt$INSTRUMENT <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(doGrep("BodyAcc"), doGrep("GravityAcc")), ncol = nrow(y))
dt$POWEREDBY <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(doGrep("mean()"), doGrep("std()")), ncol = nrow(y))
dt$VARIABLE <- factor(x %*% y, labels = c("Mean", "SD"))

11. Features with 1 category

dt$JERK <- factor(doGrep("Jerk"), labels = c(NA, "Jerk"))
dt$MAGNITUDE <- factor(doGrep("Mag"), labels = c(NA, "Magnitude"))

12. Features with 3 categories

n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(doGrep("-X"), doGrep("-Y"), doGrep("-Z")), ncol = nrow(y))
dt$AXIS <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

13. Output to tidy data set

setkey(dt, subject, ACTIVITY, DOMAIN, POWEREDBY, INSTRUMENT, JERK, MAGNITUDE, VARIABLE, AXIS)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
head(dtTidy)

14. CSV output for deployment

write.csv(dtTidy, "tidy.csv")

