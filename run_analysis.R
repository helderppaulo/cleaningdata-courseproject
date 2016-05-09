tidyDataset <- function() {
	builddf <- function(dataPath, activityPath, subjectPath, labels, activityMap, type) {
		rawdata <- readLines(dataPath)
		splitted <- sapply(rawdata, function(x) { strsplit(gsub("^[ ]+", "", x), " +") })
		df <- as.data.frame(splitted, stringsAsFactors = FALSE)
		df <- as.data.frame(t(df), stringsAsFactors = FALSE)
		df <- sapply(df, as.numeric)
	
		cleanlabels <- as.character(gsub("^[0-9]{1,3} ","", labels))
		colnames(df) <- cleanlabels
		filteredColumns <- grep("[mM][eE][aA][nN]\\(\\)|[sS][tT][dD]\\(\\)", cleanlabels)
		df <- df[,filteredColumns]

		activity <- readLines(activityPath)
		activity <- sapply(activity, function(x) { activityMap[as.numeric(x)] } )
		df <- cbind.data.frame(df, activity)
		df <- cbind.data.frame(df, type = rep(type, nrow(df)))
		df <- cbind.data.frame(df, subject = readLines(subjectPath))
		df
	}

	activityMap <- readLines("UCI HAR Dataset/activity_labels.txt")
	activityMap <- sapply(activityMap, function(x) { tolower(gsub("^[1-6] ", "", x)) })
	names(activityMap) <- NULL
	labels <- readLines("UCI HAR Dataset/features.txt")
	
	testdf <- builddf("UCI HAR Dataset/test/X_test.txt", "UCI HAR Dataset/test/y_test.txt", "UCI HAR Dataset/test/subject_test.txt", labels, activityMap, "test")
	traindf <- builddf("UCI HAR Dataset/train/X_train.txt", "UCI HAR Dataset/train/y_train.txt", "UCI HAR Dataset/train/subject_train.txt", labels, activityMap, "training")

	rbind(testdf, traindf)
}

groupBySubjectAndActivity <- function(df) {
	grouped <- aggregate(df[1:66], list(activity = df$activity, subject = df$subject), mean)
	grouped$subject <- as.numeric(grouped$subject)
	grouped[order(grouped$activity,grouped$subject),]
}
