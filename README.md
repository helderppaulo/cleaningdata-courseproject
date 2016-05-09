# cleaningdata-courseproject

```{r echo = FALSE}
tidyDataset <- function() {
	builddf <- function(dataPath, activityPath, subjectPath, labels, activityMap, type) {
		# read raw data
		rawdata <- readLines(dataPath)
		# cleaning whitespace dirt and splitting by the whitespaces
		splitted <- sapply(rawdata, function(x) { strsplit(gsub("^[ ]+", "", x), " +") })
		# transform into a data frame		
		df <- as.data.frame(splitted, stringsAsFactors = FALSE)
		# transpose the data frame		
		df <- as.data.frame(t(df), stringsAsFactors = FALSE)
		# transform the data into numeric format (from string)		
		df <- sapply(df, as.numeric)
	
		# clean the labels index text
		cleanlabels <- as.character(gsub("^[0-9]{1,3} ","", labels))
		# fill the column names in the data frame
		colnames(df) <- cleanlabels
		# filter the labels that are not mean or standard deviation
		filteredColumns <- grep("[mM][eE][aA][nN]\\(\\)|[sS][tT][dD]\\(\\)", cleanlabels)
		# filter the columns in the data frame
		df <- df[,filteredColumns]

		# reads the activities raw data
		activity <- readLines(activityPath)
		# exchanges the identifier for the activity label
		activity <- sapply(activity, function(x) { activityMap[as.numeric(x)] } )
		# binds the activity, type and subject columns on the data frame
		df <- cbind.data.frame(df, activity)
		df <- cbind.data.frame(df, type = rep(type, nrow(df)))
		df <- cbind.data.frame(df, subject = readLines(subjectPath))
		df
	}

	activityMap <- readLines("UCI HAR Dataset/activity_labels.txt")
	activityMap <- sapply(activityMap, function(x) { tolower(gsub("^[1-6] ", "", x)) })
	names(activityMap) <- NULL
	labels <- readLines("UCI HAR Dataset/features.txt")

	# builds the training and test datasets	
	testdf <- builddf("UCI HAR Dataset/test/X_test.txt", "UCI HAR Dataset/test/y_test.txt", "UCI HAR Dataset/test/subject_test.txt", labels, activityMap, "test")
	traindf <- builddf("UCI HAR Dataset/train/X_train.txt", "UCI HAR Dataset/train/y_train.txt", "UCI HAR Dataset/train/subject_train.txt", labels, activityMap, "training")

	# merges the two datasets and returns
	rbind(testdf, traindf)
}

groupBySubjectAndActivity <- function(df) {
	# groups the dataset by activity and subject by averaging the observations in the same group
	grouped <- aggregate(df[1:66], list(activity = df$activity, subject = df$subject), mean)
	grouped$subject <- as.numeric(grouped$subject)
	# sorts the grouped data frame by both variables
	grouped[order(grouped$activity,grouped$subject),]
}
```

The tidyDataset function reads the test and train datasets, remove the unnecessary columns (the ones that are not mean or std) for both datasets and merges them adding a new column "type" distinguishing if the observation is a training or test data. It also transposes the data frame so the observations are listed as rows and each feature is a column. Therefore it adds the activity (translated into descriptive string) and subject columns.

The groupBySubjectAndActivity function receives a tidy dataset, created by the tidyDataset function, that groups the data by the activity then subject. And sorts it by the same columns recpectively.
