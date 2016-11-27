
run_analyis <- function()
{
 
#Load packages
        library(plyr)
        
#Common to test and train - features(column names)
        featuresmetadata <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\features.txt")
        #Get the column names as a vector
        featurecolumnnames <- as.vector(featuresmetadata$V2)
        
 #Test data
        #Read Subject, Activity and Features
        
        testsubject <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\test\\subject_test.txt")
        colnames(testsubject) <- c("subject")
        
        testactivity <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\test\\y_test.txt")
        colnames(testactivity) <- c("activityid")
        #Replace abbreviated "activityid" with "activityname"
        testactivity <- mutate(testactivity, 
                               activityname = ifelse(activityid == 1, "WALKING", 
                                                     ifelse(activityid == 2, "WALKING_UPSTAIRS",
                                                            ifelse(activityid == 3, "WALKING_DOWNSTAIRS",
                                                                   ifelse(activityid == 4, "SITTING",
                                                                          ifelse(activityid == 5, "STANDING",
                                                                                 "LAYING"))))))
        
        #Read features
        testfeatures <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\test\\X_test.txt")
        
        #Assign the column names to "testfeatures" to replace V1, V2...V561
                  colnames(testfeatures) <- c(featurecolumnnames)
        #Create a new dataset consists of the columns with either mean or std
                  testfeaturesrefined <- testfeatures[, grep("mean|std", names(testfeatures))]
        #bring subject, activity and features into one dataset
                  testdataset <- cbind(testsubject, activityname = testactivity$activityname, testfeaturesrefined)
        

 #Train data
        #Read Subject, Activity and Features
        
        trainsubject <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\train\\subject_train.txt")
        colnames(trainsubject) <- c("subject")
        #Replace abbreviated "activityid" with "activityname"
        trainactivity <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\train\\y_train.txt")
        colnames(trainactivity) <- c("activityid")
        trainactivity <- mutate(trainactivity, 
                                activityname = ifelse(activityid == 1, "WALKING", 
                                                      ifelse(activityid == 2, "WALKING_UPSTAIRS",
                                                             ifelse(activityid == 3, "WALKING_DOWNSTAIRS",
                                                                    ifelse(activityid == 4, "SITTING",
                                                                           ifelse(activityid == 5, "STANDING",
                                                                                  "LAYING"))))))
        
        
        #Read features
        trainfeatures <- read.table("C:\\Users\\admin\\Documents\\DataScience_JohnsHopkins\\week4\\UCI HAR Dataset\\train\\X_train.txt")
        
        
        #Assign the column names to "trainfeatures" to replace V1, V2...V561
        colnames(trainfeatures) <- c(featurecolumnnames)
        #Create a new dataset consists of the columns with either mean or std
        trainfeaturesrefined <- trainfeatures[, grep("mean|std", names(trainfeatures))]
        #bring subject, activity and features into one dataset
        traindataset <- cbind(trainsubject, activityname = trainactivity$activityname, trainfeaturesrefined)
        

 #Join "Test" and "Train" datasets vertically
        
        totaldata <- rbind(testdataset, traindataset)
        
        colnames(totaldata) = c("subject",
                                "activityname",
                                "Body Acceleration Mean in X direction (Time Domain)",
                                "Body Acceleration Mean in Y direction (Time Domain)",
                                "Body Acceleration Mean in Z direction (Time Domain)",
                                "Body Acceleration Standard Deviation in X direction (Time Domain)",
                                "Body Acceleration Standard Deviation in Y direction (Time Domain)",
                                "Body Acceleration Standard Deviation in Z direction (Time Domain)",
                                "Gravity Acceleration Mean in X direction (Time Domain)",
                                "Gravity Acceleration Mean in Y direction (Time Domain)",
                                "Gravity Acceleration Mean in Z direction (Time Domain)",
                                "Gravity Acceleration Standard Deviation in X direction (Time Domain)",
                                "Gravity Acceleration Standard Deviation in Y direction (Time Domain)",
                                "Gravity Acceleration Standard Deviation in Z direction (Time Domain)",
                                "Body Acceleration Jerk Mean in X direction (Time Domain)",
                                "Body Acceleration Jerk Mean in Y direction (Time Domain)",
                                "Body Acceleration Jerk Mean in Z direction (Time Domain)",
                                "Body Acceleration Jerk Standard Deviation in X direction (Time Domain)",
                                "Body Acceleration Jerk Standard Deviation in Y direction (Time Domain)",
                                "Body Acceleration Jerk Standard Deviation in Z  direction (Time Domain)",
                                "Body Gyro Mean in X direction (Time Domain)",
                                "Body Gyro Mean in Y direction (Time Domain)",
                                "Body Gyro Mean in Z direction (Time Domain)",
                                "Body Gyro Standard Deviation in X direction (Time Domain)",
                                "Body Gyro Standard Deviation in Y direction (Time Domain)",
                                "Body Gyro Standard Deviation in Z direction (Time Domain)",
                                "Body Gyro Jerk Mean in X direction (Time Domain)",
                                "Body Gyro Jerk Mean in Y direction (Time Domain)",
                                "Body Gyro Jerk Mean in Z direction (Time Domain)",
                                "Body Gyro Jerk Standard Deviation in X direction (Time Domain)",
                                "Body Gyro Jerk Standard Deviation in Y direction (Time Domain)",
                                "Body Gyro Jerk Standard Deviation in Z direction (Time Domain)",
                                "Body Accelaration Mag Mean (Time Domain)",
                                "Body Accelaration Mag Standard Deviation (Time Domain)",
                                "Gravity Accelaration Mag Mean (Time Domain)",
                                "Gravity Accelaration Mag Standard Deviation (Time Domain)",
                                "Body Accelaration Jerk Mag Mean (Time Domain)",
                                "Body Accelaration Jerk Mag Standard Deviation (Time Domain)",
                                "Body Gyro Mag Mean (Time Domain)",
                                "Body Gyro Mag Standard Deviation (Time Domain)",
                                "Body Gyro Jerk Mag Mean (Time Domain)",
                                "Body Gyro Jerk Mag Standard Deviation (Time Domain)",
                                "Body Acceleration Mean in X direction (Frequency Domain)",
                                "Body Acceleration Mean in Y direction (Frequency Domain)",
                                "Body Acceleration Mean in Z direction (Frequency Domain)",
                                "Body Acceleration Standard Deviation in X direction (Frequency Domain)",
                                "Body Acceleration Standard Deviation in Y direction (Frequency Domain)",
                                "Body Acceleration Standard Deviation in Z direction (Frequency Domain)",
                                "Body Acceleration Mean Frequency in X direction (Frequency Domain)",
                                "Body Acceleration Mean Frequency in Y direction (Frequency Domain)",
                                "Body Acceleration Mean Frequency in Z direction (Frequency Domain)",
                                "Body Acceleration Jerk Mean in X direction (Frequency Domain)",
                                "Body Acceleration Jerk Mean in Y direction (Frequency Domain)",
                                "Body Acceleration Jerk Mean in Z direction (Frequency Domain)",
                                "Body Acceleration Jerk Standard Deviation in X direction (Frequency Domain)",
                                "Body Acceleration Jerk Standard Deviation in Y direction (Frequency Domain)",
                                "Body Acceleration Jerk Standard Deviation in Z direction (Frequency Domain)",
                                "Body Acceleration Jerk MeanFrequency in X direction (Frequency Domain)",
                                "Body Acceleration Jerk MeanFrequency in Y direction (Frequency Domain)",
                                "Body Acceleration Jerk MeanFrequency in Z direction (Frequency Domain)",
                                "Body Gyro Mean in X direction (Frequency Domain)",
                                "Body Gyro Mean in Y direction (Frequency Domain)",
                                "Body Gyro Mean in Z direction (Frequency Domain)",
                                "Body Gyro Standard Deviation in X direction (Frequency Domain)",
                                "Body Gyro Standard Deviation in Y direction (Frequency Domain)",
                                "Body Gyro Standard Deviation in Z  direction (Frequency Domain)",
                                "Body Gyro MeanFrequency in X direction (Frequency Domain)",
                                "Body Gyro MeanFrequency in Y direction (Frequency Domain)",
                                "Body Gyro MeanFrequency in Z direction (Frequency Domain)",
                                "Body Accelaration Mag Mean (Frequency Domain)",
                                "Body Accelaration Mag Standard Deviation (Frequency Domain)",
                                "Body Accelaration Mag MeanFrequency (Frequency Domain)",
                                "Body Body Acceleration Jerk Mag Mean (Frequency Domain)",
                                "Body Body Acceleration Jerk Mag Standard Deviation (Frequency Domain)",
                                "Body Body Acceleration Jerk Mag MeanFrequency (Frequency Domain)",
                                "Body Body Gyro Mag Mean (Frequency Domain)",
                                "Body Body Gyro Mag Standard Deviation (Frequency Domain)",
                                "Body Body Gyro Mag MeanFrequency (Frequency Domain)",
                                "Body Body Gyro Jerk Mean (Frequency Domain)",
                                "Body Body Gyro Jerk Mag Standard Deviation (Frequency Domain)",
                                "Body Body Gyro Jerk Mag MeanFrequency (Frequency Domain)")
        

#Create a dataset group by subject, activity( calculate mean for each of the columns)
        finaldataset <- group_by(totaldata, subject, activityname) %>% summarise_each(funs(mean)) 
        finaldataset
}