getFeaturesMetaData <- function(interestIn = c("mean()", "std()")) {
  data <- read.csv("features.txt", sep = " ", header = F)
  colnames(data) <- c("featureId", "fullFeatureName")
  res <- data.frame(featureId = numeric(0), fullFeatureName = integer(0))
  label <- c()
  for (str in interestIn) {
    strFeature <- data[grep(str, data$fullFeatureName, fixed = T),]
    res <- rbind(res, strFeature)
  }
  res$label <- gsub("[\\(\\)\\-]", "", res$fullFeatureName)
  return(res)
}

tidySet <- function(dirs = c("test", "train")) {
  featureMetaData <- getFeaturesMetaData()
  rawLabels = paste("V", labels$featureId, sep = "")
  
  baseSize <- length(rawLabels)
  res = as.data.frame(setNames(replicate(baseSize + 2, numeric(0), simplify = F), c(featureMetaData$label, "subjectId", "activityName")))
  subjectIds = read.csv("./test/subject_test.txt")
  
  activityNames = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
  
  for (folder in dirs) {
    dataFilePath <- paste(".", folder, paste("X_", folder, ".txt", sep = ""), sep = "/")
    data <- read.csv(dataFilePath, sep = "", header = F, strip.white = T)
    tempRes <- data[rawLabels]
    colnames(tempRes) <- labels$label
    
    subjectFilePath <- paste(".", folder, paste("subject_", folder, ".txt", sep = ""), sep = "/")
    subjectIds <- read.csv(subjectFilePath, sep = "", header = F, strip.white = T)
    colnames(subjectIds) <- c("subjectId")
    
    activityIdsFilePath <- paste(".", folder, paste("y_", folder, ".txt", sep = ""), sep = "/")
    activityIds <- read.csv(activityIdsFilePath, sep = "", header = F, strip.white = T)
    activityNamesData <- as.data.frame(activityNames[activityIds$V1])
    colnames(activityNamesData) <- c("activityName")
    
    res <- rbind(res, cbind(tempRes, subjectIds, activityNamesData))
  }
  
  return(res)
}