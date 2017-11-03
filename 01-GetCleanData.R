library(openxlsx)
library(mscstexta4r)

##Call the API by logging in with the URL and Key
##Values passed here from file .mscskeys.json
textaInit()

##Load raw data from excel file
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")

##Create a blank data frame to put scrubbed data into
data_clean <- as.data.frame(matrix(1, nrow(data_raw)))

##Make a new column in the "clean" data frame that holds the qualitative data
data_clean$comment.overall <- as.character(data_raw[,168])

##This needs to be developed and wrapped into a function
comment <- data_clean$comment.overall[16:17]

comment <- strsplit(comment, "\\.|\\?|\\!")
comment <- lapply(comment, function(x) x <- x[!x == ""])

results <- textaDetectTopics(comment[[2]])

