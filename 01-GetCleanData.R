library(openxlsx)
library(mscstexta4r)

textaInit()
data_clean <- as.data.frame(matrix(1, nrow(data_raw)))
data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
                     "Response Data_Working")
data_clean$comment.overall <- as.character(data_raw[,168])
df <- as.data.frame(data_clean$comment.overall)

data_clean$comment.overall <- WUTIL$getSentiment(df, key = NULL, max = 100)
