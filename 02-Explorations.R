##An R Script for ideas that we are exploring

####################PLOTTING EXPLORATION####################
##Back up data frames for messing with
bananas <- data_clean
monkeysUncle <- sentdf

##Trying to remove the NA values, but doesn't work because each one has a unique element id.
# monkeysUncle <- unique(monkeysUncle) 
monkeysUncle <- monkeysUncle[complete.cases(monkeysUncle), ]

##Exploring ways to best visualize the data
pltName <- ggplot(sentdf, aes(sentiment,word_count, color = sentType)) + geom_point()
pltName         ##Not great. Really messy because of outliers.

pltMU <- ggplot(monkeysUncle, aes(word_count,sentiment, color = sentiment)) + geom_point()
pltMU           ##Hard to read

pltName2 <- ggplot(sentdf, aes(sentScore, word_count, color=sentType)) + geom_point()
pltName2        ##Best representation of the data so far
##I want to explore a way to count the exact number of positive, negative and neutral
##I also think we should remove any comments that have literal "n/a" or wrote "none"
##Exploring a way to normalize the data and make sure it is scaled between -1 and +1


####################Social Networking Analysis exploration####################
##packages sna and igraph will be used here
##Watched a Lynda tutorial on it



