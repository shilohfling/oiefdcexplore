##An R Script for plotting the numerical data
library(openxlsx)
library(dplyr)
library(ggplot2)
library(likert)

##Don't need this part - can be found in 01-GetCleanData.R
#data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
#                      "Response Data_Working")
#NumAnalysis1 <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)

##Data frame for first section of questions
NumAnalysis1$admissions <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.a..Applying.to.and.being.admitted.by.Webster`
NumAnalysis1$orientation <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.b..Initial.orientation.to.Webster.and.my.program.of.study`
NumAnalysis1$academic.advising <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.c..Academic.advising`
NumAnalysis1$registering.for.classes <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.d..Registering.for.classes`
NumAnalysis1$financial.aid <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.e..Applying.for.and.receiving.financial.aid`
NumAnalysis1$paying.tuition <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.f..Paying.tuition.and.fees`
NumAnalysis1$library.resources <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.g..Accessing.and.using.library.resources`
NumAnalysis1$technology <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.h..Using.Webster.technology.to.assist.in.my.learning`
NumAnalysis1$writing.center <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.i..Accessing.and.using.the.Writing.Center`
NumAnalysis1$career.planning <- data_raw$`Please.rate.your.level.of.satisfaction.with.the.following.Webster.University.processes..-.j..Assistance.with.career.planning`


#####EXPLORATION FOR PLOTTING THE DATA#####
# #dataRange = as.data.frame(c(0:5))
# section1 <- ggplot(NumAnalysis1, aes(x=admissions)) + geom_bar()
# section1
# section1q <- hist(NumAnalysis1$admissions)
# section1q
# ##Try likert package
# ?likert
# ?plot.likert
# section1a <- likert.bar.plot(NumAnalysis1, admissions)
# section1b <- likert.density.plot(NumAnalysis1, facet = T, bw = 0.5)
# ?likert.density.plot


# 
# section1c <- ggplot(NumAnalysis1,
#                     aes(x=admissions)) +
#                 theme(legend.position = "top",
#                       axis.text = element_text(size = 6))
# (section1d <- sections1c + geom_point(aes(color = )))

p5 <- ggplot(NumAnalysis1)
p5 + geom_line(aes(color = ))


min_rank(NumAnalysis1$admissions)
NumAnalysis1a <- percent_rank(NumAnalysis1$admissions)

p6 <- ggplot(NumAnalysis1a, aes(admissions)) + geom_point()

mean(NumAnalysis1a)

##Need to figure out the syntax that will allow me to facet wrap the plot 
##Then all the plots will show up in one big plot of individual plots
##http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#faceting_to_the_rescue


p7 <- ggplot(NumAnalysis1)
p7 + geom_bar()
p7
