##An R Script for plotting the numerical data
#library(openxlsx)
library(dplyr)
library(ggplot2)
#library(likert)
##Let's melt some data
library(reshape2)

##Don't need this part - can be found in 01-GetCleanData.R
#data_raw <- read.xlsx("../sources/RM_Outcomes Survey Data_2-20-2017_Working Response - macro.xlsm", 
#                      "Response Data_Working")
#NumAnalysis1 <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)

##########################SECTION 1 - ANALYSIS##################################
##Data frame for first section of questions
##Move relevant information over to the new data frame for plotting
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
NumAnalysis1$major <- data_raw$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis1$campcomb <- data_raw$`Combined.Campus.Location.(FOR.REPORTING)`
NumAnalysis1$department <- data_raw$`Department.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis1$school <- data_raw$`School.(not.scrubbed)`

##Melt the data down to make it tidy and ready for plotting
NumAnalysis1MELT <- melt(NumAnalysis1, id.vars = c("id.student", "major", "campcomb", "department", "school"))

##Plot the data into faceted bar graphs, one for each question
##Will show how many people answered each answer (1-4) and how many people from each school felt that way
section1 <- ggplot(NumAnalysis1MELT, aes(value, fill = school)) + 
        geom_bar() + 
        facet_wrap(~variable) + 
        theme(legend.position="bottom", legend.box = "horizontal") +
        labs(subtitle="Sentiment based on School/College") +
        xlab("") +
        ylab("# of students")
section1        ##View the plot

##Save the plot
ggsave("Section1Plot.jpg", plot = section1, width = 15, height = 10, units = "in")


##########################SECTION 2 - ANALYSIS##################################
NumAnalysis2 <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)
NumAnalysis2$major <- data_raw$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis2$campcomb <- data_raw$`Combined.Campus.Location.(FOR.REPORTING)`
NumAnalysis2$department <- data_raw$`Department.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis2$school <- data_raw$`School.(not.scrubbed)`
NumAnalysis2$clear.SLO <- data_raw$`In.my.program.of.study:.-.a..The.student.learning.outcomes.were.clear.to.me.`
NumAnalysis2$achieve.SLO <- data_raw$`In.my.program.of.study:.-.b..I.understood.how.the.courses.helped.me.to.achieve.the.student.learning.outcomes.`
NumAnalysis2$applied.concepts <- data_raw$`In.my.program.of.study:.-.c..I.understood.the.ways.concepts.could.be.applied.to.practice.`
NumAnalysis2$related.concepts <- data_raw$`In.my.program.of.study:.-.d..I.recognized.how.concepts.in.one.course.related.to.those.in.other.courses.`
NumAnalysis2$linked.new.concepts <- data_raw$`In.my.program.of.study:.-.e..I.was.able.to.link.new.concepts.to.my.prior.knowledge.`
NumAnalysis2$perceived.connections <- data_raw$`In.my.program.of.study:.-.f..I.perceived.connections.between.my.program.of.study.and.the.world.around.me.`

NumAnalysis2MELT <- melt(NumAnalysis2, id.vars = c("id.student", "major", "campcomb", "department", "school"))

section2 <- ggplot(NumAnalysis2MELT, aes(value, fill = school)) + 
        geom_bar() + 
        facet_wrap(~variable) + 
        theme(legend.position="bottom", legend.box = "horizontal") +
        labs(subtitle="Sentiment based on School/College") +
        xlab("") +
        ylab("# of students")
section2   

ggsave("Section2Plot.jpg", plot = section1, width = 15, height = 10, units = "in")





##########################SECTION 3 - ANALYSIS##################################
NumAnalysis3 <- data.frame("id.student" = data_raw$`Student.ID.(Complete)`)
NumAnalysis3$major <- data_raw$`Major.1.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis3$campcomb <- data_raw$`Combined.Campus.Location.(FOR.REPORTING)`
NumAnalysis3$department <- data_raw$`Department.(from.Recipients.and.Response.Rates.Data.Set)`
NumAnalysis3$school <- data_raw$`School.(not.scrubbed)`
NumAnalysis3$knowledgeable.learner <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.a..Be.a.knowledgeable.learner.`
NumAnalysis3$informed.instructor <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.b..Be.an.informed.instructor.`
NumAnalysis3$reflective.collaborator <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.c..Be.a.reflective.collaborator.`
NumAnalysis3$responsive.educational.professional <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.d..Be.a.responsive.educational.professional.`
NumAnalysis3$respect.myself <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.e..Respect.myself.`
NumAnalysis3$respect.others <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.f..Respect.others.`
NumAnalysis3$respect.MPC <- data_raw$`Please.indicate.your.level.of.agreement.with.the.following..My.experiences.in.my.program.of.study.have.enhanced.my.ability.to:.-.g..Respect.my.professional.community.`

NumAnalysis3MELT <- melt(NumAnalysis3, id.vars = c("id.student", "major", "campcomb", "department", "school"))

section3 <- ggplot(NumAnalysis3MELT, aes(value, fill = major)) + 
        geom_bar() + 
        facet_wrap(~variable) + 
        theme(legend.position="bottom", legend.box = "horizontal") +
        labs(subtitle="Sentiment based on major") +
        xlab("") +
        ylab("# of students")
section3   

ggsave("Section3Plot.jpg", plot = section1, width = 15, height = 10, units = "in")