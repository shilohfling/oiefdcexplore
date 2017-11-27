library(ggplot2)

plotData1 <- commentWordsDF %>%
        group_by(major, sentence_id) %>% 
        #group_by(major) %>%
        top_n(10, MSscore)

testing2 <- commentWordsDF
testing2 <- unique(testing2)
testing2 <- as.data.frame(testing2)

campus_top_10 <- subset(commentWordsDF, campus == "Webster Groves, MO") %>%
                #unique(MStopics) %>%
                #unique(sentences) %>%
                #select(campus) %>%
                group_by(campus, sentences) %>%
                #min(CAMPUS_MEAN) %>%
                #max(CAMPUS_MEAN) %>%
                top_n(10, CAMPUS_MEAN)

dept_top_10 <- subset(commentWordsDF, department == "Business") %>%
        group_by(department, sentences) %>%
        top_n(10, DEPT_DG_MEAN)


major_top_10 <- commentWordsDF %>%
        #unique(id.student) %>%
        #unique(sentences) %>%
        group_by(major, sentences) %>%
        top_n(10, MAJOR_MEAN)



##########EXPLORATION##########
examplePlot <- ggplot()
