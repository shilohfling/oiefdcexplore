##################################################
# This dashboard prototype is for subsetting
# data and creating customized reports through a 
# friendly Shiny based GUI.
#################################################

##### Function to calculate frequency of responses #####
levelQuestionAvgFreq <- function(x, questions) {
        x %>% select(X.Program.Level, questions) %>% 
                gather("Q", "V", starts_with("Q")) %>% 
                group_by(Q) %>% na.omit() %>% 
                mutate(F.Overall.Avg = mean(V)) %>% 
                ungroup() %>% 
                group_by(Q, X.Program.Level) %>% 
                mutate(F.Program.Level.Avg = mean(V)) %>% 
                ungroup() 
}

##### General table functions, should be flexible enough to apply to most tables #####
TableA <- function(x, questions, qindex) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        x <- x %>% levelQuestionAvgFreq(questions) %>% 
                group_by(Q, X.Program.Level, F.Overall.Avg, F.Program.Level.Avg) %>% 
                summarise() %>% ungroup() %>% 
                spread(X.Program.Level, F.Program.Level.Avg) 
        
        x <- x %>% left_join(questionsIndex, by = c("Q" = "Question"))
        
        qitem <- x$QA[[1]]
        
        x <- x %>% select(-c(Category, Shortname, Qualtrics, QA)) %>% 
                rename(Question = QB) %>% 
                select(-Q) %>% 
                select(Question, everything())
        
        names(x)[names(x) == "Question"] <- qitem
        
        return(x)
}

TableB <- function(x, question) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        x <- x %>% select(question, X.Program.Level) %>%
              rename_("V" = question) %>%
              na.omit() %>%
              filter(V != "No Response") %>%
              mutate(F.Overall.Count = nrow(.)) %>% 
              add_count(V) %>%
              rename(F.Responses.Question = n) %>% 
              add_count(V, X.Program.Level) %>%
              rename(F.Responses.Question.Program = n) %>% 
              add_count(X.Program.Level) %>%
              rename(F.Responses.Program = n) %>%
              mutate(F.Overall.Avg = (F.Responses.Question/F.Overall.Count) * 100) %>% 
              mutate(F.Program.Avg = (F.Responses.Question.Program/F.Responses.Program) * 100) %>% 
              group_by(V, X.Program.Level, F.Overall.Count, F.Overall.Avg, 
                       F.Responses.Question.Program, F.Program.Avg) %>% 
              summarise() %>% ungroup
        
        y <- x %>% select(-F.Responses.Question.Program) %>% 
              spread(X.Program.Level, F.Program.Avg) %>% 
              rename(F.GRAD.Program.Level.Avg = GRAD) %>% 
              rename(F.UNDG.Program.Level.Avg = UNDG)
        
        z <- x %>% select(-F.Program.Avg) %>% 
              spread(X.Program.Level, F.Responses.Question.Program) %>% 
              rename(F.GRAD.Program.Q.Respondents.in.Avg = GRAD) %>% 
              rename(F.UNDG.Program.Q.Respondents.in.Avg = UNDG) %>% 
              left_join(y, by = c("V", "F.Overall.Count", "F.Overall.Avg"))
     
        return(z)
}

##### Customized tables for reporting tables that don't fit into TableA or TableB #####
Table1 <- function(x) {

        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        ##Response Rates
        x %>% add_count(X.Program.Level, X.Data.Set) %>%
                rename(F.Total.Respondents = n) %>%
                add_count(X.Program.Level) %>%
                rename(F.Total.Graduates = n) %>%
                mutate(F.Response.Rate = (F.Total.Respondents/F.Total.Graduates) *100) %>%
                group_by(X.Program.Level, X.Data.Set, F.Total.Graduates, 
                         F.Total.Respondents, F.Response.Rate) %>%
                summarise() %>% ungroup() %>%
                filter(X.Data.Set == "Responder")  %>%
                select(-X.Data.Set)
}

Table6 <- function(x, questions, qindex) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        x %>% levelQuestionAvgFreq(questions) %>% 
                add_count(Q) %>% 
                rename(F.Respondents.in.Avg = n) %>% 
                group_by(Q, X.Program.Level, F.Program.Level.Avg, F.Respondents.in.Avg) %>% 
                summarise() %>% ungroup() %>% 
                select(-X.Program.Level)
        
        xx <- x %>% left_join(qindex, by = c("Q" = "Question"))
        
        qitem <- x$QA[[1]]
         
        xx <- xx %>% select(-c(Category, Shortname, Qualtrics, QA)) %>%
                rename(Question = QB) %>%
                select(-Q) %>%
                select(Question, everything())

        names(xx)[names(xx) == "Question"] <- qitem

        return(xx)
}

Table7 <- function(x, questions, qindex) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        x <- x %>% levelQuestionAvgFreq(questions) %>%
                add_count(Q) %>%
                rename(F.Respondents.in.Avg = n) %>%
                group_by(Q, X.Program.Level) %>% 
                add_count(Q) %>%
                rename(F.Program.Respondents.in.Avg = n) %>%
                group_by(Q, F.Overall.Avg, F.Respondents.in.Avg, X.Program.Level, 
                         F.Program.Level.Avg, F.Program.Respondents.in.Avg) %>%
                summarise() %>% ungroup() 
        
        y <- x %>% select(-F.Program.Level.Avg) %>% 
                spread(X.Program.Level, F.Program.Respondents.in.Avg) %>% 
                rename(F.GRAD.Program.Respondents.in.Avg = GRAD) %>% 
                rename(F.UNDG.Program.Respondents.in.Avg = UNDG)
        
        z <- x %>% select(-F.Program.Respondents.in.Avg) %>% 
                spread(X.Program.Level, F.Program.Level.Avg) %>% 
                rename(F.GRAD.Program.Level.Avg = GRAD) %>% 
                rename(F.UNDG.Program.Level.Avg = UNDG) %>% 
                left_join(y, by = c("Q", "F.Overall.Avg", "F.Respondents.in.Avg"))
        
        # xx <- z %>% left_join(questionsIndex, by = c("Q" = "Question"))
        # 
        # qitem <- x$QA[[1]]
        # 
        # xx <- xx %>% select(-c(Category, Shortname, Qualtrics, QA)) %>% 
        #         rename(Question = QB) %>% 
        #         select(-Q) %>% 
        #         select(Question, everything())
        # 
        # names(xx)[names(xx) == "Question"] <- qitem
        # 
        # return(xx)
}

