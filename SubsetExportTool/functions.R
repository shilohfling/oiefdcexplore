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

#### Function to replace Q column with Questions from Question Index

AddQtext <- function(x, qindex, header = TRUE) {
      
      x <- x %>% left_join(questionsIndex, by = c("Q" = "Question"))
      
      qitem <- x$QA[[1]]
      
      x <- x %>% select(-c(Category, Shortname, Qualtrics, QA)) %>% 
            rename(Question = QB) %>% 
            select(-Q) %>% 
            select(Question, everything())
      
      if (header == TRUE){      
            names(x)[names(x) == "Question"] <- qitem
      }
      
      return(x)
}

#### Rename the V column to the question from the Question Index
AddHeader <- function(x, q, qindex){
      names(x)[names(x) == "V"] <- questionsIndex$QB[questionsIndex$Question == q]
      return(x)
}

##### Make a DT Preview Frame

PreviewDT <- function(x, cnames) {
      x <- x %>% select(X.Program.Level, X.Degree, X.Campus1, X.Major, X.Dept, X.School) %>% 
            renameColumns(cnames)
      return(x)
}


##### Rename any/all columns that match the nm vector
renameColumns <- function(x, nm) {

      existing <- match(names(nm), names(x))
      names(x)[na.omit(existing)] <- nm[which(!is.na(existing))]
      
      return(x)
}




##### General table functions, should be flexible enough to apply to most tables #####
TableA <- function(x, questions, qindex, cnames) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        x <- x %>% levelQuestionAvgFreq(questions) %>% 
              group_by(Q, X.Program.Level, F.Overall.Avg, F.Program.Level.Avg) %>% 
              summarise() %>% ungroup() %>% 
              spread(X.Program.Level, F.Program.Level.Avg) %>% 
              renameColumns(cnames)
        
        AddQtext(x, qindex)
}

TableB <- function(x, question, qindex, cnames) {
        
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
               renameColumns(c("GRAD" = "F.GRAD.Program.Level.Avg",
                               "UNDG" = "F.UNDG.Program.Level.Avg"))
               #rename(F.GRAD.Program.Level.Avg = GRAD) %>% 
               #rename(F.UNDG.Program.Level.Avg = UNDG)
        
         z <- x %>% select(-F.Program.Avg) %>% 
               spread(X.Program.Level, F.Responses.Question.Program) %>% 
               renameColumns(c("GRAD" = "F.GRAD.Program.Q.Respondents.in.Avg",
                               "UNDG" = "F.UNDG.Program.Q.Respondents.in.Avg")) %>% 
               #rename(F.GRAD.Program.Q.Respondents.in.Avg = GRAD) %>% 
               #rename(F.UNDG.Program.Q.Respondents.in.Avg = UNDG) %>% 
               left_join(y, by = c("V", "F.Overall.Count", "F.Overall.Avg")) %>% 
               renameColumns(cnames)
        
        AddHeader(z, question, qindex)
}

##### Customized tables for reporting tables that don't fit into TableA or TableB #####
Table1 <- function(x, cnames) {

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
              select(-X.Data.Set) %>% 
              renameColumns(cnames)
}

Table6 <- function(x, questions, qindex, cnames) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10. Please select more data.")
        }
        
        if(!"UNDG" %in% unique(x$X.Program.Level)) {
                
                return("This item applies to undergradutes only, please select undergraduate data.")
                
        } else {
                
                x <- x %>% levelQuestionAvgFreq(questions) %>% 
                      add_count(Q) %>% 
                      rename(F.Respondents.in.Avg = n) %>%
                      rename(F.UNDG.Avg = F.Program.Level.Avg) %>% 
                      group_by(Q, X.Program.Level, F.UNDG.Avg, F.Respondents.in.Avg) %>% 
                      summarise() %>% ungroup() %>% 
                      select(-X.Program.Level) %>% 
                      renameColumns(cnames)
                
                x <- AddQtext(x, qindex)
                
                return(x)
        }
}

Table7 <- function(x, questions, qindex, cnames) {
        
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
              renameColumns(c("GRAD" = "F.GRAD.Program.Respondents.in.Avg",
                              "UNDG" = "F.UNDG.Program.Respondents.in.Avg"))
        
        z <- x %>% select(-F.Program.Respondents.in.Avg) %>% 
              spread(X.Program.Level, F.Program.Level.Avg) %>%
              renameColumns(c("GRAD" = "F.GRAD.Program.Level.Avg",
                              "UNDG" = "F.UNDG.Program.Level.Avg")) %>% 
              left_join(y, by = c("Q", "F.Overall.Avg", "F.Respondents.in.Avg")) %>% 
              renameColumns(cnames)
        
        AddQtext(z, qindex)
}

