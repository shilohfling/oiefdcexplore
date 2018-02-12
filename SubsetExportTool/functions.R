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

Table1 <- function(x) {##Response Rates
      x %>% add_count(X.Program.Level, X.Data.Set) %>%
            rename(F.Total.Respondents = n) %>%
            add_count(X.Program.Level) %>%
            rename(F.Total.Graduates = n) %>%
            mutate(F.Response.Rate = F.Total.Respondents/F.Total.Graduates) %>%
            group_by(X.Program.Level, X.Data.Set, F.Total.Graduates, F.Total.Respondents, F.Response.Rate) %>%
            summarise() %>% ungroup() %>%
            filter(X.Data.Set == "Responder")  %>%
            select(-X.Data.Set)
}

TableQ <- function(x, questions) {
        
        if(nrow(x) < 10) {
                return("Selected sample is less than 10.  Please select more data.")
        }
        
        x %>% levelQuestionAvgFreq(questions) %>% 
                group_by(Q, X.Program.Level, F.Overall.Avg, F.Program.Level.Avg) %>% 
                summarise() %>% ungroup() %>% 
                spread(X.Program.Level, F.Program.Level.Avg)
}

Table6 <- function(x, questions) {
        x %>% levelQuestionAvgFreq(questions) %>% 
                add_count(Q) %>% 
                rename(F.Respondents.in.Avg = n) %>% 
                group_by(Q, X.Program.Level, F.Program.Level.Avg, F.Respondents.in.Avg) %>% 
                summarise() %>% ungroup() %>% 
                select(-X.Program.Level)
}

Table7 <- function(x, questions) {
        x %>% levelQuestionAvgFreq(questions) %>% 
                add_count(Q) %>% 
                rename(F.Respondents.in.Avg = n) %>% 
                group_by(Q, X.Program.Level, F.Program.Level.Avg, F.Respondents.in.Avg) %>% 
                summarise() %>% ungroup() %>%
                spread(Q, X.Program.Level, F.Program.Level.Avg, F.Respondents.in.Avg)
}
