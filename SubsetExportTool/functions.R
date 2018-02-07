Table1 <- function(x) {##Response Rates
      x %>% add_count(Program.Level, DataSet) %>%
            rename(Total.Respondents = n) %>%
            add_count(Program.Level) %>%
            rename(Total.Graduates = n) %>%
            mutate(Response.Rate = Total.Respondents/Total.Graduates) %>%
            group_by(Program.Level, DataSet, Total.Graduates, Total.Respondents, Response.Rate) %>%
            summarise() %>% ungroup() %>%
            filter(DataSet == "Responder")  %>%
            select(-DataSet)
}

TableQ <- function(x, questions) {
      x %>% select(School:Degree, questions) %>% 
            gather("Q", "V", starts_with("Q")) %>% 
            group_by(Q) %>% na.omit() %>% 
            mutate("Overall.Avg" = mean(V)) %>% 
            ungroup() %>% 
            group_by(Q, Program.Level) %>% 
            mutate("Program.Level.Avg" = mean(V)) %>% 
            ungroup() %>% 
            group_by(Q, Program.Level, Overall.Avg, Program.Level.Avg) %>% 
            summarise() %>% 
            spread(Program.Level, Program.Level.Avg) %>% 
            rename(GRAD.Avg = GRAD) %>% 
            rename(UNDG.Avg = UNDG)
}
