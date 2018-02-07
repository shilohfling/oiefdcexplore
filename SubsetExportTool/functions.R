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
      x %>% select(X.Program.Level, questions) %>% 
            gather("Q", "V", starts_with("Q")) %>% 
            group_by(Q) %>% na.omit() %>% 
            mutate(F.Overall.Avg = mean(V)) %>% 
            ungroup() %>% 
            group_by(Q, X.Program.Level) %>% 
            mutate(F.Program.Level.Avg = mean(V)) %>% 
            ungroup() %>% 
            group_by(Q, X.Program.Level, F.Overall.Avg, F.Program.Level.Avg) %>% 
            summarise() %>% 
            spread(X.Program.Level, F.Program.Level.Avg) %>% 
            rename(GRAD.Avg = GRAD) %>% 
            rename(UNDG.Avg = UNDG)
}
