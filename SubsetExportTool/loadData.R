##################################################
# This dashboard prototype is for subsetting
# data and creating customized reports through a 
# friendly Shiny based GUI.
##################################################

##Load the data object from disk
data <- readRDS("sources/data.RDS")

data$AY <- as.character(data$AY)

##Set the column names from the index file
colnames(data) <- questionsIndex$Question

##Set column types, tidy, etc
#data$X.Program.Level <- as.factor(data$X.Program.Level)

##Create misc. objects
cnm <- c("AY" = "Academic Year",
        "X.Program.Level" = "Program Level",
        "F.Total.Graduates" = "Total Graduates",
        "F.Total.Respondents" = "Total Respondents",
        "F.Response.Rate" = "Response Rate",
        "F.Overall.Avg" = "Overall Average",
        "F.Overall.Total.Percent" = "% of Overall Total",
        "F.UNDG.Avg" = "UNDG Average",
        "F.Program.Level.Avg" = "Program Level Average",
        "F.Respondents.in.Avg" = "# of Respondents in Average",
        "F.GRAD.Program.Level.Avg" = "GRAD Average",
        "F.UNDG.Program.Level.Avg"= "UNDG Average",
        "F.GRAD.Program.Level.Percent" = "% of GRAD Total",
        "F.UNDG.Program.Level.Percent" = "% of UNDG Total",
        "F.GRAD.Program.Respondents.in.Avg" = "# of GRAD Respondents in Average",
        "F.UNDG.Program.Respondents.in.Avg" = "# of UNDG Respondents in Average",
        "F.Overall.Count" = "# of Overall Respondents",
        "F.GRAD.Program.Q.Respondents.in.Avg" = "# of GRAD Respondents",
        "F.UNDG.Program.Q.Respondents.in.Avg" = "# of UNDG Respondents",
        "X.Degree" = "Degree", 
        "X.Campus1" = "Campus", 
        "X.Major" = "Major", 
        "X.Dept" = "Department", 
        "X.School" = "College/School")

