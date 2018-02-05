testQ <- function(x) {
      sjp.likert(select(x, starts_with("Q")))
}

Q1table <- function(x) {
      sjmisc::frq(x, Program.Level)
}