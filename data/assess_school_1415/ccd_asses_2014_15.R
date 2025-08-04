
library(tidyverse)
library(readr)
library(stringr)

# 1.write a function to get "ncessch + assessment"  ----------------------          
combine_sch_asse <- function(sch_name, asse_name, year){
  sch <- read_csv(sch_name)
  ass <- read_csv(asse_name)
  
  #sch$ncessch <- sprintf("%012.0f", sch$ncessch)
  #asse <- rename(asse, ncessch = NCESSCH)
  
  com <- merge(x = sch, y = ass, by = "NCESSCH", all.x = TRUE, sort = FALSE)
  com <- com[!is.na(com$STNAM), ]
  com <- select(com, NCESSCH, contains(year))
  
  var_names <- names(com)[2:length(com)]
  names(com)[2:length(com)] <- sub(str_c("_", year), "", var_names)
  
  return(com)
}

# 2.write a function to get "sch + 4 assessment" merge together  ----------------------          
get_result <- function(sch_name, asse1_name, asse2_name, asse3_name, asse4_name, year){
  asse1 <- combine_sch_ass(sch_name, asse1_name, year)
  asse2 <- combine_sch_ass(sch_name, asse2_name, year)
  asse3 <- combine_sch_ass(sch_name, asse3_name, year)
  asse4 <- combine_sch_ass(sch_name, asse4_name, year)
  
  sch <- read_csv(sch_name)
  #sch$ncessch <- sprintf("%012.0f", sch$ncessch)

  sch <- merge(x = sch, y = ass1, by="NCESSCH", all.x = TRUE, sort = FALSE)
  sch <- merge(x = sch, y = ass2, by="NCESSCH", all.x = TRUE, sort = FALSE)
  sch <- merge(x = sch, y = ass3, by="NCESSCH", all.x = TRUE, sort = FALSE)
  sch <- merge(x = sch, y = ass4, by="NCESSCH", all.x = TRUE, sort = FALSE)
  
  return(sch)
}
  
# 3.output (change next 6 variable names) ----------------------   
sch_name = "../data/ccd-1415.csv"
math_ach = "../data/assessment_1415/math-achievement-sch-sy2014-15.csv"
math_par = "../data/assessment_1415/math-participation-sch-sy2014-15.csv"
rla_ach = "../data/assessment_1415/rla-achievement-sch-sy2014-15.csv"
rla_par = "../data/assessment_1415/rla-participation-sch-sy2014-15.csv"
year = "1415"
output = get_result(sch_name, math_ach, math_par, rla_ach, rla_par, year)
write.csv(output, file = str_c("../data/ccd_ass_", year, ".csv"), na = ".")


