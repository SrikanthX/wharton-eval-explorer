library(tidyverse)
evals <- data.table::fread("wharton_reports_wharton-course-evaluations.csv",header=T)
availableElectives <- c("ACCT706","ACCT747","BEPP770","FNCE703","FNCE705","FNCE707","FNCE717","FNCE731","FNCE738","FNCE750","FNCE751","FNCE754","FNCE756","FNCE780","FNCE783","FNCE791","FNCE802","FNCE811","HCMG841","HCMG845","HCMG850","HCMG852","HCMG860","HCMG867","LGST642","LGST813","LGST815","MGMT624","MGMT671","MGMT701","MGMT721","MGMT731","MGMT748","MGMT794","MGMT801","MGMT802","MGMT804","MGMT806","MGMT809","MGMT811","MGMT812","MGMT871","MKTG711","MKTG712","MKTG721","MKTG739","MKTG747","MKTG754","MKTG777","MKTG778","MKTG852","OIDD613","OIDD614","OIDD615","OIDD636","OIDD643","OIDD653","OIDD654","OIDD662","OIDD667","OIDD673","REAL890","REAL891","STAT701","STAT722","ACCT897","BEPP763","BEPP773","LGST693","LGST806","MGMT690","MGMT793")
evals <- evals %>%
  mutate(Course = substr(Section,1,7)) %>% 
  mutate(Department = substr(Section,1,4)) %>%
  mutate(Year = zoo::as.yearmon(as.numeric(substr(Term,1,4)))) %>%
  mutate(Available = Course %in% availableElectives)
evals <- evals[,c(16:19,1:15)]
courseTitles <- evals %>%
  group_by(Course) %>%
  summarise(Title = max(Title))

evals %>%
  {if(T) filter(., Available == T) else . } %>%
  group_by_at(c("Course"))    %>%
  summarise_at(.funs = c("mean","max"),.vars = c("Overall Quality of Instructor (0=worst 4=best)","Rate the Difficulty of this Course (0=easiest 4=hardest)")) 

groupingVars <- c("Course")
functions <- c("mean","max")
analysisVars = c("Overall Quality of Course (0=worst 4=best)")

evals %>%
  {if(F) filter(., Available == T) else . } %>%
  group_by_at(groupingVars)    %>%
  summarise_at(.funs = functions,.vars = analysisVars)

evals %>%
  group_by (Course) %>%
  summarise(n()) %>% View()
