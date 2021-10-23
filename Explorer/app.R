library(shiny)
library(tidyverse)
library(gmodels)

# releaseNotes
releaseNotes <- "Version 0.2: added how recently prof taught and selecting WEMBA West and East is now an or rather than an and."
releaseNotes <- c(releaseNotes,"Version 0.3: added course selector")
releaseNotes <- c(releaseNotes,"Version 0.4: fixed some column headings going missing")

# prepare the evals
evals <- data.table::fread("./data/wharton_reports_wharton-course-evaluations.csv",header=T)

availableElectivesWest <- c("ACCT706","ACCT747","BEPP770","FNCE703","FNCE705","FNCE707","FNCE717","FNCE731","FNCE738","FNCE750","FNCE751","FNCE754","FNCE756","FNCE780","FNCE783","FNCE791","FNCE802","FNCE811","HCMG841","HCMG845","HCMG850","HCMG852","HCMG860","HCMG867","LGST642","LGST813","LGST815","MGMT624","MGMT671","MGMT701","MGMT721","MGMT731","MGMT748","MGMT794","MGMT801","MGMT802","MGMT804","MGMT806","MGMT809","MGMT811","MGMT812","MGMT871","MKTG711","MKTG712","MKTG721","MKTG739","MKTG747","MKTG754","MKTG777","MKTG778","MKTG852","OIDD613","OIDD614","OIDD615","OIDD636","OIDD643","OIDD653","OIDD654","OIDD662","OIDD667","OIDD673","REAL890","REAL891","STAT701","STAT722","ACCT897","BEPP763","BEPP773","LGST693","LGST806","MGMT690","MGMT793","MKTG776","OIDD761","REAL721")
availableElectivesEast <- c("ACCT706","ACCT747","ACCT897","FNCE797","BEPP763","OIDD763","BEPP770","BEPP773","FNCE730","REAL730","FNCE717","FNCE705","FNCE703","FNCE707","FNCE731","FNCE738","FNCE750","FNCE751","FNCE754","FNCE756","FNCE783","FNCE791","FNCE802","FNCE811","FNCE780","HCMG841","HCMG845","HCMG850","HCMG852","HCMG860","HCMG867","LGST642","LGST693","OIDD693","LGST806","MGMT691","OIDD691","LGST809","MGMT815","LGST813","LGST815","MGMT624","MGMT671","MGMT690","OIDD690","MGMT701","MGMT720","MGMT721","MGMT731","MGMT740","MGMT748","MGMT793","OIDD793","MGMT794","MGMT801","MGMT802","MGMT804","MGMT806","MGMT809","MGMT811","MGMT812","MGMT871","MKTG711","MKTG712","MKTG721","MKTG739","MKTG747","MKTG754","MKTG776","STAT476","MKTG777","MKTG778","MKTG852","OIDD613","OIDD614","OIDD615","OIDD636","OIDD643","OIDD653","OIDD654","OIDD662","OIDD667","OIDD673","OIDD761","BEPP761","REAL721","FNCE721","REAL890","REAL891","STAT701","STAT722")

evals <- evals %>%
  mutate(Course = substr(Section,1,7)) %>% 
  mutate(Department = substr(Section,1,4)) %>%
  mutate(Year = zoo::as.yearmon(as.numeric(substr(Term,1,4)))) %>%
  mutate(AvailableWest = Course %in% availableElectivesWest) %>%
  mutate(AvailableEast = Course %in% availableElectivesEast) %>%
  mutate(WEMBAEast = ifelse(substr(Section,8,9) == "70",T,F)) %>% 
  mutate(WEMBAWest = ifelse(substr(Section,8,9) == "75",T,F)) 

evals <- evals[,c(16:22,1:15)]

courseTitles <- evals %>%
                    group_by(Course) %>%
                    summarise(Title = max(Title))

getTermStringFromTermIdentifier <- function(termId) {
  if(is.na(termId)){
    return(NA)
  }
  seasonLetter <- substr(termId,5,5)
  season <- recode(seasonLetter,A = "Spring", B = "Summer", C = "Fall")
  year = substr(termId,1,4)
  return(paste(season,year))
}

getTermStringFromTermIdentifier <- Vectorize(getTermStringFromTermIdentifier)

lastTaught <- evals %>%
          group_by(Instructor) %>%
          summarise(maxTerm = max(Term),
                    maxTermWest = base::suppressWarnings(max(Term[WEMBAWest])),
                    maxTermEast = base::suppressWarnings(max(Term[WEMBAEast]))) %>%
          mutate(lastTaughtTerm = getTermStringFromTermIdentifier(maxTerm),
                 lastTaughtTermWest = getTermStringFromTermIdentifier(maxTermWest),
                 lastTaughtTermEast = getTermStringFromTermIdentifier(maxTermEast)) %>%
          select(Instructor,lastTaughtTerm,lastTaughtTermEast,lastTaughtTermWest)

lastTaughtThisCourse <- evals %>%
  group_by(Instructor,Course) %>%
  summarise(maxTerm = max(Term),
            maxTermWest = base::suppressWarnings(max(Term[WEMBAWest])),
            maxTermEast = base::suppressWarnings(max(Term[WEMBAEast]))) %>%
  mutate(lastTaughtTerm = getTermStringFromTermIdentifier(maxTerm),
         lastTaughtTermWest = getTermStringFromTermIdentifier(maxTermWest),
         lastTaughtTermEast = getTermStringFromTermIdentifier(maxTermEast)) %>%
  select(Instructor,Course,lastTaughtTerm,lastTaughtTermEast,lastTaughtTermWest)

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Wharton EMBA Course Eval Explorer")),
  sidebarLayout(
    sidebarPanel(h1("Define your output"),
                 checkboxGroupInput("filters",
                                    h3("Filters"),
                                    choices = c("Available to WEMBA 47 West" = "currentWest", "Available to WEMBA 47 East" = "currentEast")),
                 selectInput('classes',"Select Course(s)",courseTitles$Course,multiple = T,selectize = T),
                 checkboxGroupInput("groupingVars",
                                    h3("Grouping Variables"),
                                    choices = c("Course","Department","Instructor"),selected = c("Course","Department","Instructor")),
                 checkboxGroupInput("analysisVars",
                                   h3("Analysis Variables"), choices = colnames(evals)[12:22],selected = "Overall Quality of Instructor (0=worst 4=best)"),
                 checkboxGroupInput("analyses",h3("Summary Functions"),
                                    choices = c("Mean","Min","Max","95ci"),
                                    selected="Mean")
                 ),
    mainPanel(DT::dataTableOutput("results"),
              p(releaseNotes[1]),
              p(releaseNotes[2]),
              p(releaseNotes[3])) 
  )
)

ninetyfiveciLow <- function(values){
  base::suppressWarnings(gmodels::ci(values)[2]) 
}

ninetyfiveciHigh <- function(values){
  base::suppressWarnings(gmodels::ci(values)[3]) 
}

# Define server logic ----
server <- function(input, output) {
  filters <- reactive({input$filters})
  classes <- reactive({input$classes})
  groupingVars <- reactive({input$groupingVars})
  analyses <- reactive({input$analyses})
  analysisVars <- reactive({input$analysisVars})
  analysisVarsPattern <- reactive({paste("^",analysisVars(),".*",collapse = "|",sep="")})
  functions <- reactive({
    na.omit(c(base::ifelse("Mean" %in% analyses(),"mean",NA),
              base::ifelse("Min" %in% analyses(),"min",NA),
              base::ifelse("Max" %in% analyses(), "max", NA),
              base::ifelse("95ci" %in% analyses(),"ninetyfiveciLow",NA),
              base::ifelse("95ci" %in% analyses(),"ninetyfiveciHigh",NA)
              ))
  })
  analysis <- reactive({
    evals %>%
      {if("currentWest" %in% filters() & !("currentEast" %in% filters())) filter(., AvailableWest == T) else . } %>%
      {if("currentEast" %in% filters() & !("currentWest" %in% filters())) filter(., AvailableEast == T) else . } %>%
      {if("currentEast" %in% filters() & "currentWest" %in% filters()) filter(., AvailableEast == T | AvailableWest == T) else . } %>%
      group_by_at(groupingVars()) %>%    
      summarise_at(.funs = functions(),.vars = analysisVars())
  })
  counts <- reactive({
    evals %>%
      group_by_at(groupingVars()) %>%
      summarise(n=n())
  })
  withCounts <- reactive({
    analysis() %>%
      left_join(counts(),by=groupingVars())
  })
  withLastTaught <- reactive({
    if("Instructor" %in% groupingVars() & "Course" %in% groupingVars()){
      withCounts() %>%
        left_join(lastTaught,by="Instructor") %>%
          left_join(lastTaughtThisCourse, by = c("Instructor","Course")) %>%
            rename("Most recent term instructing any class" = "lastTaughtTerm.x",
                   "Most recent term instructing any class (WEMBA East)" = "lastTaughtTermEast.x",
                   "Most recent term instructing any class (WEMBA West)" = "lastTaughtTermWest.x",
                   "Most recent term instructing this class" = "lastTaughtTerm.y",
                   "Most recent term instructing this class (WEMBA East)" = "lastTaughtTermEast.y",
                   "Most recent term instructing this class (WEMBA West)" = "lastTaughtTermWest.y")
    }else if("Instructor" %in% groupingVars()){
      withCounts() %>%
        left_join(lastTaught, by = "Instructor") %>%
        rename("Most recent term instructing any class" = "lastTaughtTerm.x",
               "Most recent term instructing any class (WEMBA East)" = "lastTaughtTermEast.x",
               "Most recent term instructing any class (WEMBA West)" = "lastTaughtTermWest.x")
    }
  })
  results <- reactive({
    if("Course" %in% groupingVars()){
      withLastTaught() %>% 
        left_join(courseTitles,by="Course")
    }else{
      withLastTaught()
    }
  })
  orderedResults <- reactive({
    results() %>%
      {if(length(classes())>0) filter(., Course %in% classes()) else . } %>%
      select(groupingVars(),contains("Title"),"n",matches("(.*Overall.*)|(.*Instructor Ability.*)|(.*Instructor Access.*)|(.*Value.*)|(.*Learned.*)|(.*Rate.*)|(.*Would.*)|(mean)|(min)|(max)|(ninetyfiveciLow)|(ninetyfiveciHigh)"),matches("^Most recent term.*"))
  })
  output$groupingVars <- renderText(input$groupingVars)
  output$analysisVarsPattern <- renderText(analysisVarsPattern())
  output$results <- DT::renderDataTable(orderedResults())
  output$functions <- renderText(functions())
  output$withCounts <- DT::renderDataTable(withCounts())
}

# Run the app ----
shinyApp(ui = ui, server = server)