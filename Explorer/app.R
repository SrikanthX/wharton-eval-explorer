library(shiny)
library(tidyverse)
library(gmodels)

# prepare the evals
evals <- data.table::fread("./data/wharton_reports_wharton-course-evaluations.csv",header=T)
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

# Define UI ----
ui <- fluidPage(
  titlePanel(h1("Wharton EMBA Course Eval Explorer")),
  sidebarLayout(
    sidebarPanel(h1("Define your output"),
                 checkboxInput(inputId = "onlyCurrent",label = "Only consider WEMBA 47 West courses",value = T),
                 checkboxGroupInput("groupingVars",
                                    h3("Grouping Variables"),
                                    choices = c("Course","Department","Instructor"),selected = c("Course","Department","Instructor")),
                 checkboxGroupInput("analysisVars",
                                    h3("Analysis Variables"), choices = colnames(evals)[9:19],selected = "Overall Quality of Instructor (0=worst 4=best)"),
                 checkboxGroupInput("analyses",h3("Summary Functions"),
                                    choices = c("Mean","Min","Max","95ci"),
                                    selected="Mean"),
                 ),
    mainPanel(DT::dataTableOutput("results"))
  )
)

ninetyfiveciLow <- function(values){
  gmodels::ci(values)[2]
}

ninetyfiveciHigh <- function(values){
  gmodels::ci(values)[3]
}

# Define server logic ----
server <- function(input, output) {
  onlyCurrent <- reactive({input$onlyCurrent})
  groupingVars <- reactive({input$groupingVars})
  analyses <- reactive({input$analyses})
  analysisVars <- reactive({input$analysisVars})
  functions <- reactive({
    na.omit(c(base::ifelse("Mean" %in% analyses(),"mean",NA),
              base::ifelse("Min" %in% analyses(),"min",NA),
              base::ifelse("Max" %in% analyses(), "max", NA),
              base::ifelse("95ci" %in% analyses(),"ninetyfiveciLow",NA),
              base::ifelse("95ci" %in% analyses(),"ninetyfiveciHigh",NA)
              ))
      # base::ifelse("95ci" %in% analyses(),c("ninetyfiveciLow","ninetyfiveciHigh"), NA),
      #"n"))
  })
  analysis <- reactive({
    evals %>%
      {if(onlyCurrent()) filter(., Available == T) else . } %>%
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
  results <- reactive({
    if("Course" %in% groupingVars()){
      withCounts() %>% 
        left_join(courseTitles,by="Course")
    }else{
      withCounts()
    }
  })
  output$onlyCurrent <- renderText(input$onlyCurrent)
  output$groupingVars <- renderText(input$groupingVars)
  output$results <- DT::renderDataTable(results())
  output$functions <- renderText(functions())
}

# Run the app ----
shinyApp(ui = ui, server = server)