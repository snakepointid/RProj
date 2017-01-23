library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(
  
  #  Application title
  headerPanel("get parameter values"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
    # Simple integer interval
    sliderInput("cps", "cp:", 
                min=0, max=8, value=4,step=1),
    
    # Decimal interval with step value
    sliderInput("depth", "tree's max depth:", 
                min = 1, max = 12, value = 5, step= 1),
    
    # Specification of range within an interval
    sliderInput("minm", "minimun obser in node:",
                min = 0, max = 200, value = 30,step=5),
    
    # Provide a custom currency format for value display, with basic animation
    radioButtons("imp", "whether impute those missing value:",
                 list("yes" = 'yes',
                      "no" = 'no')),
    selectInput("var1", "drop a variable:", 
                choices = c('',useful)),
    selectInput("var2", "drop a variable:", 
                choices = c('',useful))
  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    h3(textOutput("cap")),
    verbatimTextOutput("Rules")
    
  )
))