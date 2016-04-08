library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Tabsets"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("choice", "Analysis Type:",
                   c("Daily" = "day_mentions",
                     "Week" = "weekly_mentions",
                     "live" = "live")),
      br()
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Mention Analysis", plotOutput("plot1")), 
                  tabPanel("Location Analysis", plotOutput("plot2"))
                  
      )
    )
  )
))