
library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(readxl)

password <- readLines("password.txt")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  uiOutput("main_ui")
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  access_granted <- reactiveVal(FALSE)
  
  # read data
  # df_=readxl::read_excel("data/A Ball Scouting Reports.xlsx")
  df_=readxl::read_excel("data/DoubleAFall.xlsx",sheet="Pitching")
  
  
  df = reactive({
    
    df_
    
  })
  
  
  
  ######
  #display for raw data
  ######
  
  output$raw = renderDT(
    {
      
      df()
      
    }
  )
  
  
  
  ###
  # Render UI
  ###
  
  
  output$main_ui <- renderUI({
    if (!access_granted()) {
      tagList(
        passwordInput("pwd", "Enter Password"),
        actionButton("go", "Submit"),
        textOutput("wrong_pwd")
      )
    } else {
      
      sidebarLayout(
        sidebarPanel(
          
          selectInput("team", 
                      label = "Team", 
                      choices = c("All",unique(df_$Team)), 
                      selected = "All")
          
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Raw",DTOutput('raw'))
          )#end tabsetpanel
        )#end mainpanel
      )#end sidebarlayout
    }
  })
  
  observeEvent(input$go, {
    if (input$pwd == password) {
      access_granted(TRUE)
    } else {
      output$wrong_pwd <- renderText("❌ Incorrect password.")
    }
  })
  
  
  
  
  
  

  
}




# Run the application 
shinyApp(ui = ui, server = server)
