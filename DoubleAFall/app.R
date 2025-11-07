
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
  df_hitting_=readxl::read_excel("data/DoubleAFall.xlsx",sheet="Hitting")
  df_pitching_=readxl::read_excel("data/DoubleAFall.xlsx",sheet="Pitching")
  
  
  #####################
  # Apply filtering
  #####################
  
  df_hitting = reactive({
    
    # reactively filter to team
    if(input$team != "All"){
      df_hitting_=df_hitting_ %>% filter(Team==input$team)
    }
    
    df_hitting_
  })
  
  df_pitching = reactive({
    
    # reactively filter to team
    if(input$team != "All"){
      df_pitching_=df_pitching_ %>% filter(Team==input$team)
    }
    
    df_pitching_
  })
  
  
  
  #######################
  #display for raw data
  #######################
  
  output$raw_hitting = renderDT(
    {
      df_hitting()
    }
  )
  
  output$raw_pitching = renderDT(
    {
      df_pitching()
    }
  )
  
  
  ######
  #display for aggregate pitching data
  ######
  
  
  agghitting=reactive({
    # df=df_ %>% filter(Team=="Lugnuts")
    
    agghitting=df_hitting() %>% 
      group_by(Team,Name) %>% 
      summarize(games=n(),
                order=round(mean(Order),2),
                Hits=sum(Hits),
                AB=sum(`At Bats`),
                BB=sum(BB),
                HBP=sum(HBP),
                SO=sum(SO),
                XBH=sum(XBH),
                SB=sum(SB),
                CS=sum(CS)
      ) %>% 
      ungroup() %>% 
      mutate(AVG=round(Hits/AB,3),
             OBP=round(((Hits+BB+HBP)/(AB+BB+HBP)),2),
             `SO/G`=round(SO/games,2)
      ) %>% 
      arrange(desc(AVG)) %>% 
      mutate(`Percentile (AVG)`=round(100*(nrow(.)-row_number())/nrow(.),2))
    
    agghitting
    
  })
  
  
  output$agghitting = renderDT({
    
    agghitting()
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
  )
  
  
  ############################ Pitching
  
  
  aggpitching=reactive({
    # df=df_ %>% filter(Team=="Lugnuts")
    
    aggpitching=df_pitching() %>% 
      group_by(Team,Name) %>% 
      summarize(games=n(),
                order=round(mean(Order),2),
                IP=round(sum(IP),2),
                H=sum(H),
                ER=sum(ER),
                SO=sum(SO),
                BB=sum(BB+HBP),
                Pitches=sum(Pitches),
                Strikes=sum(Strikes)
      ) %>% 
      ungroup() %>% 
      mutate(`Strike%`=round(100*Strikes/Pitches,2),
             `SO/IP`=round(SO/IP,2),
             `BB/IP`=round(BB/IP,2),
             ERA=round(9*ER/IP,2),
             WHIP=(BB+H)/IP
      ) %>% 
      arrange(desc(IP)) 
      # mutate(`Percentile (Strike%)`=round(100*(nrow(.)-row_number())/nrow(.),2))
    
    aggpitching
    
  })
  
  
  output$aggpitching = renderDT({
    
    aggpitching()
    
  }, options = list(lengthChange = FALSE,paging = FALSE)
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
                      choices = c("All",unique(df_pitching_$Team)), 
                      selected = "All")
          
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Hitting",
                     DTOutput('agghitting')
            ),
            tabPanel("Pitching",
                     DTOutput('aggpitching')
            ),
            tabPanel("Raw Hitting",DTOutput('raw_hitting')),
            tabPanel("Raw Pitching",DTOutput('raw_pitching'))
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
