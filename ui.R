

library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Scotts Forecasting"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Spatial", tabName = "Spatial", icon = icon("th")),
      menuItem("Time", tabName = "Time", icon = icon("dashboard"))
     
    )
  ),
  ## Body content
  dashboardBody(
    

    tabItems(
      # Second tab content
      tabItem(tabName = "Time",
              mainPanel(
                h4("We are forecasting the Lawn sales for scotts. The prediction model is trained on data of 2013-10-06 -- 2016-06-25,
                   and forecast on 2016-06-25 -- 2017-06-25. There are 4 major Retaliers and 205 DMA. DMA level forecasting are visualized in 
                   previous map." ),
                
                plotlyOutput('plottrend')
                )
              
      ),
      
      # First tab content
      tabItem(tabName = "Spatial",
           
              
              # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
              fluidPage( leafletOutput("map"),
                         absolutePanel(top = 10, right = 10,
                                       sliderInput("range", "Range", min(quakes$mag), max(quakes$mag),
                                                   value = range(quakes$mag), step = 0.1
                                       )
                                       
                         ))
              
              
      )
   
    )
  )
)