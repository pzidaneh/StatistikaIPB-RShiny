library(shiny)
library(shinydashboard)
source("D:/IPB/R-Shiny Project/data.R")

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "COVID-19 Visualization and Forecasting"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Plot", tabName = "plot",icon = icon("database")),
                        menuItem("Forecast", tabName = "forecast", icon = icon("chart-line")),
                        menuItem("Table", tabName = "table", icon = icon("table")),
                        
                        h3("Entity Selection"),
                        
                        radioButtons("areaType", label = "Type:",
                                     choices = c("Individual Country" = "country",
                                                 "Group by Region" = "region",
                                                 "Group by Income" = "income")),
                        
                        conditionalPanel("input.areaType == 'country'",
                                         selectInput("country",
                                                     label = "Select Country:",
                                                     choices = uniqueLoc$location[
                                                       nchar(uniqueLoc$continent) > 0
                                                     ])),
                        
                        conditionalPanel("input.areaType == 'region'",
                                         selectInput("region",
                                                     label = "Select Region Group:",
                                                     choices = uniqueLoc$location[
                                                       nchar(uniqueLoc$continent) == 0 &
                                                         !grepl("income", uniqueLoc$location)
                                                     ])),
                        
                        conditionalPanel("input.areaType == 'income'",
                                         selectInput("income",
                                                     label = "Select Income Group:",
                                                     choices = c("Low income",
                                                                 "Lower middle income",
                                                                 "Upper middle income",
                                                                 "High income"))),
                        
                        selectInput("column", label = "Select Variable of Interest",
                                    choices = colnames(df)[sapply(df, is.numeric)]),
                        
                        h3("Plot Customization"),
                        
                        selectInput("lineWidth", label = "Line width",
                                    choices = c(1, 2, 3, 4, 5),
                                    selected = 1),
                        
                        # Colour Picker
                        colourpicker::colourInput("colour", label = "Pick A colour",
                                                  value = "black", showColour = "both",
                                                  palette = "square", returnName = T,
                                                  allowTransparent = T),
                        
                        h3("Model Optimization"),
                        
                        checkboxInput("season", label = "Use seasonal model"),
                        checkboxInput("approx",
                                      label = "Use approximated information criterion"),
                        checkboxInput("xreg", label = "Insert exogenous variable")
                      )
                    ),
                  
                  dashboardBody(
                    tabItems(
                      tabItem("plot",
                        fluidPage(
                          titlePanel(title = "COVID-19 Plot"),
                            
                            mainPanel(
                              verbatimTextOutput("head")
                              )
                            )
                          ),
                        
                      
                      
                    tabItem("forecast",
                      fluidPage(
                        titlePanel(title = "COVID-19 Forecast"),
                          
                          mainPanel(
                            plotOutput("lineChart")
                            )
                          )
                        ),
                      
                  
                  tabItem("table",
                    fluidPage(
                      titlePanel(title = "COVID-19 Table"),
                        mainPanel(tableOutput("tableDetails"))
                        )
                      )
                    )
                  )
                )

server <- function(input, output, session){
  df1 <- reactive({
    df1 <- df[df$location == input[[input$areaType]],
              c(input$column, "date")]
    
    df1 <- data.frame(Date = as.Date(df1$date),
                      y = df1[[input$column]])
    
    colnames(df1) <- c("Date", input$column)
    
    return(df1)
  })
  
  autoARIMA <- reactive({
    if (input$season == F) {
      autoARIMA <- forecast::auto.arima(df1()[input$column],
                                        seasonal = F,
                                        approximation = input$approx)
      
    } else if (input$season == T) {
      autoARIMA <- bayesforecast::auto.sarima(as.ts(df1()[[input$column]]),
                                              seasonal = T)
    }
    
    # xreg(?)
    return(autoARIMA)
  })
  
  fcARIMA <- reactive({
    fcARIMA <- forecast::forecast(autoARIMA(), #h = dim(df1())[1],
                                  h = 60,
                                  level = c(0, 68))
    
    return(fcARIMA)
  })
  
  colourpicker::updateColourInput(session, "colour", label = "Pick A Colour",
                                  showColour = "both", palette = "square",
                                  returnName = T, allowTransparent = T)
  
  df2 <- reactive({
    
    #tabel dengan negara terpilih, tanggal terbaru, total cases, new cases
    tabLoc <- df[df$location == input[[input$areaType]],
                 c("location", input$column, "date")]
    colnames(tabLoc)<-c("Selected", input$column, "Date")
    tabLoc$Date <- as.Date(tabLoc$Date)
    tabLoc <- tabLoc[tabLoc$Date == max(tabLoc$Date),]
    
    return(tabLoc)
    
  })
  
  output$tableDetails <- renderTable(
    return(df2())
  )
  
  output$head <- renderPrint({
    print(paste("Entity      :", input[[input$areaType]]))
    print(paste("Variable    :", input$column))
    print(paste("Time Stamp  :", names(timestamp)))
    print(autoARIMA())
  })
  
  #output$tableDetails <- renderTable(
  #  colnames = c("Country", "Total cases", "New cases")
  #)
  
  output$lineChart <- renderPlot({
    #plot(df1(), type = "l", col = input$colour, lwd = input$lineWidth,
    #     xlim = c(minDate, maxDate),
    #     main = paste(input[[input$areaType]], input$column))
    #lines(fcARIMA$mean)
    plot(fcARIMA(), col = input$colour, lwd = input$lineWidth,
         #xlim = c(minDate, maxDate),
         ylim = c(0, max(c(df1()[[input$column]], fcARIMA()$upper), na.rm = T)))
  })
}

shinyApp(ui, server)