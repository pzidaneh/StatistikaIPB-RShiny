function(input, output, session) {
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
        fcARIMA <- forecast::forecast(autoARIMA(), h = 60, level = c(0, 68))
        
        return(fcARIMA)
    })
    
    colourpicker::updateColourInput(session, "colour", label = "Pick A Colour",
                                    showColour = "both", palette = "square",
                                    returnName = T, allowTransparent = T)
    
    output$head <- renderPrint({
        print(paste("Entity      :", input[[input$areaType]]))
        print(paste("Variable    :", input$column))
        print(paste("Time Stamp  :", names(timestamp)))
        print(autoARIMA())
    })
    
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
