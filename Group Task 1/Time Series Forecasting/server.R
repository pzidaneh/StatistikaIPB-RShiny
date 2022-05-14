function (input, output, session) {
    df1 <- reactive({
        if (input$select == "defRice") {
            return(df.rice)
        
        # Default Covid Data will take long time to read if offline
        # Will only open when ready to deploy
        
        #} else if (input$select == "defCovid") {
        #    df.covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
        #    return(df.covid)
        
        } else {
            df1 <- read.csv(input$file$datapath,
                            header = input$firstRowHeader,
                            sep = input$delim)
            return(df1)
        }
    })
    
    observe({
        if (input$select == "file"){
            req(input$file)
        }
        updateSelectInput(session, "dateVar", label = "Select Date Column",
                          choices = colnames(df1()))
        updateSelectInput(session, "forecastVar", label = "Select Forecast Column",
                          choices = colnames(df1())[sapply(df1(), is.numeric)])
        updateDateRangeInput(session, "dataPeriod", label = "Data Period:",
                             max = nrow(df1()))
    })
    
    ts.data <- reactive({
        req(input$forecastVar); req(input$dateVar)
        
        ts.data <- xts::xts(x = df1()[[input$forecastVar]],
                            order.by = as.Date(df1()[[input$dateVar]],
                                               origin = "1970-01-01"))
        # frequency = ...
        # order.by -> need handling for non-datetime index
        ts.data <- tsbox::ts_ts(ts.data)
        
        return(ts.data)
    })
    
    ts.data.train <- reactive({
        #n <- round(input$trainProportion*length(ts.data))
        #return(ts.data()[1:n])
        return(ts.data())
    })
    
    # Menu 1: Data ####
    output$dataTable <- renderTable(ts.data())
    output$dataPlot <- renderPlot(plot(ts.data()))
    level <- c(FALSE, 68)  # need custom level
    
    # Test Print Data ####
    output$testPrint <- renderPrint({
        print("Hello! Welcome to our Time-Series Forecasting Program")
        
        #print(paste("input$select", input$select))
        #print(paste("input$firstrowHeader", input$firstrowHeader))
        #print(paste("forecastVar: ", input$forecastVar))
        #print(paste("dateVar: ", input$dateVar))
        
        #print("head(df1())")
        #print(head(df1()))
        
        #print("ts.data()")
        #print(ts.data())
        
        #print("ts.data.train()")
        #print(ts.data.train())
    })
    
    # Test Print Table ####
    output$dataTable <- renderTable(head(df1()))
    
    # Menu 2: Exploration ####
    output$ADFTest <- renderPrint({
        print(adf.test(ts.data.train(), k = 7))
        print(adf.test(ts.data.train(), k = 30))
        print(adf.test(ts.data.train(), k = 90))
        # must allow user to choose their own k (lag)
        # and/or show plot of all p-value
    })
    
    output$plotACF <- renderPlot(acf(ts.data.train()))
    output$plotPACF <- renderPlot(pacf(ts.data.train()))
    # must allow user to choose their own max.lag
    
    output$EACF <- renderPrint(eacf(ts.data.train()))
    # must allow user to choose their own max AR/MA order
    
    # Menu 3: Forecasting
    fc <- reactive({
        if (input$method == "hw") {
            tsModel <- HoltWinters(ts.data.train())
            
        } else if (input$method == "arima") {
            tsModel <- auto.arima(ts.data.train())
            
        }
        fc <- forecast(tsModel, h = input$forecastPeriod, level = level)
        return(fc)
    })
    
    output$testPrint2 <- renderPrint({
        try(print(length(ts.data.train())))
        try(print(ts.data.train()))
        try(print(fc()))
    })
    output$forecastTable <- renderDataTable(fc())
    output$forecastPlot <- renderPlot(plot(fc()))
    
    # Menu 4: Advanced
    output$plotDecomposition <- renderPlot(decompose(ts.data))
}
