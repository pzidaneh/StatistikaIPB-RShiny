function (input, output, session) {
    df1 <- reactive({
        if (input$select == "defRice") {
            return(df)
        } else if (input$select == "defCovid") {
            df.covid <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")
            return(df.covid)
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
    
    # Menu 1: Original Data
    output$dataTable <- renderTable(ts.data())
    output$dataPlot <- renderPlot(plot(ts.data()))
    level <- c(FALSE, 68)  # need custom level
    
    fc <- reactive({
        if (input$method == "sma") {
            tsModel <- SMA(ts.data.train(), n = 3)  # custom n
            fc <- forecast(tsModel, h = input$forecastPeriod, level = level)
            
        } else if (input$method == "dma") {
            fc <- myDMA(ts.data.train(), m = 3,  # custom m
                        h = input$forecastPeriod)
            
        } else if (input$method == "ses") {
            fc <- ses(ts.data.train(), h = input$forecastPeriod,
                      alpha = NULL, level = level)  # custom alpha
            
        } else if (input$method == "des") {
            fc <- forecast(HoltWinters(ts.data.train(), gamma = FALSE),
                           h = input$forecastPeriod)
            # custom alpha-beta
            
        } else if (input$method %in% c("ar", "co", "hl")) {
            fit1 <- lm(ts.data.train() ~ time(ts.data.train()))
            futureTime <- (end(ts.data.train()) - start(ts.data.train()))/
                length(ts.data.train())
            
            fit.co <- orcutt::cochrane.orcutt(fit1, max.iter = 1000)
            rho <- fit.co$rho
            
            if (input$method == "ar") {
                pred1 <- NULL
                
            } else if (input$method == "co") {
                coef.co <- coef(fit.co)
                pred1 <- coef.co[[1]] + coef.co[[2]]*futureTime
                
            } else if (input$method == "hl") {
                fit.hl <- HoRM::hildreth.lu(df$ipm, df$tahun, rho)
                coef.hl <- coef(fit.hl)
                pred1 <- coef.hl[[1]]/(1 - rho) + coef.hl[[2]]*futureTime
                
            }
            fc <- ts(pred1)
            
        } else if (input$method == "arima") {
            autoARIMA <- forecast::auto.arima(ts.data.train())
            fc <- forecast::forecast(autoARIMA, h = input$forecastPeriod,
                                     level = level)
            
        }
        
        return(fc)
    })
    
    # hw, co, lu, ar still error
    
    output$testPrint <- renderPrint({
        print(input$select)
        print(input$firstrowHeader)
        print(paste("forecastVar: ", input$forecastVar))
        print(paste("dateVar: ", input$dateVar))
        
        print(head(df))
        #print(df1())
        #print(ts.data.train())
        
    })
    
    # Menu 1: Exploration
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
    
    # Menu 2: Forecasting
    output$testPrint2 <- renderPrint({
        try(print(length(ts.data.train())))
        try(print(ts.data.train()))
        try(print(fc()))
    })
    output$forecastTable <- renderDataTable(fc())
    output$forecastPlot <- renderPlot(plot(fc()))
    
    # Menu 3: Advanced
    output$plotDecomposition <- renderPlot(decompose(ts.data))
}
