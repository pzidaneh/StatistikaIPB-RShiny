function (input, output, session) {
    df1 <- reactive({
        if (is.null(input$file)) {
            return(df)
        } else {
            df1 <- read.csv(input$file$datapath,
                            header = input$firstRowHeader,
                            sep = input$delim)
            return(df1)
        }
    })
    
    ts.data <- reactive({
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
    
    fc <- reactive({
        if (input$method == "sma") {
            tsModel <- SMA(ts.data.train(), n = 3)  # custom n
            fc <- forecast(tsModel, h = input$forecastPeriod)$mean
            
        } else if (input$method == "dma") {
            fc <- myDMA(ts.data.train(), m = 3,  # custom m
                        h = input$forecastPeriod)$mean
            
        } else if (input$method == "ses") {
            fc <- ses(ts.data.train(), h = input$forecastPeriod,
                      alpha = NULL)$mean  # custom alpha
            
        } else if (input$method == "des") {
            fc <- forecast(HoltWinters(ts.data.train(), gamma = FALSE),
                           h = input$forecastPeriod)$mean  # custom alpha-beta
            
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
                                         level = c(0, 68))
                
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
    
    # Menu 2: Forecasting
    output$testPrint2 <- renderPrint({
        try(print(length(ts.data.train())))
        try(print(ts.data.train()))
        try(print(fc()))
    })
    output$forecastTable <- renderTable(fc())
    output$forecastPlot <- renderPlot(plot(fc()))
    
    # Menu 3: Advanced
    output$plotDecomposition <- renderPlot(decompose(ts.data))
}