dashboardPage(
    dashboardHeader(title = "Forecast"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "dt",icon = icon("database")),
            menuItem("Exploration", tabName = "explore", icon = icon("search")),
            menuItem("Forecast", tabName = "fore", icon = icon("chart-line")),
            menuItem("Advanced", tabName = "adv", icon = icon("table")),
            # Decomposition, decomp -> Advanced, adv
            h3(),
            
            selectInput("select", label = "Select Data",
                        choices = c("Default (Rice Prices)" = "defRice",
                                    #"Default (Covid-19)" = "defCovid",
                                    # add new dataset
                                    "Custom" = "file")),
            
            conditionalPanel(condition = "input.select == 'file'",
                             fileInput("fileinput", "Insert File")),
            
            #checkboxInput("first.row.header", "First Row as Header"),
            checkboxInput("firstrowHeader", "First Row as Header"),
            
            #selectInput("DateVar", label = "Select a Date",
            #            choices = colnames(df)),
            selectInput("dateVar", label = "Select Date Column",
                        choices = NULL),
            
            selectInput("forecastVar", label = "Select Forecast Column",
                        choices = NULL),  # make NULL for updateSelectInput
            
            sliderInput("dataPeriod", "Data Period",
                        min = 0, max = 100, value = 0),
            # need start & end date, not just the period itself
            # perhaps dataPeriod -> startDate & endDate?
            
            sliderInput("forecastPeriod", "Forecast Period Ahead",
                        min = 0, max = 100, value = 10)
        )
    ),
    
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        shinyDashboardThemes("poor_mans_flatly"),
        
        
        tabItems(
            tabItem("dt",
                    fluidPage(
                        titlePanel("Data"),
                        
                        fluidRow(
                            # Test Print Data ####
                            box(title = "Test Print", width = 10, solidHeader = T,
                                verbatimTextOutput(outputId = "testPrint")),
                            
                            box(title = "Table", solidHeader = T,
                                tableOutput(outputId = "dataTable")),
                            
                            box(title = "Plot", width = 10 ,solidHeader = T,
                                plotOutput(outputId = "dataPlot"))
                        )
                    )
            ),
            
            tabItem("explore",
                    fluidPage(
                        titlePanel("Exploration"),
                        
                        fluidRow(
                            box(title = "ADF Test", solidHeader = T,
                                verbatimTextOutput(outputId = "ADFTest")),
                            
                            box(title = "ACF Plot", solidHeader = T,
                                plotOutput(outputId = "PlotACF")),
                            
                            box(title = "PACF Plot", solidHeader = T,
                                plotOutput(outputId = "PlotPACF")),
                            
                            box(title = "EACF Plot", solidHeader = T,
                                verbatimTextOutput(outputId = "PlotEACF")),
                        )
                    )
            ),
            
            
            
            tabItem("fore",
                    fluidPage(
                        titlePanel("Forecasting"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("method", "Select a Method",
                                            choices = c(
                                                "Holt-Winter" = "hw",
                                                "ARIMA Models" = "arima"
                                            ))
                            ),
                            
                            mainPanel(
                                verbatimTextOutput("testPrint2"),
                                tableOutput(outputId = "forecastTable"),
                                plotOutput(outputId = "forecastPlot")
                            )
                        )
                    )
            ),
            
            tabItem("adv",  # from "decomp" to "adv"
                    fluidPage(
                        titlePanel("Advanced"),  # from Decomposition to Advanced
                        
                        mainPanel(
                            plotOutput(outputId = "plotDecomposition"),
                            tableOutput(outputId = "metrics")  # metrik to metrics
                        )
                    )
            )
        )
    )
)
