library(shiny)
library(graphics)
library(shinydashboard)
library(DT)

ui <- dashboardPage(skin = "purple",
    dashboardHeader(title = "My Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Iris", tabName = "iris", icon = icon("tree")),
            menuItem("Cars", tabName = "cars", icon = icon("car")),
            menuItem("Tugas1", tabName = "tugas1", icon = icon("file")),
            menuItem("Tugas2", tabName = "tugas2", icon = icon("file"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("iris",
                    box(plotOutput(outputId = "correl"), width = 8),
                    box(
                        selectInput(inputId = "features", label = "Features:",
                                    c("Sepal.Width", "Petal.Length", "Petal.Width")), width = 4
                    )),
            tabItem("cars",
                    fluidPage(
                        h1("Cars"),
                        dataTableOutput(outputId = "carstable")
                    )),
            
            tabItem("tugas1", 
                    fluidPage(
                        titlePanel("Faridatun Nisa"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                fileInput(inputId = "file", label = h3("Choose Text File"), 
                                          accept = ".txt"),
                                
                                checkboxInput(inputId = "header", 
                                              label = "Baris pertama nama kolom"),
                                
                                checkboxInput(inputId = "sort",
                                              label = "Urutkan kategori berdasarkan nilai"),
                                
                                br(),
                                
                                textInput(inputId = "color", label = "Masukkan Warna / Kode Warna",
                                          placeholder = "black atau #000000", value = "black"),
                                
                                br(),
                                
                                radioButtons(inputId = "statistik", label = "Statistik:",
                                             choices = c("Frekuensi" = "freq",
                                                         "Persentase" = "persentase"))
                            ),
                            
                            mainPanel(
                                verbatimTextOutput(outputId = "textOutput"),
                                tableOutput(outputId = "tabFreq"),
                                plotOutput(outputId = "barChart")
                            )
                        )
                    )
            ),
            
            tabItem("tugas2",
                    fluidPage(
                        titlePanel("Eksplorasi Data"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                fileInput(inputId = "file", label = "Choose File"),
                                
                                checkboxInput(inputId = "header", label = "Baris pertama nama kolom", value = TRUE),
                                
                                
                                selectInput(inputId = "select", label = "Pilih Variabel", choices = "NULL"),
                                
                                radioButtons("delimiter", "Delimiter:",
                                             c("Semicolon (;)"=";",
                                               "Dot (.)"=".",
                                               "Comma (,)"=",",
                                               "Space ( )"=" ")),
                                
                                img(src="task2_logo.png", width = "30%")
                            ),
                            
                            mainPanel(
                                dataTableOutput(outputId = "table"),
                                plotOutput(outputId = "histogram"),
                                plotOutput(outputId = "boxplot")
                            )
                        )
                    )
                    )
        )
        
    )
)

server <- function(input, output, session){
    output$correl <- renderPlot({
        plot(iris$Sepal.Length, iris[[input$features]],
             xlab = "Sepal Length", ylab = "Feature")
    })
    
    output$carstable <- renderDataTable(mtcars)
    
    var <- reactive({
        req(input$file)
        
        if(is.null(input$file)) {return()}
        var <- (read.csv2(input$file$datapath, 
                          header = input$header, sep = input$delimiter))
        
        if(input$header == T) {
            main <<- NULL
        }
        
        else if (input$header == F) {
            main <<- colnames(var)
        }
        
        return(var)
        
    })
    
    observe(
        updateSelectInput(session = session, "select",label = "variabel", 
                          choices = colnames(var())))
    
    
    tab <- reactive({
        tab1 <- var()[[input$select]]
        tab1 <- as.numeric(tab1)
        
    })
    
    output$table <- renderDataTable(var(), options = list(pageLength = 6))
    
    output$histogram <- renderPlot(hist(tab(), xlab = input$select, main = paste("Histogram of", input$select)))
    
    output$boxplot <- renderPlot(boxplot(tab(), xlab = input$select, main = paste("Boxplot of", input$select)))
    
    
    
    dt <- reactive({
        ext <- tools::file_ext(input$file$datapath)
        req(input$file)
        validate(need(ext == "txt", "Please upload a txt file"))
        dt <- read.table(input$file$datapath, header = input$header)
        if (input$header == T) {
            main <<- NULL
        } 
        
        else if(input$header == F) {
            main <<- colnames(dt)
        }
        
        return(dt)
        
    })
    
    tabel <- reactive({
        tabel <- table(dt())
        
        if (input$sort == T) {
            tabel <- sort(tabel, T)
        }
        
        if (input$statistik == "freq") {
            
            ylab <<- "Frekuensi"
            main <<- "Frekuensi Kategori"
            
            if (input$sort == T) {
                Frekuensi <- sort(table(dt()), T)
                return (Frekuensi)
            }
            
            else if (input$sort == F) {
                Frekuensi <- table(dt())
                return (Frekuensi)
            }
            
            
            return(tabel)
        }
        
        else if (input$statistik == "persentase") {
            
            ylab <<- "Persentase"
            main <<- "Persentase Kategori"
            
            if (input$sort == T) {
                Persentase <- sort(table(dt())/sum(table(dt()))*100, T)
                return (Persentase)
            }
            
            else if (input$sort == F) {
                Persentase <- table(dt())/sum(table(dt()))*100
                return (Persentase)
            }
            
            
            return(tabel/sum(tabel)*100)
        }
        
        
        return(tabel)
    })
    
    
    output$tabFreq <- renderTable({
        return(tabel())
    })
    
    output$barChart <- renderPlot({
        return(barplot(tabel(), ylab = ylab, main = main, col=input$color))
    })
    
    
    
}

shinyApp(ui, server)

