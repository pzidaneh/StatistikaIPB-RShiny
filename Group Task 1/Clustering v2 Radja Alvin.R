library(tidyverse)
library(shinydashboard)
library(psych)
library(factoextra)
library(ppclust)

ui <- dashboardPage(
    dashboardHeader(title = "Title"),
    dashboardSidebar(disable = T),
    dashboardBody(
        fluidPage(
            #Upload File
            box(title = "Upload File", solidHeader = T, status = "primary", height = 350,
                
                #Buat upload file
                fileInput(inputId = "file", label = "Upload file dalam bentuk .txt atau .csv", 
                          accept = c("txt/csv", ".csv", ".txt", "text/comma-separated-values,text/plain"),
                          buttonLabel = "Cari File", 
                          placeholder = "Belum ada file"), 
                
                #Milih ada header atau engga
                checkboxInput(inputId = "header", label = "Baris pertama adalah header", value = T), 
                
                #Milih jenis delimitter
                selectInput(inputId = "delim", label = "Jenis Delimiiter", 
                            choices = c("Tab" = "tab", 
                                        "Spasi" = "space",
                                        "Koma (,)" = ",",
                                        "Titik Koma (;)" = ";", 
                                        "Titik Dua (:)" = ":")), 
                
                #Pilih variabel
                selectInput(inputId = "select", 
                            label = "Pilih variabel yang akan digunakan", 
                            choices = NULL, 
                            multiple = TRUE, 
                            selected = NULL)
            ), 
            
            #Buat milih variabel dan jenis klastering
            box(
                title = "Variabel dan Jenis Clustering", solidHeader = T, status = "primary", height = 350,
                
                #Pilih jenis clustering
                selectInput(inputId = "clustering", label = "Jenis Clustering",
                            choices = c("K-Means Clustering" = "kmc", 
                                        "Fuzzy Analysis Clustering" = "fanny")
                ), 
                
                #Kriteria pemilihan buat clustering
                sliderInput("center", "How many centroid?", min=2, max=10, value=4),
                sliderInput("nstart", "How many random sets?", min=1, max=100, value=10)
            ), 
   
            #Buat bikin tab
            tabBox(title = "Output Clustering", id = "tabboxoutput", side = "right", width = 230, selected = "Output File",
                   tabPanel(
                       "Clustering Optimum", 
                       tableOutput(outputId = "clustOpt")
                   ),
                   tabPanel(
                       "Clustering Plot", 
                       plotOutput(outputId = "clusterPlot")
                   ),
                   tabPanel(
                       "Summary File",
                       tableOutput(outputId = "summary")
                   ),
                   tabPanel(
                       "Output File",
                        verbatimTextOutput(outputId = "fileOut")
                  )
            )
        )
    )
)

server <- function(input, output, session){

    #Load variabel dari data yang dibutuhkan
    dfInput <- reactive({
        req(input$file)

        df <- read.csv(input$file$datapath, sep = input$delim, header = T)

        if (input$header == T) {
            main <- colnames(df)
        } else {
            main <- NULL
        }

        return(df)
    })

    #Buat pilih variabel
    observe(
        updateSelectInput(
            inputId = "select",
            label = "Pilih variabel yang akan digunakan",
            choices = colnames(df)
        )
    )

    #Buat subsetting data
    dfSubset <- reactive({
        dfSubset <- dfInput() %>% select(!!! rlang::syms(input$select))
        return(dfSubset)
    })
    
    #Bikin summary dari variabel
    output$summary <- renderTable({
        df.sum <- describe(dfSubset())
    }, rownames = T)

    #Mulai clustering
    clustering <- reactive({
        
        #Buat normalisasi data
        df <- dfSubset()
        df <- scale(df)
        
        #Clustering berdasarkan pilihan opsi
        if (input$method == 'kmc'){
            cluster <- kmeans(df, centers = input$center, nstart = input$nstart)
            plot <- fviz_cluster(cluster, data = df)
        } else if (input$method == 'fanny'){
            cluster <- fcm(df, centers = input$center, nstart = input$nstart)
            result <- ppclust2(cluster, "fanny")
            plot <- fviz_cluster(result, data=df)
            
            return(plot)
        }
    })
    
    #Output File
    output$fileOut <- renderPrint(head(dfSubset()))
    
    #Output plot clustering
    output$clusterPlot <- renderPlot({clustering()})
    
}
shinyApp(ui, server)