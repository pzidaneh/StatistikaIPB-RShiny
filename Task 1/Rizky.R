library(shiny)

ui <- fluidPage(
  titlePanel("Muhammad Rizky Nurhambali"),
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "file1", label = "Masukkan File", multiple = FALSE, accept = ".txt", width = NULL,
                buttonLabel = "Cari File...", placeholder = "Tidak ada file yang dipilih"),
      textInput(inputId = "warna", label = "Masukan Kode Hex Warna", placeholder="#000000", value = NULL),
      checkboxInput(inputId = "header", label = "Baris pertama nama kolom", value = F, width = NULL),
      checkboxInput(inputId = "urut", label = "Urutkan Berdasarkan Frekuensi", value = F, width = NULL),
      checkboxInput(inputId = "putarBalik", label = "Putar Balik Urutan Frekuensi", value = F, width = NULL),
      checkboxInput(inputId = "horiz", label = "Bentuk Horizontal", value = F, width = NULL)
    ),
    mainPanel(
      tableOutput(outputId = "freqTable"),
      plotOutput(outputId = "freqBarplot")
    )
  )
)

server <- function(input, output, session){
  Var1 <- reactive({file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "txt", "Harap unggah file .txt"))
  
    Var1 <- read.table(file$datapath, header = input$header)
  
    if(input$header == T) {
      main <- colnames(Var1)
    
    } else {
      main <- NULL
    }
    
    return(Var1)
    
  })
  
  tab <- reactive({
    tab1 <- table(Var1())
    
    if(input$urut == T) {
      tab1 <- sort(tab1)
    }
    
    if(input$putarBalik == T) {
      tab1 <- rev(tab1)
    }
    
    return(tab1)
  })
  
  output$freqTable <- renderTable({
    
    return(tab())

  })
  
  output$freqBarplot <- renderPlot({
    
    return(barplot(tab(), col = input$warna, horiz = input$horiz))
    
  })
}

shinyApp(ui,server)