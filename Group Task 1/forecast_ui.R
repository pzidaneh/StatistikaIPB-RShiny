mySelectInput <- function(x) {
    y <- colnames(df)
    cond1 <- sapply(df, is.numeric)
    
    if (length(x) == 1) {
        cond2 <- grepl(x, y, fixed = T)
    } else {
        cond2 <- grepl(paste(x, collapse = "|"), y, fixed = F)
    }
    
    selectInput("column", label = "Select Variable Input",
                choices = tidyColNames[which(cond1 & cond2)])
}

myMainPanel <- function(x) {
    return(NULL)
}

dashboardPage(
    dashboardHeader(title = "Covid-19 Viz. & Prediction"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cases", tabName = "case"),
            menuItem("Deaths", tabName = "death"),
            menuItem("Vaccination", tabName = "vaccine"),
            menuItem("Hospitalizations", tabName = "hospital"),
            menuItem("Excess Mortality", tabName = "excess")
        )
    ),
    
    dashboardBody(
        shinyDashboardThemes(theme = "grey_dark"),
        
        tabItems(
            tabItem("case",
                    fluidPage(
                titlePanel("Covid-19 Cases"),
                sidebarLayout(
                    sidebarPanel(
                        mySelectInput(c("case", "positive"))
                    ),
                    
                    mainPanel(
                        verbatimTextOutput("head"),
                        plotOutput("lineChart")
                    )
                )
            )),
            
            tabItem("death", fluidPage(
                titlePanel("Covid-19 Deaths"),
                sidebarLayout(
                    sidebarPanel(
                        mySelectInput("death")
                    ),
                    
                    mainPanel(
                        verbatimTextOutput("head"),
                        plotOutput("lineChart")
                    )
                )
            )),
            
            tabItem("vaccine", fluidPage(
                titlePanel("Covid-19 Vaccinations"),
                sidebarLayout(
                    sidebarPanel(
                        mySelectInput(c("vaccin", "booster"))
                    ),
                    
                    mainPanel(
                        verbatimTextOutput("head"),
                        plotOutput("lineChart")
                    )
                )
            )),
            
            tabItem("hospital", fluidPage(
                titlePanel("Covid-19 Hospitalizations"),
                sidebarLayout(
                    sidebarPanel(
                        mySelectInput(c("hosp", "patients"))
                    ),
                    
                    mainPanel(
                        verbatimTextOutput("head"),
                        plotOutput("lineChart")
                    )
                )
            )),
            
            tabItem("excessDeath", fluidPage(
                titlePanel("Excess Deaths"),
                sidebarLayout(
                    sidebarPanel(
                        mySelectInput("excess")
                    ),
                    
                    mainPanel(
                        verbatimTextOutput("head"),
                        plotOutput("lineChart")
                    )
                )
            ))
        )
    )
)

#runApp("C:/Users/LENOVO/Documents/00 IPB/Project R Shiny/3_Covid Dashboard")
