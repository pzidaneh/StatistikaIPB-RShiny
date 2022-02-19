library(shiny)
library(shinydashboard)
library(dashboardthemes)

#df <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")

firstUp <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    return(x)
}

tidyColNames <- firstUp(colnames(df))
tidyColNames <- gsub("_", " ", tidyColNames)
tidyColNames <- gsub("smoothed", "(smoothed)", tidyColNames)

tidyColNames <- ifelse(grepl(" (smoothed) ", tidyColNames, fixed = T),
                       paste0(gsub(" (smoothed) ", " ", tidyColNames, fixed = T),
                              " (smoothed)"),
                       tidyColNames)

uniqueLoc <- unique(df[c('location', 'continent')])
minDate <- min(as.Date(unique(df$date)))
maxDate <- max(as.Date(unique(df$date)))

#timestamp <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data-last-updated-timestamp.txt")
