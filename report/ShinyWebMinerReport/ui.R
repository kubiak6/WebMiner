#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

load("data/reportData.RData")
report.tags <- c("play", "plus", "orange", "t-mobile")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Raport analizy umieszczanych treści na serwisie Telepolis"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
        selectInput("reportTitle", "Wybierz raport",
                    choices = names(reportQue))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2(textOutput("header")),
      lapply(report.tags, function(tagName) {
        
        list(h3(textOutput(paste(tagName, "_", tagName))),
        wordcloud2Output(paste0(tagName, "_plot")))
      })
    )
  )
))
