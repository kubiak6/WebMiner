#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("wordcloud2")
library('dplyr')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
  
    lapply(report.tags, function(tagName) {
      output[[paste0(tagName, "_plot")]] <- renderWordcloud2({
        reportQue[[input[["reportTitle"]]]]$input[[1]] %>%
        filter(tag==tagName) %>%
        select(term, wage) %>% 
        top_n(100, wage) %>% 
        rename(word = term, freq = wage) %>% 
        wordcloud2(size=0.5)
      })
    })
    lapply(report.tags, function(tagName) {
      output[[paste(tagName, "_", tagName)]] <- renderText({
        tagName
      })
    })
    
    output[["header"]] <- renderText({
      reportQue[[input[["reportTitle"]]]]$desc
    })

})
