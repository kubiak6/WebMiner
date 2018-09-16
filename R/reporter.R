reportQue <- list()

reporter.addOutput <- function(title, desc, input, param=NA)
{
  reportOutput <- list(title=character(), desc=character(), input=list(), param=numeric())
  index <- length(reportOutput$title) + 1
  reportOutput$title[index] <- title
  reportOutput$desc[index] <- desc
  reportOutput$input[[index]] <- input
  class(reportOutput$input[[index]]) <- class(input)
  reportOutput$param[index] <- param

  nextInt <- length(reportQue) + 1
  reportQue[[nextInt]] <<- reportOutput
  names(reportQue)[nextInt] <<- title

}

reporter.saveOutput <- function()
{
  print(paste0(Sys.time(),": Reporter: saving ", length(reportQue), " reports to publish."))
  save(reportQue, file = "report/ShinyWebMinerReport/data/reportData.RData")
}

reporter.plot <- function(x,...) UseMethod("reporter.plot")

reporter.plot.sentiment <- function(input, plotParam=NA)
{
  output <- list()
  
  outPlot <- input %>% 
    ggplot(aes(x=mod_dt, y=sentiment, fill=tag)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c('#FD6A02', '#7f51be','#32CD32', '#FF0085')) +
    theme(legend.position="bottom")

  output[[1]] <- outPlot
   
  outPlot <- input %>% 
    ggplot(aes(x=mod_dt, y=sentiment, fill=tag)) +
    geom_bar(stat="identity", position = "dodge", show.legend=FALSE) +
    geom_smooth(size = 1, se = TRUE, colour = "black", show.legend=FALSE) + 
    facet_wrap(~tag, ncol = 2, scales = "free") +
    scale_fill_manual(values=c('#FD6A02', '#7f51be','#32CD32', '#FF0085'))
    
  output[[2]] <- outPlot
  
  
  return(output)
}



reporter.plot.ngram <- function(input, outputName, plotParam)
{
  
  print(paste0(Sys.time(),": Miner: generating plot..."))
  
  # input %>% 
  #   group_by(tag) %>% 
  #   arrange(desc(wage)) %>%
  #   top_n(10, wage) %>%
  #   ungroup() %>% 
  #   ggplot(aes(reorder(term, wage), wage, fill = tag)) +
  #   geom_col(show.legend = FALSE) +
  #   labs(x = NULL, y = "wage") +
  #   facet_wrap(~tag, ncol = 2, scales = "free") +
  #   coord_flip()
  # ggsave(paste0("output/plot_", outputName,".png"), width = 16, height = 6)
  
  for(tagName in crawler.tags) {
    print(paste0("Output for ",tagName))
    png(paste0("output/prod/cloud_",outputName, "_", tagName, ".png"), width=1000, height=1000)
    
    output[[tagName]] <- input %>%
      filter(tag==tagName) %>%
      with(wordcloud(term, tf_idf, max.words = plotParam,
                     random.order=FALSE, rot.per=0.1,
                     colors=c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C","#6FFDEE", "#FF7F00")))
    #brewer.pal(8, "Paired")
    dev.off()
  }
}

reporter.plot.sentiment <- function(input, outputName)
{
  
  input %>% 
    ggplot(aes(x=mod_dt, y=sentiment, fill=tag)) +
    geom_bar(stat="identity", position = "dodge") +
    scale_fill_manual(values=c('#FD6A02', '#7f51be','#32CD32', '#FF0085')) +
    theme(legend.position="bottom")
  
  ggsave(paste0("output/prod/plot_", outputName,".png"), width = 10, height = 6)
  
  #tagColor <- hash( keys = crawler.tags, values = c('#7f51be', '#32CD32', '#FD6A02', '#FF0085'))
  
  input %>% 
    ggplot(aes(x=mod_dt, y=sentiment, fill=tag)) +
    geom_bar(stat="identity", position = "dodge", show.legend=FALSE) +
    geom_smooth(size = 1, se = TRUE, colour = "black", show.legend=FALSE) + 
    facet_wrap(~tag, ncol = 2, scales = "free") +
    scale_fill_manual(values=c('#FD6A02', '#7f51be','#32CD32', '#FF0085'))
  
  ggsave(paste0("output/prod/plot2_", outputName,".png"), width = 10, height = 6)

}

