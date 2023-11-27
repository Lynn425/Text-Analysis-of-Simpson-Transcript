# ===============================================
# Fill in the following fields
# ===============================================
# Title: Analysis of The Simpsons Transcript
# Description: This is a shiny app which analyze word frequency and word association
# Author: Yanbing Li
# Date: 12/01/2022


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(wordcloud)
library(igraph)
library(ggraph)

# ===============================================
# Import data
# ===============================================
data<- read.table("simpsons-transcripts.txt",sep="^")
simpsons<-data.frame(text=data$V5)
# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("HW6: The Simpsons Transcript"),
  fluidRow(
    h3("Widgets"),
    column(width=3,
           numericInput(inputId = "top", 
                        label = "top frequent words", 
                        value = 5,
                        min=5,
                        max=20)
    ),
    # replace with your widgets
    column(width=3,
           checkboxInput(inputId = "remove",
                         label = strong("Remove stopwords"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("Word frequency analysis"),
                       plotOutput("plot1"),
                       hr(),
                       tableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("Bigram analysis"),
                       plotOutput("plot2"),
                       hr(),
                       tableOutput('table2'))
  )
)

# ===============================================
# Define word frequency analysis
# ===============================================

server <- function(input, output) {
  tidy_simpsons<-unnest_tokens(simpsons, word, text)
  
  freqs<-reactive({
    if(input$remove){
      tidy_simpsons %>%anti_join(stop_words) %>%count(word) 
    }
    else{
      tidy_simpsons %>% count(word) 
    }
  })
    
  top_freqs <- reactive({
    freqs() %>%arrange(desc(n)) %>%slice_head(n = input$top)
  })
  
  # ===============================================
  # Define bigram analysis
  # ===============================================
 
  count_bigrams<-reactive({
    simpsons_bigrams <- simpsons %>% unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% filter(!is.na(bigram))
    bigrams_separated <- simpsons_bigrams %>% separate(bigram, c("word1", "word2"), sep = " ")
    if(input$remove){
      bigrams_filtered <- bigrams_separated %>%filter(!word1 %in% stop_words$word) %>%filter(!word2 %in% stop_words$word)
      bigrams_filtered %>%count(word1, word2, sort = TRUE)
    }
    else{
      bigrams_separated %>%count(word1, word2, sort = TRUE)
    }
    
  })
  
   
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    ggplot(data = top_freqs(),
           aes(x = reorder(word, -n), y = n)) +
      geom_col() +
      labs(title = paste("Top",input$top, "frequent words")) +
      xlab("word") +
      ylab("count")
  })
  
  # code for numeric summaries of word frequencies
  output$table1 <- renderTable({
    top_freqs()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
    if(input$remove){
      bigrams_graph <- count_bigrams() %>%filter(n > 120) %>% graph_from_data_frame()
    }
    else{
      bigrams_graph <- count_bigrams() %>%filter(n > 1600) %>% graph_from_data_frame()
    }
    set.seed(1234)
    ggraph(bigrams_graph, layout = "fr") +
      geom_edge_link() +
      geom_node_point() +
      geom_node_text(aes(label = name),
                     vjust = 1, hjust = 1) +
      labs(title = "Common bigrams in Simpsons")
  })
  
  # code for numeric summaries of word association frequencies
  output$table2 <- renderTable({
    head(count_bigrams())
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

