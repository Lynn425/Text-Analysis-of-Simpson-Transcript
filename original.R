# ===============================================
# Fill in the following fields
# ===============================================
# Title:
# Description:
# Author: 
# Date:


# ===============================================
# Packages
# ===============================================
library(tidyverse)



# ===============================================
# Import data
# ===============================================
# for demo purposes of the "template", we use data starwars
# (but you will have to replace this with the data in "simpsons-transcripts.txt")
dat <- dplyr::starwars


# ===============================================
# Define "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Title of your app"),
  fluidRow(
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "choose", 
                        label = "Choose one option", 
                        choices = c("option 1" = "opt1",
                                    "option 2" = "opt2",
                                    "option 3" = "opt3"), 
                        selected = "opt1")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           selectInput(inputId = "select", 
                       label = "Make a selection",
                       choices = c("select 1" = "sel1",
                                   "select 2" = "sel2",
                                   "select 3" = "sel3"),
                       selected = "sel1")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           radioButtons(inputId = "arrange", 
                        label = "Order bars by:", 
                        choices = c("decreasing freq" = "arr_dec",
                                    "increasing freq" = "arr_inc",
                                    "alphabetical a-z" = "arr_a2z",
                                    "alphabetical z-a" = "arr_z2a"),
                        selected = "arr_dec")
    ),
    
    # replace with your widgets
    column(3,
           p(em("Input widgets")),
           sliderInput(inputId = "size",
                       label = "Size",
                       min = 1,
                       max = 10,
                       value = 1),
           checkboxInput(inputId = "facets",
                         label = strong("Facet by gender"),
                         value = FALSE)
    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Analysis1",
                       h3("What kind of analysis1?"),
                       plotOutput("plot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Analysis2", 
                       h3("What kind of analysis2"),
                       plotOutput("plot2"),
                       hr(),
                       verbatimTextOutput('table2'))
  )
)


# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # you may need to create reactive objects
  # (e.g. data frame to be used in plot1)
  dat_freq <- reactive({
    dat %>% group_by(sex) %>% count()
  })
  
  
  # ===============================================
  # Outputs for the first TAB
  # ===============================================
  
  # code for plot1
  output$plot1 <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat_freq(), aes(x = sex, y = n)) +
      geom_col()
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    # replace the code below with your code!!!
    dat_freq()
  })
  
  
  # ===============================================
  # Outputs for the second TAB
  # ===============================================
  
  # code for plot2
  output$plot2 <- renderPlot({
    # replace the code below with your code!!!
    p1 = ggplot(data = dat, aes(x = height, y = mass)) +
      geom_point(size = input$size)
    
    if (input$facets) {
      p1 = p1 + facet_wrap(~ gender)
    }
    
    p1
  })
  
  # code for statistics
  output$table2 <- renderPrint({
    # replace the code below with your code!!!
    summary(dat$height)
  })
  
}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

