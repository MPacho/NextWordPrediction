library(shiny)

shinyUI(fluidPage(theme="style_word_prediction.css",
      titlePanel("Next Word Prediction"),
  
      tabsetPanel(
    
        # App tab
        tabPanel("App",
             
          # Description       
          fluidRow(
            tags$div(class="header", id="desc",
                    tags$p("Type a sentence. Three next word sugestions will appear below.")
            )
          ),
      
          # Text area
          fluidRow(
            textAreaInput("typeText", "", value = "", placeholder="Type here...")
          ),
  
          # Output area
          fluidRow(
            column(4,
              textOutput("wordPrediction2")),
            column(4,
              textOutput("wordPrediction1")),
            column(4,
              textOutput("wordPrediction3"))
          )
        ),
    
    
        # Info tab
        tabPanel("Info",
          fluidRow(
          tags$div(class="header", id="desc",
            tags$p("Author: Magdalena Paszko"),
            tags$p("December 2017"),
            tags$p("This app gives top three next word suggestions given a sequence of
                  words entered by the user. The language model used for prediction is
                  4-gram with stupid backoff algorithm with a customary value of the discount
                  factor lambda = 0.4. The model was trained on 40% of blogs and news SwiftKey
                  data in English."),
            tags$p("For R code, visit my ",tags$a(target="_blank", href="https://github.com/MPacho/NextWordPrediction", "Github repo"),".")
        
            )
          )
        )
      )
))