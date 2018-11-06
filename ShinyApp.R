
require(shiny)
require(data.table)

# Define UI for PredictingNextWord ----
ui <- pageWithSidebar(
    
    # App title ----
    headerPanel("Predict Next Word"),
    
    # Sidebar panel for inputs ----
    sidebarPanel(
        
        textInput("inputText","Enter text here:"),
        
        submitButton(text="Submit")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
        
        h4("Instructions:"),
        h5("Enter minimum of one word in the text box on the left and press Enter or click Submit."),
        
        
        
        h4("Suggested words that may follow the entered text are:"),
        
        
        h3(textOutput("text1")),
        
        h6("Suggested on:"),h6 (textOutput("text2"))
    )
)

## Define server logic ---

##---------------------------Retreive n-grams from the database------------------
source("FindNextWord.R",local=FALSE)


server <- function(input, output) {
    
    # This is in a reactive expression
    #https://shiny.rstudio.com/articles/understanding-reactivity.html
    predictWords <- reactive({
        words<-paste(unlist(predictNextWord(input$inputText)),collapse=" , ")
        words<-paste(words)
        words
    })
    
    
    dateNtime<-reactive({
        words<-paste(unlist(predictNextWord(input$inputText)),collapse=" , ")
        dt<-paste(Sys.time())
    })
    
    # Return the formula text for printing ----
    output$text1 <- renderText({
        predictWords()
    })
    
    output$text2<-renderText({
        dateNtime()   
    })
}

shinyApp(ui,server)

