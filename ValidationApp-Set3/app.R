library(shiny)
library(tidyverse)


# Define UI for slider demo application
ui <- pageWithSidebar(
  
  #  Application title
  headerPanel(" "),
  
  sidebarPanel(
    h6(textOutput("save.results")), # This is intentionally an empty object.
    h4("Experiment on classifying diplomatic language."),
    tags$a("Contact: Christian Rauh", 
           href="http://www.christian-rauh.eu"),
    h6(""),
    h6("Details on research purpose and results are available after the coding exercise."),
    h6(""),
    h6(textOutput("counter")),
    h6(""),
    h4("\nMany thanks for your help!\n"),
    # Display the page counter text.
    
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    # Main Action is where most everything is happening in the
    # object (where the welcome message, survey, and results appear)
    uiOutput("MainAction"),
    # This displays the action Button Next.
    actionButton("Click.Counter", "Next")    
  )
)


server <- function(input, output) {
  
  # Load the set of sentences to be coded
  Qlist <<- read_rds("Set3.rds")
  # Qlist <<- Qlist[1:5,] # Shorter Version for testing purposes
  
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(Qlist))
  # Name each element of the vector based on the
  # first column of the Qlist (sentence ID)
  # names(results)  <<- Qlist[,1]

  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("You are coder ", counter, ".")
    })
  
  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0) {
      return(
        list(
          h4(" \nWelcome!"),
          h5("In the following you'll see 50 randomly selected sentences from international diplomatic speeches."),
          strong("For every sentence, please choose whether it conveys a more conflictual or a more cooperative message from your point of view."),
          br(""),
          p("Just decide spontaneously without thinking too much about possible contexts. There is no right or wrong, it's only your perception that matters."),
          br("")
        )
      )
    } 

    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
      return(
        list(
          h5(paste(input$Click.Counter, "/", nrow(Qlist))), # Progress
          h3(textOutput("question")),
          radioButtons("survey", "\nIn your view, the message of this sentence is ...",
                       choices = c("Conflictual", "Somewhat conflictual", "Neutral", "Somewhat cooperative", "Cooperative"),
                       selected = NA, inline = F)
        )
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter>nrow(Qlist))
      return(
        list(
          # h4("View aggregate results"),
          # tableOutput("surveyresults"),
          h4("Many thanks for your help!"),
          downloadButton('downloadData', 'Download your results'),
          br(),
          h6("This button won't work ...")
        )
      )    
  })
  
  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    # try is used because there is a brief moment in which
    # the if condition is true but input$survey = NULL
    if ((input$Click.Counter>0)&(input$Click.Counter<=nrow(Qlist)))
      try(results[input$Click.Counter] <<- input$survey)

    # If the user has clicked through all of the survey questions
    # save the user choices to the question/text list, store the user number (from counter),
    # and append data to pre-existing data
    if (input$Click.Counter==nrow(Qlist)+1) {
      Qlist$human <<- results[1:nrow(Qlist)]
      Qlist$coder <<- counter
      
        if (file.exists("survey.results.rds")){
          presults <- read_rds(file="survey.results.rds")
        }
        if (!file.exists("survey.results.rds")){
          presults<-NULL
        }
        presults <- presults <<- rbind(presults, Qlist)
        write_rds(presults, file="survey.results.rds")
    }
    # Because there has to be a UI object to call this
    # function I set up render text that displays the content
    # of this function.
    ""
  })
  
  # # This function renders the table of results from the
  # # survey.
  # output$surveyresults <- renderTable({
  #   t(summary(presults))
  # })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(Qlist, file, row.names = F)
    }
  )
  
  # # The option list is a reactive list of elements that
  # # updates itself when the click counter is advanced.
  # option.list <- reactive({
  #   qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
  #   # Remove items from the qlist if the option is empty.
  #   # Also, convert the option list to matrix. 
  #   as.matrix(qlist[qlist!=""])
  # })
  
  # This function shows the question text.
  output$question <- renderText({
    paste0(
      Qlist$sentence[input$Click.Counter]
    )
  })
  
}


shinyApp(ui, server)