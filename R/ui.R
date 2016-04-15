library(shiny)

shinyUI(
    fluidPage(
        # Application title
        titlePanel("Word Prediction"),
        wellPanel("Auto-completion is a common function on mobile devices.  As
                  a user types, an auto-completion function presents that user
                  with possible completions to the current word being typed or
                  probable words that could follow the current word after it is
                  typed.  This project intends to provide the latter function -
                  word-prediction."),
        inputPanel(
            textInput(
                "phrase",
                "Text Input: ",
                value = ""
            ),
            submitButton("Submit")
        ),
        mainPanel(
            textOutput("word")
        )
    )
)
