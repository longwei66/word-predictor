shinyUI(fluidPage(
        titlePanel("Word Predictor"),
        tags$hr(),
        fluidRow(
                column(7,
                       textInput(inputId = "myInputText",
                                 label = "Type the text here",
                                 value = "",
                                 width = NULL,
                                 placeholder = "Type the begining of your sentence here ...")
                )
        ),
        fluidRow(
                column(3,
                       textOutput("predictedText")
                )
        )
))