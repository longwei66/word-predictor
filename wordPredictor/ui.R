shinyUI(fluidPage(
        titlePanel("Word Predictor"),
        
        sidebarLayout(
                sidebarPanel(
                        #helpText("Type few words and let this app predict next word"),
                        
                        textInput(inputId = "myInputText",
                                  label = "Type the text here",
                                  value = "",
                                  width = NULL,
                                  placeholder = "Type the begining of your sentence here ...")
                ),
                
                mainPanel(
                        textOutput("predictedText")
                )
        )
))