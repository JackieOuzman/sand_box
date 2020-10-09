


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h1("Select outcome")),

inputPanel(uiOutput("data1"),   ## uiOutput - gets the UI from the server
           uiOutput("data2"),
           uiOutput("data3")),
h2("Cost of modification"),
mainPanel(hotable("hotable1"),
          h2("Yield t/ha"),         
          (hotable("hotable2"))  
))



