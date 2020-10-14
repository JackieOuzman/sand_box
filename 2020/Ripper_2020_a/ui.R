


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h1("Name of app")),

inputPanel(uiOutput("data1"),   ## uiOutput - gets the UI from the server
           uiOutput("data2"),
           uiOutput("data3")),
          h2("Cost of modification"),
mainPanel(
  hotable("hotable1"),
  h2("Yield t/ha"),
  (hotable("hotable2")),
  h2("Extra cost or benefits"),
  (hotable("hotable3")),
  
  #(verbatimTextOutput("mod1")), #change this to graph see below
  h2("Results"),
  (plotOutput("plot1")), #change this to graph
  (verbatimTextOutput("site1"))
  
)#main panel bracket
)#fluid page bracket




