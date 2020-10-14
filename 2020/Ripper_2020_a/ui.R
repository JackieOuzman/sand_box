


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel(h1("Name of app")),

fluidRow(
  column(width=4, uiOutput("data1")),   ## uiOutput - gets the UI from the server
  column(width=4, uiOutput("data2")),
  column(width=4, uiOutput("data3"))
),#fluid row bracket 1
## Cost table
fluidRow(
  column(width=6,h2("Cost of modification Scenario 1")),
  column(width=6,h2("Cost of modification Scenario 2"))
),#fluid row bracket 2
  fluidRow(
    column(width=6,hotable("hotable1")),
    column(width=6,hotable("hotable1_a")) #can't output the exact same tabel
  ),#fluid row bracket 3
## Yield table
fluidRow(
  column(width=6,h2("Yield t/ha Scenario 1")),
  column(width=6,h2("Yield t/ha Scenario 2"))
),#fluid row bracket 4
fluidRow(
  column(width=6,hotable("hotable2")),
  column(width=6,hotable("hotable2_a")) #can't output the exact same tabel
),#fluid row bracket 5

## Extra table
fluidRow(
  column(width=6,h2("Extra cost or benefits Scenario 1")),
  column(width=6,h2("Extra cost or benefits Scenario 2"))
),#fluid row bracket 6
fluidRow(
  column(width=6,hotable("hotable3")),
  column(width=6,hotable("hotable3_a")) #can't output the exact same tabel
),#fluid row bracket 7

## Results
fluidRow(
  column(width=12,h2("Results Scenario 1 and 2"))
),#fluid row bracket 8
fluidRow(
  column(width=12,plotOutput("plot1")),
  
),#fluid row bracket 9
) #fluidPage bracket

   
##(verbatimTextOutput("mod1")), #change this to graph see below
##  (verbatimTextOutput("site1"))





