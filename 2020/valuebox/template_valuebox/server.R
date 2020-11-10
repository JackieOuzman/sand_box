#### developer: Tamas Markó
### email: tamas.marko@myhappydata.com
library(shiny)

#-------------------------------------
# SHINY
#-------------------------------------	 

shinyServer(
    function(input, output) {
        output$mean_mpg<- renderText({ 
            round(mean(mtcars$mpg),1)
        })
    }
)  