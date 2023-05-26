####################################################
##R ui script
###################################################

library(dplyr)
library(Lahman)

#modify the Teams object (creating new teams object) as per instructions
teams <- Teams %>% 

###################################################
shinyUI(fluidPage(
  fluidRow(
    
    # Application title
    titlePanel("Visualizing Baseball Data Over Time"),
    sidebarLayout(
        sidebarPanel(
            #add inputs

        ),
        mainPanel(

        )
    )
  )
))

