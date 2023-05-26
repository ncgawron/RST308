####################################################
##R ui script
###################################################
###
#
#       Nicholas Gawron 
#       Section 002
#       Final Project
#########################################################
library(shiny)
library(dplyr)
library(Lahman)



ModTeams <- Teams %>% 
            filter((lgID == "AL" )|( lgID == "NL")) %>% 
            filter(yearID >= 1901)%>% 
            mutate(winPerc = W/G)%>%
            mutate(RunPGame = R/G)%>% 
            mutate(HomePGame = HR/G)%>% 
            mutate(TotBGame = (H+X2B+2*X3B+3*HR)/G) %>% 
            mutate(StrOutPG = SO/G)%>% 
            mutate(SOpW = SO/BB)%>% 
            mutate(Whip = (3*(H+BB)/IPouts))%>% 
            select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, 
            WCWin, LgWin, WSWin, name,park, attendance, BPF, 
            PPF, teamIDBR, teamIDlahman45, teamIDretro))

 
coln<-colnames(ModTeams)

    ###################################################
shinyUI(fluidPage(
    fluidRow(
        
        # Application title
        titlePanel("Visualizing Baseball Data Over Time"),
        sidebarLayout(
            sidebarPanel(
                    sliderInput("yrs",
                                "Include Years in Range",
                                min = 1901,
                                max = 2020,
                                value = c(1901,2020),sep=""),
                    br(),
                    selectInput("stat2plot","Statistics to Plot",coln[-c(1:6)],selected = "HR"),
                    checkboxInput("check1","Color by League?"),
                    checkboxInput("check2","Add in trend across time?")
                
            ),
            mainPanel(
                plotOutput("Data2Plot"), 
                br(),
                DT::dataTableOutput("TableOData")
              
            )
        )
    )
))

