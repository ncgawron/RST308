#
###################################################
###
#
#       Nicholas Gawron 
#       Section 002
#       Final Project
#########################################################

library(shiny)
library(ggplot2)
library(Lahman)
 
#modify the Teams object (creating new teams object) as per instructions

ModTeams <- Teams %>% filter(lgID == "AL" | lgID == "NL") %>% filter(yearID >= 1901)%>% 
  mutate(winPerc = W/G)%>% mutate(RunPGame = R/G)%>% mutate(HomePGame = HR/G)%>% 
  mutate(TotBGame = (H+X2B+2*X3B+3*HR)/G) %>% mutate(StrOutPG = SO/G)%>% 
  mutate(SOpW = SO/BB)%>% mutate(Whip = (3*(H+BB)/IPouts))%>% 
  select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name,park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

 output$Data2Plot<-renderPlot({
  plottingvar <-  input$stat2plot
  Years2Plot <- input$yrs
  dotplot<-ggplot(ModTeams, aes_string(x = "yearID", y= input$stat2plot))
 g<- dotplot+geom_point()
  
  #ifelse(input$check1, g<- dotplot+geom_point(aes(col=lgID)),g<- dotplot+geom_point())
  #dotplot

 
 if(input$check1 & !input$check2){
   g<- dotplot+geom_point(aes(col=lgID))
 } else if (!input$check1 & input$check2){
   g<- dotplot+geom_point()+geom_smooth()
 } else if (input$check1 & input$check2){
   g<- dotplot+geom_point(aes(col=lgID))+geom_smooth(aes(col=lgID))
 }
 g+xlim(input$yrs)
  })

  output$TableOData<-DT::renderDataTable({
    var <- input$stat2plot
    Years2Plot <- input$yrs
    ModTeamsSub <- ModTeams[, c("yearID", var), drop = FALSE]
    ModTeamsSub <- ModTeams%>% filter(yearID>=Years2Plot[1] & yearID<=Years2Plot[2])
    tab <- aggregate(ModTeamsSub[[var]] ~ yearID, data = ModTeamsSub, FUN = mean)
    tab[, 2] <- round(tab[, 2], 2)
    names(tab)[2] <- paste0("Average ")
    names(tab)[1] <- paste0("yearID ")
    tab

})
  
  
  
})
