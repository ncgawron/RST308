library(Lahman)
library(tidyverse)
library(ggplot2)
library(DT)
view(Teams)
#subsets rows
modTeams <- Teams %>% filter(lgID == c("AL","NL")) %>% filter(yearID >= 1901)

#creates new variables
modTeams <- modTeams %>% mutate(winPerc = W/G)  %>% mutate(RunPGame = R/G)  %>% mutate(HomePGame = HR/G)  %>% mutate(TotBGame = SB/G)  %>% mutate(StrOutPG = SO/G) %>% 
  mutate(SOpW = SO/BB)  %>% mutate(Whip = (3*(H+BB)/IPouts))

# Total bases per game (TB/G) variable unable to be created ... there is not Tb variables 
# used the SO variable which is strike outs per game


#gets rid of serval variables 
modTeams <- modTeams %>% select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name,park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))
view(modTeams)



tableDataFrame<-aggregate(modTeams$X2B~modTeams$yearID,data=modTeams,FUN=mean)
datatable(tableDataFrame, colnames = c('yearID', 'Avg'))

#if else plot

dotplot<-ggplot(ModTeams, aes(x = yearID, y= HR))
val <- 1
val2 <- 0
ifelse(val == 0, g<- dotplot+geom_point(aes(col=lgID)),g<- dotplot+geom_point())
g+geom_smooth(aes(colour=lgID))


if(val1 == 1){
g<- dotplot+geom_point(aes(col=lgID))
} else if {
  
}







view(Teams)


ModTeams <- Teams %>% filter(lgID == c("AL","NL")) %>% filter(yearID >= 1901)%>% 
  mutate(winPerc = W/G)%>% mutate(RunPGame = R/G)%>% mutate(HomePGame = HR/G)%>% 
  mutate(TotBGame = (H+X2B+2*X3B+3*HR)/G) %>% mutate(StrOutPG = SO/G)%>% 
  mutate(SOpW = SO/BB)%>% mutate(Whip = (3*(H+BB)/IPouts))%>% 
  select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name,park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))
view(ModTeams)


#creates new variables
ModTeams <- teams %>% mutate(winPerc = W/G)  %>% mutate(RunPGame = R/G)  %>% mutate(HomePGame = HR/G)  %>% mutate(TotBGame = SB/G)  %>% mutate(StrOutPG = SO/G) %>% 
  mutate(SOpW = SO/BB)  %>% mutate(Whip = (3*(H+BB)/IPouts))

# Total bases per game (TB/G) variable unable to be created ... there is not Tb variables 
# used the SO variable which is strike outs per game


#gets rid of serval variables 
ModTeams <- modTeams %>% select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name,park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))




ModTeams<-1



ModTeams <- Teams %>% filter(lgID == c("AL","NL")) %>% filter(yearID >= 1901)%>% 
  mutate(winPerc = W/G)%>% mutate(RunPGame = R/G)%>% mutate(HomePGame = HR/G)%>% 
  mutate(TotBGame = SB/G) %>% mutate(StrOutPG = SO/G)%>% 
  mutate(SOpW = SO/BB)%>% mutate(Whip = (3*(H+BB)/IPouts))%>% 
  select(!c(franchID,divID, Rank, G, Ghome, W, L, DivWin, WCWin, LgWin, WSWin, name,park, attendance, BPF, PPF, teamIDBR, teamIDlahman45, teamIDretro))