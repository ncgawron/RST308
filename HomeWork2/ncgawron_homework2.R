################################################;
#Author:    Nicholas Gawron                                                                              ;
#Collaborators:  Jamie Loring                                                         ;
#Program Purpose: HW 2 ;
#Date:  2/9/21                                                                                   ;
################################################;

rm(list = ls())
rm 
cat("\014") 
#clears consle and environment 

#reads in librarys needed 
library(tidyverse)
library(readxl)

#question1 / impport car data # question 2 as well with printing 
cars <- read_excel("data/CarDepreciation.xlsx")
View(cars)
#prints Dep. coloum to the console in the form of a tibble
cars[,4]
#prints dep. coloumn to the coonsle in the form of an atomic array vector
cars$Depreciation


#question 3 
education <- read_excel("data/censusEd.xlsx", sheet = "EDU01D")


#question 4 Prints to console first three rows
education[1:3,]

#question 5 
#reads in readr package again to be safe 
library(readr)
scores<- readr::read_csv("data/scoresFull.csv")

#question 6 
air <-readr::read_delim("https://www4.stat.ncsu.edu/~online/datasets/AirQualityUCI.csv",delim = ";")

#Question7
crabs <- readr::read_table2("../HomeWork2/data/crabs.txt")
#makes the empty column called X7 null
crabs$X7 = NULL



#API QUESTIONS
#
########################################################################
########################################################################
########################################################################
########################################################################
#packages already installed from console
library("httr")
library("jsonlite")

#Qu 2 / endpoints we can access 
# Some common end points (or tables) that we could use are Berry's, Berry Firmness, and Encounter Conditions. 

#question 3 on API ...creates a URL to fetch info in Pokemon data 

base_url <- "https://pokeapi.co/api/v2"
tab_name <- "pokemon"
poke_name <- "pikachu"

full_url <- paste0(base_url, "/",tab_name,"/", poke_name)

#question 4 

RawPoke  <- GET(full_url) # retrieve the pikachu information in raw form
PokeInfo<-content(RawPoke) #to turn it into JSON text form


#returns a list of 17 
PokeInfoMod <- jsonlite::fromJSON(full_url, flatten = TRUE)

#question 5 on API 

#this prints the all move names in an atomic vector to the console
(PokeInfoMod$moves)$move.name

#according to this database  pikachu can learn 81 moves.