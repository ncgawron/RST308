################################################;
#Author:    Nicholas Gawron                                                                              ;
#Collaborators:  Jamie Loring                                                         ;
#Program Purpose: HW 1 ;
#Date:  1/29/21                                                                                   ;
################################################;

rm(list = ls())
rm 
cat("\014") 
#clears consle and environment 


#Question 1 
expValues <- rexp(30,rate =2)

#Question 2
hist(expValues,main = paste("Histogram of Random Exponential Values"))

#Question 3
numSeq <- seq(from=10,to=100,by=0.5)

#Question 4
randomValues = sample(numSeq, 30, replace = TRUE)

#Question5 
numSeq[c(1,10,15,30)]

#Question 6
lenSeq <- length(numSeq)
lenSeq #Prints
myNames = list(NULL, c("Exp", "Random")) #created names 

#Question 10


#Question 7 
numSeq[-c(1,2,seq(from=150,to=lenSeq))]

#Question 8 
numMat = matrix(c(expValues,randomValues),ncol = 2)
numMat

#Question 9
numMat <- matrix(c(expValues,randomValues),ncol = 2,dimnames = myNames)

#Question 11
lettterVec <- sample(letters, 30, replace = TRUE)

binded <- cbind(numMat,sample(letters, 30, replace = TRUE)) #binded the vector and matrix
binded  #prints THIS was stored as a matrix to make question 12 easier. 


#Question 12
typeof(binded[1,2]) 
# All of the values in the matrix that were numeric data became characters. this is checked with typeof command

#Question 13
myDF <- data.frame(Exponential = expValues,Random =randomValues,myLetters = lettterVec)    


#Question 14
 myDF$Random  #one way 
 myDF[,2]     #second way 
 myDF[[2]]    #third way
#these three ways are all different and return the same col. of values
 
#Question 15
myDF[10:20,] #since it is b4 the comma we only fetch rows
 #prints 

#Question 16

names(myDF)
#outputs names of coloums in the data frame 

#Question 17

names(myDF)[2]<-"myValues"  # gets the second name from the name vector & replaces it
myDF

#Question 18
myDF[,3] <- NULL # eliminates 3rd col. and puts it on myDF
myDF #prints to console


#Question 19

fit <- lm(cars$speed ~ cars$dist)

typeof(fit)
str(fit)


length(fit)


# Based off the typeof, the fit model is a list object
#also according to str, fit is a list of length 12
# str(fit)  >>>>>  List of 12
# The length of this list is 12 

#Question 20

fit[[1]] #returns coeff. 
fit$coefficients #also does the same thing 

#this returns the "double" valued atomic vector inside the first element of the list 
#called fit. This vector has two values stored in it
# This is determined to be a set of double (not characters or a list) by the 
#typeof command below
typeof(fit[[1]])
str(fit[[1]])
