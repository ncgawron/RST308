StemCol<- 4026+9757+2085+4095;(StemCol)
NStem<- 861+1883+4681+1010+4352+1191; (NStem)
N <- StemCol+NStem; totPol
B <- 0.03
sig <- .25
#Proportional allocation we will Estimate \sigma_i \approx p_i(1-p_i)
# Allocation for each of the stratas 
a1 <- StemCol/N ; a1
a2 <- NStem/N ;a2
#Sample Size Denominator
Denom<- (N*B/2)^2 + .25*((StemCol) + (NStem)) ; Denom
n <- (.5*( (StemCol) + (NStem)))^2/Denom ; n

ntest <- ((StemCol+NStem)^2/(B^2*N^2+StemCol+NStem));ntest




for(i in 1:5){
letters[i]fff
}