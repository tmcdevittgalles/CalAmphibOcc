### The goal of this script is to build a simulated dataset consistenting of 
### presence/absence and eventually abundance of a taxa across numberous sites
### Each site would be visited multiple times within a season and there will be
### a varity of number of seasons. Part of goal of this code is to see how 
### differnt number of visits, seasons, and sites influence the ability of the 
### model to extract the given known predictors The dataset will initially be
### simple and will gradually get more complex as we add different predictors. 
### In this code we will assign specific coeficients to each new predictor and 
### see how well the model can estimate them. This step is super cruical in our
### understand of the strenghts of the 'unmarked' package and how to utilize it 
### going forward in our CPW SGA grant

## Ok lets first start with some packages
library(tidyverse) # for data manipulations

library(unmarked) # for the models

## Setting up the initial parameters

nSites <- 75 ## number of sites

nVisits <- 2 ## number of visits

nYears <- 10 ## number of seasons

psi <- rep(NA, nYears) ## Occupancy probability for each year

muZ <- Z <- array( dim =c(nSites, nYears)) ## Expected and realized occupancy

y <- array( NA, dim=c(nSites, nVisits, nYears))

set.seed(29380)

psi[1] <- muZ[,1] <- .25 ## setting up initial occupancy probability

pConst <- rep(.8, 10) ## constant probability of detection``

pVar <-  seq(0.1,1 , length.out = 10) ## seasonal probability of detection

phi <- runif(n=nYears-1, min =0.5, max =1 ) # survival probability

gamma <- runif( n = nYears -1 , min = 0.1, max=0.25) # colonization probability

## Generate the true state of each site x year for occurance

## First Year , Initial conditions

Z[,1] <- rbinom(nSites, 1, psi[1] ) ## bernoulli trial for each site with the 
                                    ## probability of occury == 0.25

## following seasons

for( i in 1:nSites){  ## Looping over each sites
    for( k in 2:nYears){ ## Looping over each season, note starting on 2 since
                        ## we already initialized the first season above
        muZ[i,k] <- Z[ i, k-1] * phi[k-1] + ( 1 - Z[i , k-1]) * gamma[k-1]
        Z[i, k ] <- rbinom( 1, 1, muZ[i,k])
    }
}