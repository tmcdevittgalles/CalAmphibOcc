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

### Ok lets start with a vary simple model with static parameters

## Setting up the initial parameters

nSites <- 75 ## number of sites

nVisits <- 2 ## number of visits

nYears <- 10 ## number of seasons

psi <- rep(NA, nYears) ## Occupancy probability for each year

muZ <- Z <- obsZ <- array( dim =c(nSites, nYears)) ## Expected and realized 
## occupancy

y <- array( NA, dim=c(nSites, nVisits * nYears))

set.seed(29381) # i guess this maintains the randomness so you can fluctuate
# the paramets while keeping the observation consistent

years <- 2009:2018 ## the years of our data

psi[1] <- muZ[,1] <- .25 ## setting up initial occupancy probability

pConst <- .5  ## constant probability of detection``

pVar <-  rnorm(10, .5,.1) ## seasonal probability of detection

phi <- .75 # survival probability

gamma <- .15 # colonization probability

## Generate the true state of each site x year for occurance

## First Year , Initial conditions

Z[,1] <- rbinom(nSites, 1, psi[1] ) ## bernoulli trial for each site with the 
## probability of occury == 0.25

## following seasons

for( i in 1:nSites){  ## Looping over each sites
    for( k in 2:nYears){ ## Looping over each season, note starting on 2 since
        ## we already initialized the first season above
        muZ[i,k] <- Z[ i, k-1] * phi + ( 1 - Z[i , k-1]) * gamma
        Z[i, k ] <- rbinom( 1, 1, muZ[i,k])
    }
}

## now lets construct the observed data with including the probability of 
## observing the said taxa

for( i in 1:nSites){ ## looping through the sites
    for( k in 1:nYears){ ## looping through the years
        prob <- Z[i, k] * pVar[k] ## probability of detecting the species
        v <- k * nVisits 
        y[i, v - 1 ] <- rbinom( 1, 1 , prob) ## Site i X year k X visit 1
        y[i, v ] <- rbinom(1,1, prob) ## Site i X year k X visit 2
        # determine status of each sites per season
        if( y[i,v] + y[i,v-1] > 0 ){ ## if greater than 0 we observed taxa
            obsZ[i,k] <- 1
        } else{ ## if == to 0 then we did not observe it
            obsZ[i,k] <- 0
        }
        
    } ## Closes year loop
} ## Closes site loop

## compute annual population occupancy

for( k in 2:nYears) {
    psi[k] <- psi[k-1] * phi + (1-psi[k-1])*gamma
}

## calculating observed values


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    theme_classic() + ylim(0,.5) + xlab("Years") + ylab("Site Occupancy") 


### Cool we have our observed data lets now see if we can extract the given
### parameters using unmarked

## combindind all data

obsy <- matrix( y, nSites, nYears*nVisits) ## observation matrix

years <- matrix( as.factor(years), nSites , nYears, byrow=T) ## year matrix

## making the multifram object

simUMF <- unmarkedMultFrame(
    y= obsy,
    yearlySiteCovs = list( years = years),
    numPrimary = nYears
)

summary(simUMF) ## sweet it looks like it works

## ok lets look at what the model produces and will it retain the parameters
## we assigned to it

m0 <- colext( psiformula = ~ 1, gammaformula = ~ 1, epsilonformula = ~ 1,
              pformula = ~ 1, data= simUMF)

summary(m0) 

## Extracting the values of each parameter
estPsi <- m0@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m0@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m0@estimates@estimates$ext@estimates ## Extinction
estP <- m0@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.27, Actual = 0.26

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.14, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## Estimated : 0.73 ,Actual = 0.75

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.56 , Actual = 0.51


## extract projecting occupancy

expOcc  <- m0@projected.mean[2,]


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,.75) + xlab("Years") + ylab("Site Occupancy") 


### Ok let allow survival to decrease each year, so that the population should 
### grow than shrink

## Setting up the initial parameters

nSites <- 75 ## number of sites

nVisits <- 2 ## number of visits

nYears <- 10 ## number of seasons

psi <- rep(NA, nYears) ## Occupancy probability for each year

muZ <- Z <- obsZ <- array( dim =c(nSites, nYears)) ## Expected and realized 
                                                   ## occupancy

year <- 2009:2018

y <- array( NA, dim=c(nSites, nVisits * nYears))

set.seed(29385) # i guess this maintains the randoness so you can fluctuate
                # the paramets while keeping the observation consistent

psi[1] <- muZ[,1] <- .25 ## setting up initial occupancy probability

pConst <- .5  ## constant probability of detection``

pVar <-  rnorm(10, .6,.1) ## seasonal probability of detection

phi <- seq(from =1, to= 0.25,length.out = 10 ) # survival probability decrease 
                                              # each year

gamma <- .15 # colonization probability

## Generate the true state of each site x year for occurance

## First Year , Initial conditions

Z[,1] <- rbinom(nSites, 1, psi[1] ) ## bernoulli trial for each site with the 
                                    ## probability of occury == 0.25

## following seasons

for( i in 1:nSites){  ## Looping over each sites
    for( k in 2:nYears){ ## Looping over each season, note starting on 2 since
                        ## we already initialized the first season above
        muZ[i,k] <- Z[ i, k-1] * phi[k-1] + ( 1 - Z[i , k-1]) * gamma
        Z[i, k ] <- rbinom( 1, 1, muZ[i,k])
    }
}

## now lets construct the observed data with including the probability of 
## observing the said taxa

for( i in 1:nSites){ ## looping through the sites
    for( k in 1:nYears){ ## looping through the years
        prob <- Z[i, k] * pVar[k] ## probability of detecting the species
            v <- k * nVisits 
            y[i, v - 1 ] <- rbinom( 1, 1 , prob) ## Site i X year k X visit 1
            y[i, v ] <- rbinom(1,1, prob) ## Site i X year k X visit 2
            # determine status of each sites per season
            if( y[i,v] + y[i,v-1] > 0 ){ ## if greater than 0 we observed taxa
               obsZ[i,k] <- 1
            } else{ ## if == to 0 then we did not observe it
                obsZ[i,k] <- 0
            }
                
    } ## Closes year loop
} ## Closes site loop

## compute annual population occupancy

for( k in 2:nYears) {
    psi[k] <- psi[k-1] * phi[k-1] + (1-psi[k-1])*gamma
}

## calculating observed values


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 
    



obsy <- matrix( y, nSites, nYears*nVisits) ## observation matrix

years <- matrix((year), nSites , nYears, byrow=T) ## year matrix

years.2 <- matrix((year)^2, nSites , nYears, byrow=T) ## year matrix

## making the multifram object

simUMF <- unmarkedMultFrame(
    y= obsy,
    yearlySiteCovs = list( years = years, years.2 = years.2),
    numPrimary = nYears
)

summary(simUMF) ## sweet it looks like it works

## ok lets look at what the model produces and will it retain the parameters
## we assigned to it

m0 <- colext( psiformula = ~1, gammaformula = ~1  , epsilonformula = ~ 1 ,
              pformula = ~1, data= simUMF)

summary(m0) 

## Extracting the values of each parameter
estPsi <- m0@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m0@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m0@estimates@estimates$ext@estimates ## Extinction
estP <- m0@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.211, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.141, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## Estimated : 0.692 ,Actual = 0.62

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.524 , Actual = 0.537


## extract projecting occupancy

expOcc  <- m0@projected.mean[2,]


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 


## We will now be modeling the years

m1 <- colext( psiformula = ~1, gammaformula = ~ 1 ,
              epsilonformula = ~ as.factor(years) ,
              pformula = ~1 , data= simUMF)

summary(m1) 

## Extracting the values of each parameter
estPsi <- m1@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m1@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m1@estimates@estimates$ext@estimates ## Extinction
estP <- m1@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.206, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.144, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## hmm not sure about this

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.523 , Actual = 0.537


## extract projecting occupancy

expOcc  <- m1@projected.mean[2,]


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 


### Ok let allow survival and extinction to vary across each year, so that the 
### population should fluctuate 

## Setting up the initial parameters

nSites <- 75 ## number of sites

nVisits <- 2 ## number of visits

nYears <- 10 ## number of seasons

psi <- rep(NA, nYears) ## Occupancy probability for each year

muZ <- Z <- obsZ <- array( dim =c(nSites, nYears)) ## Expected and realized 
## occupancy

year <- 2009:2018

y <- array( NA, dim=c(nSites, nVisits * nYears))

set.seed(29385) # i guess this maintains the randoness so you can fluctuate
# the paramets while keeping the observation consistent

psi[1] <- muZ[,1] <- .25 ## setting up initial occupancy probability

pConst <- .5  ## constant probability of detection``

pVar <-  rnorm(10, .6,.1) ## seasonal probability of detection

phi <- seq(from =1, to= 0.25,length.out = 10 ) # survival probability decrease 
# each year

gamma <- seq(from =.5, to= 0.8,length.out = 10 ) # colonization probability

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

## now lets construct the observed data with including the probability of 
## observing the said taxa

for( i in 1:nSites){ ## looping through the sites
    for( k in 1:nYears){ ## looping through the years
        prob <- Z[i, k] * pConst ## probability of detecting the species
        v <- k * nVisits 
        y[i, v - 1 ] <- rbinom( 1, 1 , prob) ## Site i X year k X visit 1
        y[i, v ] <- rbinom(1,1, prob) ## Site i X year k X visit 2
        # determine status of each sites per season
        if( y[i,v] + y[i,v-1] > 0 ){ ## if greater than 0 we observed taxa
            obsZ[i,k] <- 1
        } else{ ## if == to 0 then we did not observe it
            obsZ[i,k] <- 0
        }
        
    } ## Closes year loop
} ## Closes site loop

## compute annual population occupancy

for( k in 2:nYears) {
    psi[k] <- psi[k-1] * phi[k-1] + (1-psi[k-1])*gamma[k-1]
}

## calculating observed values


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 




obsy <- matrix( y, nSites, nYears*nVisits) ## observation matrix

years <- matrix((year), nSites , nYears, byrow=T) ## year matrix

years.2 <- matrix((year)^2, nSites , nYears, byrow=T) ## year matrix

## making the multifram object

simUMF <- unmarkedMultFrame(
    y= obsy,
    yearlySiteCovs = list( years = years, years.2 = years.2),
    numPrimary = nYears
)

summary(simUMF) ## sweet it looks like it works

## ok lets look at what the model produces and will it retain the parameters
## we assigned to it

m0 <- colext( psiformula = ~1, gammaformula = ~1  , epsilonformula = ~ 1 ,
              pformula = ~1, data= simUMF)

summary(m0) 

## Extracting the values of each parameter
estPsi <- m0@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m0@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m0@estimates@estimates$ext@estimates ## Extinction
estP <- m0@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.211, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.141, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## Estimated : 0.692 ,Actual = 0.62

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.524 , Actual = 0.537


## extract projecting occupancy

expOcc  <- m0@projected.mean[2,]


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 


## We will now be modeling the years 
m1 <- colext( psiformula = ~1, gammaformula = ~ 1 ,
              epsilonformula = ~ as.factor(years) ,
              pformula = ~1 , data= simUMF)

summary(m1)  ## 1762.66

## Extracting the values of each parameter
estPsi <- m1@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m1@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m1@estimates@estimates$ext@estimates ## Extinction
estP <- m1@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.17, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.144, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## hmm not sure about this

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.523 , Actual = 0.537

## extract projecting occupancy

expOcc  <- m1@projected.mean[2,]


ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 

### adding both variables

m2 <- colext( psiformula = ~1, gammaformula = ~ as.factor(years) ,
              epsilonformula = ~ as.factor(years) ,
              pformula = ~1 , data= simUMF)

summary(m2) ## AIC 1756.56 ∆AIC : - 6.1

## Extracting the values of each parameter
estPsi <- m2@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m2@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m2@estimates@estimates$ext@estimates ## Extinction
estP <- m2@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.17, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.144, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## hmm not sure about this

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.523 , Actual = 0.537

## extract projecting occupancy

expOcc  <- m2@projected.mean[2,]

ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 

#### adding both variables

m3 <- colext( psiformula = ~1, gammaformula = ~ as.factor(years) ,
              epsilonformula = ~ as.factor(years) ,
              pformula = ~ as.factor(years) , data= simUMF)

summary(m3) ## AIC 1756.56 ∆AIC : - 6.1

## Extracting the values of each parameter
estPsi <- m3@estimates@estimates$psi@estimates ## overall occupancy
estGamm <- m3@estimates@estimates$col@estimates ## Coloniztion
estSurv <- m3@estimates@estimates$ext@estimates ## Extinction
estP <- m3@estimates@estimates$det@estimates ## detection

## Ok lets now compare the model estimation with our assigned values

### Mean occupancy across all years

boot::inv.logit( estPsi) ## Estimated : 0.17, Actual = 0.237

## Probabilty of colonization

boot::inv.logit( estGamm) ## Estimated : 0.144, Actual = 0.15

## Probabilty of survival

1 - boot::inv.logit( estSurv) ## hmm not sure about this

## Probability of detection

boot::inv.logit( estP) ## Estimated : 0.523 , Actual = 0.537

## extract projecting occupancy

expOcc  <- m3@projected.mean[2,]

ggplot() + geom_point( aes( x= 2009:2018, y= colMeans(Z) ),size=3.5 ) +
    geom_line(aes( x= 2009:2018, y= colMeans(Z) ),size=1 ) +
    geom_point( aes( x= 2009:2018, y= colMeans(obsZ) ),size=3.5, color="blue") +
    geom_line(aes( x= 2009:2018, y= colMeans(obsZ) ),size=1, color="blue" ) +
    geom_point( aes( x= 2009:2018, y= expOcc ),size=3.5, color="red") +
    geom_line(aes( x= 2009:2018, y= expOcc ),size=1, color="red" ) +
    theme_classic() + ylim(0,1) + xlab("Years") + ylab("Site Occupancy") 

## Model summary 

summary(m0) ## Intercept only model : AIC = 1780.76
summary(m1) ## Sites vary for extinction : AIC = 1762.66
summary(m2) ## Sites vary for extinction AND Colinization : AIC = 1756.56
summary(m3) ## Sites vary for extinction, colinization , AND P : AIC = 1749.80
