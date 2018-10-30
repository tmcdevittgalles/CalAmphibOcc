### Manipulating Site Occupancy into unmarked format

library(tidyverse)
library(unmarked)

 ## uploading the data , We are using the file Dane sent titled 
 ## "dummy RADR OCC lab meeting 10_30.xlsx" I only resaved it as a csv file

radr.occ <- read.csv("./Data/RADR_OCC.csv" )

## Selecting the only rows we are currently interested in ( probably dont need
## "site.year" )

radr.y <- select( radr.occ, c( "site", "site.year", "site.year.round", 
                               "RADR_ADULT_TOTAL"))

## Fuck these awful column names! Lets change them
## CamelCaseForever!!!!!

colnames(radr.y) <- c("SiteCode", "SiteYear", "SiteYearRound", "RADR")

## Need to remove "Site" from "SiteYearRound"

radr.y <- separate(radr.y, SiteYearRound, sep=-6 , c("ThrowAway", "YearRound"))

## Now lets get rid of the redundant new SiteCode column called "ThrowAway"
radr.y1 <- select( radr.y, c("SiteCode", "YearRound", "RADR")) 

## Cool lets reshape it to get it in the proper format
radr.y2 <- reshape(radr.y1, idvar = "SiteCode", 
                   timevar = "YearRound", direction = "wide")



