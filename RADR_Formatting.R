### Manipulating Site Occupancy into unmarked format

library(tidyverse)
library(unmarked)

radr.occ <- read.csv("./Data/RADR_OCC.csv" )

radr.y <- select( radr.occ, c( "site", "site.year", "site.year.round", 
                               "RADR_ADULT_TOTAL"))

colnames(radr.y) <- c("SiteCode", "SiteYear", "SiteYearRound", "RADR")


radr.y <- separate(radr.y, SiteYearRound, sep=-6 , c("ThrowAway", "YearRound"))

radr.y1 <- select( radr.y, c("SiteCode", "YearRound", "RADR")) 


radr.y2 <- reshape(radr.y1, idvar = "SiteCode", 
                   timevar = "YearRound", direction = "wide")
