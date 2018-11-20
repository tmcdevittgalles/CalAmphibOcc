# Exploring multi-season occupancy models
library(unmarked)
library(tidyverse)

# fix the temp issue
load("Radr.UMF.Rdata")
str(Radr.UMF)


### Wynne's data clean up
radr.occ <- read.csv("./Data/RADR_OCC.csv" )
obs.df <- read.csv("./Data/Obser_level.csv")
site.df <- read.csv("./Data/Site_level.csv" )
colnames(site.df) <- c("SiteCode","Elev", "Forest" )

### Clean up the observation level data--------
## Selecting the only rows we are currently interested in ( probably dont need
## "site.year" )
radr.y <- select( radr.occ, c( "site", "site.year", "site.year.round", 
                               "RADR_ADULT_TOTAL"))
radr.y <- filter(radr.y, site %in% site.df$SiteCode)
colnames(radr.y) <- c("SiteCode", "SiteYear", "SiteYearRound", "RADR")
## Need to remove "Site" from "SiteYearRound"
radr.y <- separate(radr.y, SiteYearRound, sep=-6 , c("ThrowAway", "YearRound"))
## Now lets get rid of the redundant new SiteCode column called "ThrowAway"
radr.y1 <- select( radr.y, c("SiteCode", "YearRound", "RADR")) 
## Cool lets reshape it to get it in the proper format
radr.y1 <- radr.y1 %>% reshape(idvar = "SiteCode", timevar = "YearRound", direction = "wide")
radr.y1 <- select(radr.y1, c("SiteCode", sort(colnames(radr.y1)[2:33])))
colnames(radr.y1) <- gsub(x=colnames(radr.y1), pattern = "RADR.", replacement = "")
colnames(radr.y1)
colSums(radr.y1[,2:33], na.rm=T)
## Filtering for the proper site codes
radr.y1 <- filter(radr.y1, SiteCode %in% site.df$SiteCode)
write.csv(radr.y1, file = "Data/RADR_OCC.csv", row.names = FALSE)


## Fix the observation level data -----------
obs.df <- read.csv("./Data/Obser_level.csv")
obs.df$AssmtCode <- obs.df$site.year.round 
obs.df1 <- separate(obs.df, site.year.round, sep=-7 , c("SiteCode", "YearRound"))
obs.df1 <- separate(obs.df1, YearRound, sep=-1 , c("Year", "Round"))
obs.df1 <- separate(obs.df1, Year, sep=-1 , c("Year", "throw"))
obs.df1 <- separate(obs.df1, Year, sep=1 , c("Throw", "Year"))

## filtering to maintain uniform site codes
obs.df1<- filter(obs.df1, SiteCode %in% site.df$SiteCode)
obser.df <- select( obs.df1,c("SiteCode", "Year", "Round","PondArea..m.",
                              "Perimeter..m.", "Temp..","DRY" ) )

## renaming varables
colnames(obser.df)[4:7] <- c( "Area", "Perimeter", "Temp", "Dry") 

# Whoops forgor I need "Year" and "Round" as a single varible
obser.df <- unite( obser.df, "YearRound", c("Year", "Round"), sep= "_" )

## According to unmarked we need to have each observation variable in its own
## data frame so lets do that

##creating a df for observation level Area
obArea.df <- obser.df[,c(1,2,3)]
obArea.df <- reshape(obArea.df, idvar = "SiteCode", 
                     timevar = "YearRound", direction = "wide")
colnames(obArea.df)
## lets rearrange the columns so they are in chronological order
obArea.df <- obArea.df %>% select("SiteCode", sort(colnames(obArea.df)[2:33]))
colnames(obArea.df) <-gsub(x=colnames(obArea.df), pattern = "Area.", replacement = "")

### creating a df for observation level Perimeter
obPerim.df <- obser.df[,c(1,2,4)]
obPerim.df <- reshape(obPerim.df, idvar = "SiteCode", 
                      timevar = "YearRound", direction = "wide")
obPerim.df <- obPerim.df %>% select("SiteCode", sort(colnames(obPerim.df)[2:33]))
colnames(obPerim.df) <-gsub(x=colnames(obPerim.df), pattern = "Perimeter.", replacement = "")

### creating a df for observation level for Temp.
obTemp.df <- obser.df[,c(1,2,5)]
obTemp.df  <- reshape(obTemp.df, idvar = "SiteCode", 
                      timevar = "YearRound", direction = "wide")
obTemp.df <- obTemp.df %>% select("SiteCode", sort(colnames(obTemp.df)[2:33]))
colnames(obTemp.df) <-gsub(x=colnames(obTemp.df), pattern = "Temp.", replacement = "")
# replace default values of 0 with NA
obTemp.df[obTemp.df==0]<- NA

### creating a df for observation level dry
obDry.df <- obser.df[,c(1,2,6)]
obDry.df  <- reshape(obDry.df, idvar = "SiteCode", 
                     timevar = "YearRound", direction = "wide")
obDry.df <- obDry.df %>% select("SiteCode", sort(colnames(obDry.df)[2:33]))
colnames(obDry.df) <-gsub(x=colnames(obDry.df), pattern = "Dry.", replacement = "")

## Ok lets now compile them into a single list because why not
radr.obs <- list( SiteCode= obTemp.df[,1], Area = obArea.df[,2:33],
                  Perim = obPerim.df[,2:33], 
                  Temp = obTemp.df[,2:33], Dry= obDry.df[,2:33])
dim(radr.obs)

# save observation level data as a .Rdata file
save(radr.obs, file = "Data/radr.obs.Rdata")


## Checking to make sure the diminsions are all good
dim(radr.y2) ## 387 x 33

dim(site.df) ## 387 x 3

dim(obArea.df) ## 387 x 33

dim(obPerim.df) ## 387 x 33

dim(obTemp.df) ## 387 x 33

dim(obDry.df) ## 387 x 33


# Cool all the data is filtered and have the same site code now we can begin to
# model

obDry.df$SiteCode[63]


Radr.UMF <- unmarkedMultFrame( y= radr.y2[,10:33], siteCovs = site.df[,2:3],
                               obsCovs = list(  Area = obArea.df[,10:33],
                                                Perim = obPerim.df[,10:33], 
                                                Temp = obTemp.df[,10:33],
                                                Dry= obDry.df[,10:33]),
                               numPrimary = 12 )

summary(Radr.UMF)
save(Radr.UMF, file = "Data/Radr.UMF.Rdata")

sRadr.UMF <- unmarkedMultFrame( y= radr.y2[,10:33], siteCovs =site.df[,2:3],
                                obsCovs = list(Area = scale(obArea.df[,10:33]),
                                               Perim = scale(obPerim.df[,10:33]), 
                                               Temp = scale(obTemp.df[,10:33]),
                                               Dry= obDry.df[,10:33]),
                                numPrimary = 12 )

summary( sRadr.UMF )

model0 <- colext( psiformula = ~ 1 , 
                  gammaformula = ~ 1,
                  epsilonformula = ~ 1  ,
                  pformula = ~ 1 , data= Radr.UMF)

summary( model0 )


model1 <- colext( psiformula = ~ scale(Elev) * (Forest) , 
                  gammaformula = ~ scale(Elev) + (Forest) ,
                  epsilonformula = ~ scale(Elev) + Forest  ,
                  pformula = ~ Temp , data= sRadr.UMF)

summary( model1 )





model2 <- colext( psiformula = ~ (Elev) + Forest , 
                  gammaformula = ~ Elev + Forest ,
                  epsilonformula = ~ Elev + Forest  ,
                  pformula = ~Perim +Temp , data= sRadr.UMF)

summary( model2 )



