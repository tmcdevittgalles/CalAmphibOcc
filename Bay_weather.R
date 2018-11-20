### This code maniuates the daily climate data to get it into a format to began 
### building models explore the impact of climate on amphibian occurances


## Creation date : 11/06/2018

library(tidyverse)

setwd("~/Desktop/Current_Projects/Amphibian_Occ/Amphib_Occ_Prog")

## ok lets try to add some weather patterns

weather.df <- read.csv("./Data/Bay_Weather_07_18.csv")


weatherSJ.df <- filter(weather.df, NAME == "SAN JOSE") ## Isolate San Jose

## First step is to reshape the date to account for water year

weatherSJ.df$Date <-  as.Date( weatherSJ.df$DATE ,"%m/%d/%y") ## Extracting date

## Temporary variable to extract julian date

at1 <- as.POSIXlt( weatherSJ.df$Date, format= "%Y-%M-%D") 

## Adding julian date 

weatherSJ.df$Julian <- at1$yday 

## Somehow still missing Year

weatherSJ.df$Year <- lubridate::year(weatherSJ.df$Date)

## Need to recreate new Julian date for the start of the water year

weatherSJ.df$WaterDate <- rep(NA, nrow(weatherSJ.df))

for( i in 1:nrow(weatherSJ.df)) {
    if( weatherSJ.df$Year[i] == 2012 | weatherSJ.df$Year[i] == 2016 | 
        weatherSJ.df$Year[i] == 2008 ){ ## These dates are for the leap years
        if( weatherSJ.df$Julian[i] < 275 ){
            weatherSJ.df$WaterDate[i] <- weatherSJ.df$Julian[i] + 92
        }else{
            weatherSJ.df$WaterDate[i] <- weatherSJ.df$Julian[i] - 274
        }
    }else{ ## Normal non-leap year
        if( weatherSJ.df$Julian[i] < 274 ){
            weatherSJ.df$WaterDate[i] <- weatherSJ.df$Julian[i] + 92
        }else{
            weatherSJ.df$WaterDate[i] <- weatherSJ.df$Julian[i] - 273
        } 
    }
}


## Creating new variable for the specific water year

weatherSJ.df$WaterYear <- rep(NA, nrow(weatherSJ.df))

for( i in 1:nrow(weatherSJ.df)) {
    if( weatherSJ.df$Year[i] == 2012 | weatherSJ.df$Year[i] == 2016| 
        weatherSJ.df$Year[i] == 2008   ){ ## Leap year
        if( weatherSJ.df$Julian[i] < 275 ){
            weatherSJ.df$WaterYear[i] <- weatherSJ.df$Year[i]
        }else{
            weatherSJ.df$WaterYear[i] <- weatherSJ.df$Year[i] +1
        }
    }else{ ## Non-Leap year
        if( weatherSJ.df$Julian[i] < 274 ){ 
            weatherSJ.df$WaterYear[i] <- weatherSJ.df$Year[i]
        }else{
            weatherSJ.df$WaterYear[i] <- weatherSJ.df$Year[i] +1
        }
    }
}


###  Creating a new variable for cummulative sum of precip and relative precip

weatherSJ.df <- weatherSJ.df %>%
    group_by( WaterYear) %>% ## summing across water years 
    mutate(
        total.prec = cumsum(PRCP) ,
        rel.prec = total.prec/max(total.prec)
    )


### New variable for season , kind of ignoring leap year differences figure it
### is close enough

weatherSJ.df$Season <- rep(NA, nrow(weatherSJ.df))

for( i in 1:nrow(weatherSJ.df)) {
    if( weatherSJ.df$Julian[i] < 80 | weatherSJ.df$Julian[i] >= 355 ) {
        weatherSJ.df$Season[i] <- "Winter"
    }
    if( weatherSJ.df$Julian[i] >= 80 & weatherSJ.df$Julian[i] < 172 ) {
        weatherSJ.df$Season[i] <- "Spring"
    }
    if( weatherSJ.df$Julian[i] >= 172 & weatherSJ.df$Julian[i] < 264  ) {
        weatherSJ.df$Season[i] <- "Summer"
    }
    if( weatherSJ.df$Julian[i] >= 264 & weatherSJ.df$Julian[i] < 355  ) {
        weatherSJ.df$Season[i] <- "Fall"
    }
}

### Adding new variable for breeding season 
### Early breeding season is from October to March
### Late Breeding is from March to June
### None Breeding is from June till the end of September

weatherSJ.df$BreedS <- rep(NA, nrow(weatherSJ.df))

for( i in 1:nrow(weatherSJ.df)) {
    if( weatherSJ.df$Julian[i] < 60 | weatherSJ.df$Julian[i] >= 274 ) {
        weatherSJ.df$BreedS[i] <- "Early"
    }
    if( weatherSJ.df$Julian[i] >= 60 & weatherSJ.df$Julian[i] < 152 ) {
        weatherSJ.df$BreedS[i] <- "Late"
    }
    if( weatherSJ.df$Julian[i] >= 152 & weatherSJ.df$Julian[i] < 274  ) {
        weatherSJ.df$BreedS[i] <- "Non-Breeding"
    }
}

weatherSpan.df <- filter( weatherSJ.df, WaterYear > 2008)

### Plotting relative precipation from the start of the water year to may 15

ggplot( weatherSpan.df, aes( x= WaterDate, y= rel.prec, 
                             color= as.factor(WaterYear)))+
    geom_line(size=1) + theme_classic() + xlim(0,226)+ 
    theme( legend.position = "none")

### Plotting cummulative precipation from the start of the water year to may 15

ggplot( weatherSpan.df, aes( x = WaterDate, y= total.prec, 
                             color = as.factor(WaterYear)))+
    geom_line(size = 1) + theme_classic() + xlim(0,226)+ 
    theme( legend.position = "none")

## Plotting cummulative precip across the whole water year

ggplot(weatherSpan.df, aes( x = WaterDate, y= total.prec, 
                            color = as.factor(WaterYear)))+
    geom_line(size=1) + theme_classic() + 
    theme( legend.position = "none" )

## Plotting relative precip across the whole water year

ggplot( weatherSpan.df, aes( x= WaterDate, y= rel.prec, 
                             color= as.factor(WaterYear)))+
    geom_line(size=1) + theme_classic() +
    theme( legend.position = "none")


###### Calculating annual summary statistics ######

## Precip by year

precip.df <- weatherSpan.df %>%
    group_by( WaterYear) %>%
    summarise(
        Precip = sum(PRCP)
    )

## Mean temp across year and season

temp.df <- aggregate(TMAX ~ WaterYear * Season , data= weatherSJ.df, FUN=mean)

## Exploring general patterns of mean max temperature across season

ggplot( temp.df, aes( y= TMAX, x=Season, fill=Season)) +geom_violin() + 
    geom_jitter(width=0.1) + theme_classic()

## Replacing season with breading season

breed.df <- aggregate(PRCP ~ WaterYear * BreedS , data= weatherSJ.df, FUN=sum)

ggplot( breed.df, aes( y= PRCP, x=BreedS, fill= BreedS )) +geom_violin() + 
    geom_jitter(width=0.1) + theme_classic()

bSpread.df <- breed.df %>% spread(BreedS, PRCP )

### Isolating different season temperature data 
### Probably a better way to do this but this works

summer.df <- filter( temp.df, Season == "Summer" )

colnames(summer.df) <- c("WaterYear", "Season", "SummerTemp")

summer.df <- select( summer.df, c("WaterYear", "SummerTemp"))

spring.df <- filter( temp.df, Season == "Spring" )

colnames(spring.df) <- c("WaterYear", "Season", "SpringTemp")

spring.df <- select( spring.df, c("WaterYear", "SpringTemp"))

### Isolating different season Precip data 

sPrec.df <- aggregate(PRCP ~ WaterYear * Season , data= weatherSJ.df, FUN=sum)

## Fall precip

fallPre.df <-  filter( sPrec.df, Season == "Fall" )

colnames(fallPre.df) <- c("WaterYear", "Season", "FallPrec")

fallPre.df<- select( fallPre.df, c("WaterYear", "FallPrec"))

## winter precip

winterPre.df <-  filter( sPrec.df, Season == "Winter" )

colnames(winterPre.df) <- c("WaterYear", "Season", "WinterPrec")

winterPre.df <- select( winterPre.df, c("WaterYear", "WinterPrec"))

## spring precip

springPre.df <-  filter( sPrec.df, Season == "Spring" )

colnames(springPre.df) <- c("WaterYear", "Season", "SpringPrec")

springPre.df<- select( springPre.df, c("WaterYear", "SpringPrec"))


## Combinding for full data set

weather.df <- full_join( precip.df ,bSpread.df,
                           by ="WaterYear")

weather.df  <- full_join( weather.df ,
                           summer.df , by = "WaterYear")

weather.df <- full_join(weather.df ,
                           spring.df , by = "WaterYear" )

weather.df  <- full_join(weather.df  , 
                           fallPre.df , by = "WaterYear" )

weather.df <- full_join( weather.df ,
                           winterPre.df, by = "WaterYear" )

weather.df  <- full_join(weather.df  ,
                           springPre.df, by = "WaterYear" )

## Writing a new csv file from the compiled climate data

#write.csv( weather.df, file = "bay_weather.csv")