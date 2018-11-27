## Running multi-season occupancy models on RADR data

# previous steps: cleaned up data using TMG code (edited a little bit)
# See WEM clean up file for how this was done

### READ IN DATA ----------------
# detection data is in Data/RADR_OCC.csv 
obs.data <- read.csv("Data/RADR_OCC.csv", check.names=FALSE)
colnames(obs.data)
str(obs.data)
rownames(obs.data) = obs.data$SiteCode

# observation level covariate
load(file="Data/radr.obs.Rdata")
obs.cov <- radr.obs
radr.obs = NULL
str(obs.cov) # a list with each covariate as a dataframe
dim(obs.cov$Area)
dim(obs.cov$Perim)

# site level covariate
site.df <- read.csv("./Data/Site_level.csv" )
colnames(site.df) <- c("SiteCode","Elev", "Forest" )
rownames(site.df) <- site.df$SiteCode
# RADR.UMF <- unmarkedMultFrame( y = obs.data[, 2:33],
#                                siteCovs = site.df[, 2:3],
#                                obsCov = obs.cov[-1],
#                                numPrimary = 17) # have all the data from 1997 on (17 years)
# str(RADR.UMF)

obs.cov2 <- obs.cov[-1] # get rid of the first part giving the site names 

# add yearlySiteCov data
Yr <- as.character(2007:2018)
Yr <- matrix(Yr, nrow(obs.data), 12, byrow=TRUE)


### make an object to be read by unmarked-----
RADR.UMF.2007<- unmarkedMultFrame(y= obs.data[,10:33], siteCovs = site.df[,2:3],
                               obsCovs = list(Area=obs.cov2[[1]][9:32], Perim=obs.cov2[[2]][9:32],Temp = obs.cov2[[3]][9:32],Dry=obs.cov2[[4]][9:32]),
                               yearlySiteCovs=list(year=Yr),
                               numPrimary = 12 ) # have data from 2007 on (12 years)
summary(RADR.UMF.2007)
# 387 sites
# most sites have 5.083 observations
# 2007 to 2018 (12 periods)
# 2 secondary survey periods
# 134 sites have at least one detection


## run a multi-season model: null
model0 <- colext( psiformula = ~ 1 , # initial probability of occupancy
                  gammaformula = ~ 1, # colonization probability
                  epsilonformula = ~ 1  , # extinction probability
                  pformula = ~ 1 , # detection probability
                  data= RADR.UMF.2007)

# only 1 site is discarded
# Ca-MARSo has only one year of data 2006_1 and no covariates; not sure why it's discarded though.
site.df[63,]
summary(model0)
plogis(model0@estimates[1]@estimates) # 0.37 is the initial probability of occupancy
backTransform(model0, type="psi")

# parameter for the first year is the intercepts and effects are diffs between the param values in all other years relative to the param i n the first yera (ref level)
str(model0)



model1 <- colext(psiformula = ~1, # First-year occupancy
             gammaformula = ~ year-1, # Colonization
             epsilonformula = ~ year-1, # Extinction
             pformula = ~ year-1, # Detection
             data = RADR.UMF.2007)

model2 <- colext(psiformula = ~Elev + Forest,
                gammaformula = ~year-1+Elev,
                epsilonformula = ~year-1 + Forest ,
                pformula = ~year -1 + Forest + Dry + Temp + Perim + Area,
                data = RADR.UMF.2007)
# uh oh! convergence issues
# let's scale our variables 


summary(model2)

### Scale variables, remove data-poor years, create new dataset
list2env(obs.cov ,.GlobalEnv) # get the dataframes out of the list (added to R enviro)


# scale each variable in the obs.cov dataset
head(Area)
sArea <- Area[, c(13:32)] # 10 years: 2009 to 2018
sArea <- scale(sArea)
rownames(sArea) <- obs.cov$SiteCode

which.min(apply(sArea,MARGIN=1,min, na.rm = TRUE)) # ohlone has the smallest area
which.max(apply(sArea,MARGIN=1,max, na.rm = TRUE)) # glake has the largest area

sPerim <-Perim[, c(13:32)]
sPerim <- scale(sPerim)
rownames(sPerim) <- obs.cov$SiteCode


sTemp <-Temp[, c(13:32)]
sTemp <- scale(sTemp)
rownames(sTemp) <- obs.cov$SiteCode

sDry <- Dry[, c(13:32)]


# Subset the years to get rid of 2007 and 2008
Yr <- as.character(2009:2018)
Yr <- matrix(Yr, nrow(obs.data), 10, byrow=TRUE)

site.df.s <- data.frame(SiteCode = site.df$SiteCode,
                        sElev = scale(site.df$Elev),
                        sForest = scale(site.df$Forest))
rownames(site.df.s) <- site.df.s$SiteCode
head(site.df.s)


## let's run a new model using scaled covariates, and starting in 2009
RADR.UMF.2009.s<- unmarkedMultFrame(y= obs.data[,14:33], siteCovs = site.df.s[,2:3],
                                  obsCovs = list(Area=sArea, Perimeter=sPerim, Temperature= sTemp, Dry = sDry),
                                  yearlySiteCovs=list(year=Yr),
                                  numPrimary = 10)

model3 <- colext(psiformula = ~sElev + sForest,
                 gammaformula = ~year-1+sElev+sForest,
                 epsilonformula = ~year-1 +sElev+ sForest,
                 pformula = ~year -1 + sForest + Dry + Temperature + Perimeter + Area, #already scaled
                 data = RADR.UMF.2009.s)

summary(model3)


### Plot the output
op <- par(mfrow=c(1,2), mai=c(0.8,0.8,0.1,0.1))
nd <- data.frame(sForest=seq(-1.9, 2.1, length=50),
                 sElev = rep(0,50))

E.psi <- predict(model3, type="psi", newdata=nd, appendData=TRUE)
png("Plots/Occ.Forest.png", width = 4, height = 4, units='in', res = 700)
with(E.psi, {
  plot(sForest, Predicted, ylim=c(0,1), type="l",lwd = 2, col = "forestgreen",
       xlab="Percent cover of forest (scaled)", 
       ylab=expression(paste(hat(psi), " (Occupancy)")), cex.lab=1, cex.axis=1.2)
  lines(sForest, Predicted+1.96*SE, col=gray(0.7))
  lines(sForest, Predicted-1.96*SE, col=gray(0.7))
})
dev.off()



nd <- data.frame(sForest=rep(0,50),
                 sElev = seq(-1.5, 3.08, length.out = 50))

E.psi <- predict(model3, type="psi", newdata=nd, appendData=TRUE)
png("Plots/Occ.Elev.png", width = 4, height = 4, units='in', res = 700)
with(E.psi, {
  plot(sElev, Predicted, ylim=c(0,1), type="l",lwd = 2, col = "dodgerblue",
       xlab="Elevation (scaled)", 
       ylab=expression(paste(hat(psi), " (Occupancy)")), cex.lab=1, cex.axis=1.2)
  lines(sElev, Predicted+1.96*SE, col=gray(0.7))
  lines(sElev, Predicted-1.96*SE, col=gray(0.7))
})
dev.off()





nde <- data.frame(year = factor(seq(2009,2017, by = 1)),
                  sElev = rep(0,9),
                  sForest = rep(0,9))


E.p <- predict(model3, type="ext", newdata=nde, appendData=TRUE)

with(E.p, {
  plot(2009:2017, Predicted,type="o",
       xlab="Year", ylab="Extinction",
       cex.lab=0.8, cex.axis=0.8, ylim = c(0,1), lwd = 2, pch = 16, col = "dodgerblue")
  segments(x0 = 2009:2017, x1 = 2009:2017, y0 = lower, y1 = upper)
})



nde <- data.frame(year = factor(seq(2009,2017, by = 1)),
                  sElev = rep(0,9),
                  sForest = rep(0,9))


C.p <- predict(model3, type="col", newdata=nde, appendData=TRUE)

with(C.p, {
  plot(2009:2017, Predicted,type="o",
       xlab="Year", ylab="Colonization",
       cex.lab=0.8, cex.axis=0.8, ylim = c(0,1), lwd = 2, pch = 16, col = "dodgerblue")
  segments(x0 = 2009:2017, x1 = 2009:2017, y0 = lower, y1 = upper)
})

### Update the model by dropping ns predictors-----
# remove the non significant variables and re-run
model4 <- colext(psiformula = ~sElev + sForest,
                 gammaformula = ~year,
                 epsilonformula = ~year,
                 pformula = ~year -1 + Dry + Perimeter + Area, #already scaled
                 data = RADR.UMF.2009.s)
summary(model4)
# nothing is significant! 
