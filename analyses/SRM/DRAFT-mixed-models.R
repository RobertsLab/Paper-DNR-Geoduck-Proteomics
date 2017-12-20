# Playing with mixed models
# Let's use the dataset where I have summed transition abundance for each peptide: "data.melted.plus.pepsum"

par(mfrow = c(3, 1), mar=c(4,1,3,1), oma=c(1,2,1,1), cex.main= 1.3, cex=.75)

# First, we have a suspicion that the data isn't normal. Let's check using qqplot:
qqnorm(data.melted.plus.pepsum$Area, main = "Normal Q-Q Plot, Peptide Area",
       xlab = "Theoretical Quantiles", ylab = "Peptide Abundance",
       plot.it = TRUE)
qqline(data.melted.plus.pepsum$Area)

# Obviously not normal. Let's use "transformTukey" to identify the best lamda value to transform our dataa
# Great resource: http://rcompanion.org/handbook/I_12.html
library(rcompanion)

#Identify lambda value to use to transform data; value is printed in the console, and is 0.175, with Shapiro = 3.572e-10
tukeytrans <- transformTukey(data.melted.plus.pepsum$Area+1, plotit=FALSE, statistic = 1) 

#View distribution of lamda-transformed data
plotNormalHistogram(tukeytrans, main="Lamda-Transformed Distribution \nLamda=0.175") 

#Create new column in dataframe with lambda-transformed area data
data.melted.plus.pepsum$lamda.t <- (data.melted.plus.pepsum$Area+1)^0.175

#Plot lambda-transformed data to assess normality - much better!
qqnorm(data.melted.plus.pepsum$lamda.t, main = "Normal Q-Q Plot, \nLambda-transformed (l=0.175) Peptide Area",
       xlab = "Theoretical Quantiles", ylab = "Peptide Abundance",
       plot.it = TRUE)
qqline(data.melted.plus.pepsum$lamda.t)

# Let's calculate the peptide% of the total abundance in each sample 
Sample.Sum <- aggregate(Area ~ SAMPLE, data.melted.plus.pepsum, sum)
names(Sample.Sum) <- c("SAMPLE", "TotalArea")
data.melted.plus.pepsum <- merge(x=data.melted.plus.pepsum, y=Sample.Sum, by.x="SAMPLE", by.y="SAMPLE", all.x=T, all.y=T)
data.melted.plus.pepsum$AreaRatio <- data.melted.plus.pepsum$Area/data.melted.plus.pepsum$TotalArea

# Figure out which lambda to use for transformation to normal distribution, = 0.225
tukeytrans2 <- transformTukey(data.melted.plus.pepsum$AreaRatio, plotit=FALSE, statistic = 1) 

#Create new column in dataframe with lambda-transformed area ratio data
data.melted.plus.pepsum$lambda.rt <- (data.melted.plus.pepsum$AreaRatio)^0.225
hist(data.melted.plus.pepsum$lambda.rt)

# Calculate summary data for T & DO
T.mean <- aggregate(value ~ variable, T.Data.melted.noNA, mean)
T.var <- aggregate(value ~ variable, T.Data.melted.noNA, var)
T.mean.var <- T.mean*T.var
T.over.20 <- aggregate(value ~ variable, data=subset(T.Data.melted.noNA, value>20), FUN = function(x){NROW(x)})
T.over.20$value <- (T.over.20$value)/10 #represents total # mins over 20degC; transformed to be ~same scale as other variables
# Some variable describing degree/rate of T change, like amplitude/period? 
DO.mean <- aggregate(value ~ variable, DO.Data.melted.noNA, mean)
DO.var <- aggregate(value ~ variable, DO.Data.melted.noNA, var)
DO.mean.var <- DO.mean*DO.var
T.DO.stats <- cbind(T.mean, T.var$value, T.mean.var$value, T.over.20$value, DO.mean$value, DO.var$value, DO.mean.var$value)
names(T.DO.stats) <- c("Site", "T.mean", "T.var", "T.mean.var", "T.over.20", "DO.mean", "DO.var", "DO.mean.var")

# Calculate summary data for pH (this is missing PGE, so have to generate separately then merge with the other stats)
pH.mean <- aggregate(value ~ variable, pH.Data.melted.noNA, mean)
pH.var <- aggregate(value ~ variable, pH.Data.melted.noNA, var)
pH.mean.var <- pH.mean*pH.var
pH.stats <- cbind(pH.mean, pH.var$value, pH.mean.var$value)
names(pH.stats) <- c("Site", "pH.mean", "pH.var", "pH.mean.var")

# Merge DO/T & pH stats together 
Env.Stats <- merge(x=T.DO.stats, y=pH.stats, by.x="Site", by.y = "Site", all.x=T)
write.csv(file="../../analyses/Environmental/Env-Stats.csv", Env.Stats)
View( Env.Stats)
#  DateTime<as.POSIXct(strptime('7/19/16 23:50', format="%m/%d/%Y %H:%M") tried to remove dates >7/19 at 23:50 b/c some probes stopped at that time. Couldn't get it to work, though

# Change site/habitat id in protein database to the shorthand codes
data.melted.plus.pepsum$BOTH <- gsub('CI-Bare', 'CIB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('CI-Eel', 'CIE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('WB-Bare', 'WBB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('WB-Eel', 'WBE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('FB-Bare', 'FBB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('FB-Eel', 'FBE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('PG-Bare', 'PGB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('PG-Eel', 'PGE', data.melted.plus.pepsum$BOTH)

# Merge protein data with environmental stats 
data.pepsum.Env.Stats <- data.frame(merge(x=data.melted.plus.pepsum, y=Env.Stats, by.x = "BOTH", by.y = "Site", all.x=TRUE, all.y=TRUE), stringsAsFactors = F)
View(data.pepsum.Env.Stats) #check out column names to use in mixed model


# Running linear mixed model 
# https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
require(lme4)
library(car)

# Run linear mixed model with all proteins together, but nest the environmental factors by protein
lmm.all <- lmer(lambda.rt ~  (T.mean + T.var + DO.mean + DO.var + pH.mean + pH.var)/Peptide.Sequence + (1|SAMPLE), data= data.pepsum.Env.Stats, na.action = na.exclude)
lmm.mean <- lmer(lamda.t ~  (T.mean + DO.mean + pH.mean)/Protein.Name + (1|SAMPLE), data= data.pepsum.Env.Stats, na.action = na.exclude)
lmm.var <- lmer(lamda.t ~  (T.var + DO.var + pH.var)/Protein.Name + (1|SAMPLE), data= data.pepsum.Env.Stats, na.action = na.exclude)
lmm.mean.var <- lmer(lamda.t ~  (T.mean.var + DO.mean.var + pH.mean.var)/Protein.Name + (1|SAMPLE), data= data.pepsum.Env.Stats, na.action = na.exclude)
lmm.test <- lmer(lamda.t ~  (DO.mean * DO.var)/Protein.Name + (1|SAMPLE), data= data.pepsum.Env.Stats, na.action = na.exclude)
Anova(lmm.all)
Anova(lmm.mean)
Anova(lmm.var)
Anova(lmm.mean.var)
anova(lmm.mean, lmm.var, lmm.mean.var)

# lmm.T <- lmer(lamda.t ~  (T.mean + T.var + I(T.mean.var*10))/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"), na.action = na.exclude)
lmm.HSP70.mean <- lmer(lamda.t ~  (T.mean*DO.mean*pH.mean)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"), na.action = na.exclude)
lmm.HSP70.var <- lmer(lamda.t ~  (T.var*DO.var*pH.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"), na.action = na.exclude)
lmm.HSP70.mean.var <- lmer(lamda.t ~  (T.mean.var*DO.mean.var*pH.mean.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"), na.action = na.exclude)
Anova(lmm.HSP70.mean)
Anova(lmm.HSP70.var)
Anova(lmm.HSP70.mean.var)
library(MASS)


lmm.HSP90.mean <- lmer(lamda.t ~  (T.mean + DO.mean + pH.mean)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP90-alpha"), na.action = na.exclude)
lmm.HSP90.var <- lmer(lamda.t ~  (T.var + DO.var + pH.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP90-alpha"), na.action = na.exclude)
lmm.HSP90.mean.var <- lmer(lamda.t ~  (T.mean.var + DO.mean.var + pH.mean.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP90-alpha"), na.action = na.exclude)
summary(lmer(lamda.t ~  (T.mean.var + DO.mean.var + pH.mean.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP90-alpha"), na.action = na.exclude))
Anova(lmm.HSP90.mean)
Anova(lmm.HSP90.var)
Anova(lmm.HSP90.mean.var)

lmm.PDI.mean <- lmer(lamda.t ~  (T.mean + DO.mean + pH.mean)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "PDI"), na.action = na.exclude)
lmm.PDI.var <- lmer(lamda.t ~  (T.var + DO.var + pH.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "PDI"), na.action = na.exclude)
lmm.PDI.mean.var <- lmer(lamda.t ~  (T.mean.var + DO.mean.var + pH.mean.var)/Peptide.Sequence + (1|SAMPLE), data= subset(data.pepsum.Env.Stats, Protein.Name %in% "PDI"), na.action = na.exclude)
Anova(lmm.PDI.mean)
Anova(lmm.PDI.var)
Anova(lmm.PDI.mean.var)

#anova(lm1, lm2, lm3) <- lowercase "anova" function evaluate signfificance of fixed effects models

##============================
# Testing out multiple linear model (not mixed)
plot(data.pepsum.Env.Stats[,c(9,10,14,17)]) #plot area & env. mean data 
plot(data.pepsum.Env.Stats[,c(9,11,15,18)]) #plot area & env. var data
plot(data.pepsum.Env.Stats[,c(9,12,16,19)]) #plot area & env. mean.var data
lm.mean.all <- lm(lamda.t ~ T.mean + DO.mean + pH.mean, data=data.pepsum.Env.Stats)
summary(lm.mean.all)
lm.var.all <- lm(lamda.t ~ T.var + DO.var + pH.var, data=data.pepsum.Env.Stats)
summary(lm.var.all)
lm.mean.var.all <- lm(lamda.t ~ T.mean.var + DO.mean.var + pH.mean.var, data=data.pepsum.Env.Stats)
summary(lm.mean.var.all)
lm.all.all <- lm(lamda.t ~ T.mean + T.var + T.mean.var + DO.mean + DO.var + DO.mean.var + pH.mean + pH.var + pH.mean.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
summary(lm.all.all)

anova(lm.all.all, lm.mean.all) # Does non-mean data contribute significant information to the protein abundance? P=0.000973, reject null; so we cannot ignore non-mean data
anova(lm.all.all, lm.var.all) # How about non-variance data?
#P=0.07637, cannot reject null; so we CAN ignore non-var data (so, variance may be the primary driver)
anova(lm.all.all, lm.mean.var.all) #And how about non-mean.var? 
#P=0.05089, cannot reject null; so we CAN ignore non-mean.var data... likely b/c var is a factor 

### Let's do this in an organized fashion just with the mean & var parameters, where we run the model with all except one parameter 
lm.all.all <- lm(lamda.t ~ -1 + T.mean + T.var  + DO.mean + DO.var + pH.mean + pH.var, data=data.pepsum.Env.Stats)
summary(lm.all.all)
lm.all.nopHv <- lm(lamda.t ~ T.mean + T.var + DO.mean + DO.var + pH.mean, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
lm.all.nopHm <- lm(lamda.t ~ T.mean + T.var + DO.mean + DO.var + pH.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
lm.all.noDOv <- lm(lamda.t ~ T.mean + T.var + DO.mean + pH.mean + pH.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
lm.all.noDOm <- lm(lamda.t ~ T.mean + T.var + DO.var + pH.mean + pH.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
lm.all.noTv <- lm(lamda.t ~ T.mean + DO.mean + DO.var + pH.mean + pH.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])
lm.all.noTm <- lm(lamda.t ~ T.var  + DO.mean + DO.var + pH.mean + pH.var, data=data.pepsum.Env.Stats[which(!is.na(data.pepsum.Env.Stats)),])

# Now let's compare the ALL parameters model to each other model where we've removed one parameter
anova(lm.all.all, lm.all.nopHv) #removed pH var; P=0.1482
anova(lm.all.all, lm.all.nopHm) #removed pH mean; P=0.802
anova(lm.all.all, lm.all.noDOv) #removed DO var; P=0.0004025 <--- DO variance important
anova(lm.all.all, lm.all.noDOm) #removed DO mean; p=0.07392
anova(lm.all.all, lm.all.noTv) #removed T var; P=0.004817 <--- T variance important 
anova(lm.all.all, lm.all.noTm) #removed T mean; P=0.9331

# Remove the intercept - R^2 is WAAAAY better 
lm0.all.all <- lm(lamda.t ~ -1 + T.mean* DO.mean*pH.mean + T.var*DO.var*pH.var, data=data.pepsum.Env.Stats)

summary(lm0.all.all)

# Check out residuals, outliers
plot(rstandard(lm0.all.all), main="Standardized Residuals \nShould be between [-2,2]") #plot standardized residuals. Outliers are points <-2 or >2. Good news- most of my points are within the [-2,2] range
par(mfrow=c(1,4))
plot(lm0.all.all, which=1:4)

# Try a Generalized Linear Model, since my environmental data isn't continuous (it's basically discrete)
glm.all.al <- glm(lamda.t ~ T.mean + DO.mean + pH.mean + T.var + DO.var + pH.var, data=data.pepsum.Env.Stats, na.action=na.exclude)
summary(glm.all.al)
plot(glm.all.al, which=1:4)


# What I've learned
# Response variable = the data that we measured, aka protein abundance
# Explanatory variable = the data that we controlled and/or is being used to predict/explain the response variable
# Multiple linear model: use multiple  explanatory variables to predict the response variable
# Generalized linear model: To be used with non-continuous data, aka binary (live/dead), OR discrete categories (e.g. Site)
# Mixed Linear Model: a way of including a random explanatory variable, i.e. sample # (or "subject" in social sciences)
# DO variance and T variance appear to be the most influential in a multiple regression model using data from all proteins. 

# I want to run models on each protein separately. How? 
lm.HSP70.all <- lm(lamda.t ~ -1 + T.mean + DO.mean + pH.mean + T.var + DO.var + pH.var, data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"))
glm.HSP70.all <- glm(lamda.t ~ T.mean + DO.mean + pH.mean + T.var + DO.var + pH.var, data= subset(data.pepsum.Env.Stats, Protein.Name %in% "HSP70"))
summary(lm.HSP70.all)
summary(glm.HSP70.all)
par(mfrow=c(1,4))
plot(glm.HSP70.all, which=1:4)
plot(lm.HSP70.all, which=1:4)


