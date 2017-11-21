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

# Add Temperature summary data to my peptide abundance dataframe
T.mean <- aggregate(value ~ variable, T.Data.melted.noNA, mean)
T.var <- aggregate(value ~ variable, T.Data.melted.noNA, var)
T.mean.var <- T.mean*T.var
T.max <- aggregate(value ~ variable, T.Data.melted.noNA, max)
T.min <- aggregate(value ~ variable, T.Data.melted.noNA, min)
T.over.20 <- aggregate(value ~ variable, data=subset(T.Data.melted.noNA, value>20), FUN = function(x){NROW(x)})
T.over.20$value <- (T.over.20$value)/10 #represents total # mins over 20degC; transformed to be ~same scale as other variables
T.over.20
#  DateTime<as.POSIXct(strptime('7/19/16 23:50', format="%m/%d/%Y %H:%M") tried to remove dates >7/19 at 23:50 b/c some probes stopped at that time. Couldn't get it to work, though
T.stats <- cbind(T.mean, T.var$value, T.mean.var$value, T.max$value, T.min$value, T.over.20$value)
names(T.stats) <- c("Site", "T.mean", "T.var", "T.mean.var", "T.max", "T.min", "T.over.20")
data.melted.plus.pepsum$BOTH <- gsub('CI-Bare', 'CIB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('CI-Eel', 'CIE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('WB-Bare', 'WBB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('WB-Eel', 'WBE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('FB-Bare', 'FBB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('FB-Eel', 'FBE', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('PG-Bare', 'PGB', data.melted.plus.pepsum$BOTH)
data.melted.plus.pepsum$BOTH <- gsub('PG-Eel', 'PGE', data.melted.plus.pepsum$BOTH)
data.pepsum.T <- merge(x=data.melted.plus.pepsum, y=T.stats, by.x = "BOTH", by.y = "Site", all.x=TRUE, all.y=TRUE)

# Running linear mixed model 
# https://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
require(lme4)
library(car)
test.lmm <- lmer(lamda.t ~  T.mean + T.var + T.mean.var + (1|SAMPLE) + (1|SITE), data= subset(data.pepsum.T, Protein.Name %in% "HSP70"))
test.lmm <- lmer(lamda.t ~  T.mean + T.var + T.mean.var + (1|SAMPLE) + (1|SITE), data=data.pepsum.T)
summary(test.lmm)
Anova(test.lmm)
