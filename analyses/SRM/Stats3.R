#Stats3 script: prep environmental and protein data for regression models

# Pull summary statistics on environmental data by Probe Location

env.mean.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, mean)
env.mean.loc$stat <- "mean"
env.var.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, var)
env.var.loc$stat <- "var"
env.sd.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, sd)
env.sd.loc$stat <- "sd"
env.max.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, max)
env.max.loc$stat <- "max"
env.min.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, min)
env.min.loc$stat <- "min"
env.median.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts, median)
env.median.loc$stat <- "median"
env.stats.location <- rbind(env.mean.loc, env.var.loc, env.sd.loc, env.max.loc, env.min.loc, env.median.loc)
env.stats.location$comparison <- "variable"

# Write functions to calculate the % of data > 1 standard deviation from the mean, by REGION
Percent.sd.1.loc <- function(parameter, group) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+c))/d + sum(a<(b-c))/d)*100)
}

# Write functions to calculate the % of data > 2 standard deviation from the mean, by REGION
Percent.sd.2.loc <- function(parameter, group) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+2*c))/d + sum(a<(b-2*c))/d)*100)
}

Percent.high <- function(group, parameter, threshold) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"]
  b <- length(a[a>threshold])/length(a)
  return((b))
}
Percent.low <- function(group, parameter, threshold) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$variable %in% group),"value"]
  b <- length(a[a<threshold])/length(a)
  return((b))
} 

# Calculate % pH is > 8, grouped by Location
PGE.pH.high <- c("PGE", "pH", Percent.high(group="PGE", parameter="pH", threshold=8), "Percent.high", "variable")
PGB.pH.high <- c("PGB", "pH", Percent.high(group="PGB", parameter="pH", threshold=8), "Percent.high", "variable")
FBE.pH.high <- c("FBE", "pH", Percent.high(group="FBE", parameter="pH", threshold=8), "Percent.high", "variable")
FBB.pH.high <- c("FBB", "pH", Percent.high(group="FBB", parameter="pH", threshold=8), "Percent.high", "variable")
WBE.pH.high <- c("WBE", "pH", Percent.high(group="WBE", parameter="pH", threshold=8), "Percent.high", "variable")
WBB.pH.high <- c("WBB", "pH", Percent.high(group="WBB", parameter="pH", threshold=8), "Percent.high", "variable")
CIE.pH.high <- c("CIE", "pH", Percent.high(group="CIE", parameter="pH", threshold=8), "Percent.high", "variable")
CIB.pH.high <- c("CIB", "pH", Percent.high(group="CIB", parameter="pH", threshold=8), "Percent.high", "variable")

# Calculate % pH is < 7.6, grouped by Location
PGE.pH.low <- c("PGE", "pH", Percent.low(group="PGE", parameter="pH", threshold=7.6), "Percent.low", "variable")
PGB.pH.low <- c("PGB", "pH", Percent.low(group="PGB", parameter="pH", threshold=7.6), "Percent.low", "variable")
FBE.pH.low <- c("FBE", "pH", Percent.low(group="FBE", parameter="pH", threshold=7.6), "Percent.low", "variable")
FBB.pH.low <- c("FBB", "pH", Percent.low(group="FBB", parameter="pH", threshold=7.6), "Percent.low", "variable")
WBE.pH.low <- c("WBE", "pH", Percent.low(group="WBE", parameter="pH", threshold=7.6), "Percent.low", "variable")
WBB.pH.low <- c("WBB", "pH", Percent.low(group="WBB", parameter="pH", threshold=7.6), "Percent.low", "variable")
CIE.pH.low <- c("CIE", "pH", Percent.low(group="CIE", parameter="pH", threshold=7.6), "Percent.low", "variable")
CIB.pH.low <- c("CIB", "pH", Percent.low(group="CIB", parameter="pH", threshold=7.6), "Percent.low", "variable")

# Calculate % each environmental parameter is > 1 sd away from mean, grouped by Location
PGEpH.1 <- c("PGE", "pH", Percent.sd.1.loc(parameter="pH", group="PGE"), "Percent.sd.1.loc", "variable")
PGBpH.1 <- c("PGB", "pH", Percent.sd.1.loc(parameter="pH", group="PGB"), "Percent.sd.1.loc", "variable")
FBEpH.1 <- c("FBE", "pH", Percent.sd.1.loc(parameter="pH", group="FBE"), "Percent.sd.1.loc", "variable")
FBBpH.1 <- c("FBB", "pH", Percent.sd.1.loc(parameter="pH", group="FBB"), "Percent.sd.1.loc", "variable")
WBEpH.1 <- c("WBE", "pH", Percent.sd.1.loc(parameter="pH", group="WBE"), "Percent.sd.1.loc", "variable")
WBBpH.1 <- c("WBB", "pH", Percent.sd.1.loc(parameter="pH", group="WBB"), "Percent.sd.1.loc", "variable")
CIEpH.1 <- c("CIE", "pH", Percent.sd.1.loc(parameter="pH", group="CIE"), "Percent.sd.1.loc", "variable")
CIBpH.1 <- c("CIB", "pH", Percent.sd.1.loc(parameter="pH", group="CIB"), "Percent.sd.1.loc", "variable")

PGEDO.1 <- c("PGE", "DO", Percent.sd.1.loc(parameter="DO", group="PGE"), "Percent.sd.1.loc", "variable")
PGBDO.1 <- c("PGB", "DO", Percent.sd.1.loc(parameter="DO", group="PGB"), "Percent.sd.1.loc", "variable")
FBEDO.1 <- c("FBE", "DO", Percent.sd.1.loc(parameter="DO", group="FBE"), "Percent.sd.1.loc", "variable")
FBBDO.1 <- c("FBB", "DO", Percent.sd.1.loc(parameter="DO", group="FBB"), "Percent.sd.1.loc", "variable")
WBEDO.1 <- c("WBE", "DO", Percent.sd.1.loc(parameter="DO", group="WBE"), "Percent.sd.1.loc", "variable")
WBBDO.1 <- c("WBB", "DO", Percent.sd.1.loc(parameter="DO", group="WBB"), "Percent.sd.1.loc", "variable")
CIEDO.1 <- c("CIE", "DO", Percent.sd.1.loc(parameter="DO", group="CIE"), "Percent.sd.1.loc", "variable")
CIBDO.1 <- c("CIB", "DO", Percent.sd.1.loc(parameter="DO", group="CIB"), "Percent.sd.1.loc", "variable")

PGESal.1 <- c("PGE", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="PGE"), "Percent.sd.1.loc", "variable")
PGBSal.1 <- c("PGB", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="PGB"), "Percent.sd.1.loc", "variable")
FBESal.1 <- c("FBE", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="FBE"), "Percent.sd.1.loc", "variable")
FBBSal.1 <- c("FBB", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="FBB"), "Percent.sd.1.loc", "variable")
WBESal.1 <- c("WBE", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="WBE"), "Percent.sd.1.loc", "variable")
WBBSal.1 <- c("WBB", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="WBB"), "Percent.sd.1.loc", "variable")
CIESal.1 <- c("CIE", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="CIE"), "Percent.sd.1.loc", "variable")
CIBSal.1 <- c("CIB", "Salinity", Percent.sd.1.loc(parameter="Salinity", group="CIB"), "Percent.sd.1.loc", "variable")

PGETemp.1 <- c("PGE", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="PGE"), "Percent.sd.1.loc", "variable")
PGBTemp.1 <- c("PGB", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="PGB"), "Percent.sd.1.loc", "variable")
FBETemp.1 <- c("FBE", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="FBE"), "Percent.sd.1.loc", "variable")
FBBTemp.1 <- c("FBB", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="FBB"), "Percent.sd.1.loc", "variable")
WBETemp.1 <- c("WBE", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="WBE"), "Percent.sd.1.loc", "variable")
WBBTemp.1 <- c("WBB", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="WBB"), "Percent.sd.1.loc", "variable")
CIETemp.1 <- c("CIE", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="CIE"), "Percent.sd.1.loc", "variable")
CIBTemp.1 <- c("CIB", "Temperature", Percent.sd.1.loc(parameter="Temperature", group="CIB"), "Percent.sd.1.loc", "variable")

# Calculate % > 2 standard deviations from mean
PGEpH.2 <- c("PGE", "pH", Percent.sd.2.loc(parameter="pH", group="PGE"), "Percent.sd.2.loc", "variable")
PGBpH.2 <- c("PGB", "pH", Percent.sd.2.loc(parameter="pH", group="PGB"), "Percent.sd.2.loc", "variable")
FBEpH.2 <- c("FBE", "pH", Percent.sd.2.loc(parameter="pH", group="FBE"), "Percent.sd.2.loc", "variable")
FBBpH.2 <- c("FBB", "pH", Percent.sd.2.loc(parameter="pH", group="FBB"), "Percent.sd.2.loc", "variable")
WBEpH.2 <- c("WBE", "pH", Percent.sd.2.loc(parameter="pH", group="WBE"), "Percent.sd.2.loc", "variable")
WBBpH.2 <- c("WBB", "pH", Percent.sd.2.loc(parameter="pH", group="WBB"), "Percent.sd.2.loc", "variable")
CIEpH.2 <- c("CIE", "pH", Percent.sd.2.loc(parameter="pH", group="CIE"), "Percent.sd.2.loc", "variable")
CIBpH.2 <- c("CIB", "pH", Percent.sd.2.loc(parameter="pH", group="CIB"), "Percent.sd.2.loc", "variable")

PGEDO.2 <- c("PGE", "DO", Percent.sd.2.loc(parameter="DO", group="PGE"), "Percent.sd.2.loc", "variable")
PGBDO.2 <- c("PGB", "DO", Percent.sd.2.loc(parameter="DO", group="PGB"), "Percent.sd.2.loc", "variable")
FBEDO.2 <- c("FBE", "DO", Percent.sd.2.loc(parameter="DO", group="FBE"), "Percent.sd.2.loc", "variable")
FBBDO.2 <- c("FBB", "DO", Percent.sd.2.loc(parameter="DO", group="FBB"), "Percent.sd.2.loc", "variable")
WBEDO.2 <- c("WBE", "DO", Percent.sd.2.loc(parameter="DO", group="WBE"), "Percent.sd.2.loc", "variable")
WBBDO.2 <- c("WBB", "DO", Percent.sd.2.loc(parameter="DO", group="WBB"), "Percent.sd.2.loc", "variable")
CIEDO.2 <- c("CIE", "DO", Percent.sd.2.loc(parameter="DO", group="CIE"), "Percent.sd.2.loc", "variable")
CIBDO.2 <- c("CIB", "DO", Percent.sd.2.loc(parameter="DO", group="CIB"), "Percent.sd.2.loc", "variable")

PGESal.2 <- c("PGE", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="PGE"), "Percent.sd.2.loc", "variable")
PGBSal.2 <- c("PGB", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="PGB"), "Percent.sd.2.loc", "variable")
FBESal.2 <- c("FBE", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="FBE"), "Percent.sd.2.loc", "variable")
FBBSal.2 <- c("FBB", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="FBB"), "Percent.sd.2.loc", "variable")
WBESal.2 <- c("WBE", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="WBE"), "Percent.sd.2.loc", "variable")
WBBSal.2 <- c("WBB", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="WBB"), "Percent.sd.2.loc", "variable")
CIESal.2 <- c("CIE", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="CIE"), "Percent.sd.2.loc", "variable")
CIBSal.2 <- c("CIB", "Salinity", Percent.sd.2.loc(parameter="Salinity", group="CIB"), "Percent.sd.2.loc", "variable")

PGETemp.2 <- c("PGE", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="PGE"), "Percent.sd.2.loc", "variable")
PGBTemp.2 <- c("PGB", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="PGB"), "Percent.sd.2.loc", "variable")
FBETemp.2 <- c("FBE", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="FBE"), "Percent.sd.2.loc", "variable")
FBBTemp.2 <- c("FBB", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="FBB"), "Percent.sd.2.loc", "variable")
WBETemp.2 <- c("WBE", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="WBE"), "Percent.sd.2.loc", "variable")
WBBTemp.2 <- c("WBB", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="WBB"), "Percent.sd.2.loc", "variable")
CIETemp.2 <- c("CIE", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="CIE"), "Percent.sd.2.loc", "variable")
CIBTemp.2 <- c("CIB", "Temperature", Percent.sd.2.loc(parameter="Temperature", group="CIB"), "Percent.sd.2.loc", "variable")

sd.percents.loc <- data.frame(rbind(PGE.pH.high,PGB.pH.high,FBE.pH.high,FBB.pH.high,WBE.pH.high, WBB.pH.high,CIE.pH.high,CIB.pH.high,PGE.pH.low,PGB.pH.low,FBE.pH.low,FBB.pH.low,WBE.pH.low, WBB.pH.low,CIE.pH.low,CIB.pH.low,PGEpH.1, PGBpH.1, FBEpH.1, FBBpH.1, WBEpH.1, WBBpH.1, CIEpH.1, CIBpH.1, PGEDO.1, PGBDO.1, FBEDO.1, FBBDO.1, WBEDO.1, WBBDO.1, CIEDO.1, CIBDO.1, PGESal.1, PGBSal.1, FBESal.1, FBBSal.1, WBESal.1, WBBSal.1, CIESal.1, CIBSal.1, PGETemp.1, PGBTemp.1, FBETemp.1, FBBTemp.1, WBETemp.1, WBBTemp.1, CIETemp.1, CIBTemp.1, PGEpH.2, PGBpH.2, FBEpH.2, FBBpH.2, WBEpH.2, WBBpH.2, CIEpH.2, CIBpH.2, PGEDO.2, PGBDO.2, FBEDO.2, FBBDO.2, WBEDO.2, WBBDO.2, CIEDO.2, CIBDO.2, PGESal.2, PGBSal.2, FBESal.2, FBBSal.2, WBESal.2, WBBSal.2, CIESal.2, CIBSal.2, PGETemp.2, PGBTemp.2, FBETemp.2, FBBTemp.2, WBETemp.2, WBBTemp.2, CIETemp.2, CIBTemp.2))
colnames(sd.percents.loc) <- c("variable", "metric", "value", "stat", "comparison")
env.stats.location <- rbind(env.stats.location, sd.percents.loc)

# Save the region-based stats dataframe as .csv
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/env-stats-by-location.csv", env.stats.location)

# Plot environmental variables: are they linearly correlated?
# First, I need convert dataframe to wide form
library(reshape)
Env.Data.Master.noOuts.wide <- cast(Env.Data.Master.noOuts, DateTime+variable ~ metric, value="value")

par(mfrow = c(3, 2))
plot(Env.Data.Master.noOuts.wide$Temperature ~ Env.Data.Master.noOuts.wide$DO, xlab="DO", ylab="Temperature", main="Temp~DO")
plot(Env.Data.Master.noOuts.wide$Temperature ~ Env.Data.Master.noOuts.wide$pH, xlab="pH", ylab="Temperature", main="Temp~pH")
plot(Env.Data.Master.noOuts.wide$Temperature ~ Env.Data.Master.noOuts.wide$Salinity, xlab="Salinity", ylab="Temperature", main="Temp~Salinity")
plot(Env.Data.Master.noOuts.wide$DO ~ Env.Data.Master.noOuts.wide$pH, xlab="pH", ylab="DO", main="DO~pH")
plot(Env.Data.Master.noOuts.wide$DO ~ Env.Data.Master.noOuts.wide$Salinity, xlab="Salinity", ylab="DO", main="DO~Salinity")
plot(Env.Data.Master.noOuts.wide$pH ~ Env.Data.Master.noOuts.wide$Salinity, xlab="Salinity", ylab="pH", main="pH~Salinity")
# Don't really see any correlations.

# Merge Environmental summary data with my peptide data
env.stats.location$comparison <- "loc"
env.stats.location$stat <- gsub("Percent.sd.1.loc", "sd.1", env.stats.location$stat)
env.stats.location$stat <- gsub("Percent.sd.2.loc", "sd.2", env.stats.location$stat)
env.stats.location$stats <- paste(env.stats.location$metric, env.stats.location$stat, env.stats.location$comparison, sep="-")
env.stats.location.wide <- cast(env.stats.location, variable~stats, value="value")

# Merge summary statistics with peptide data

# Change site/habitat id in protein database to the shorthand codes
data.melted.plus.pepsum.wide$BOTH <- gsub('CI-Bare', 'CIB', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('CI-Eel', 'CIE', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('WB-Bare', 'WBB', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('WB-Eel', 'WBE', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('FB-Bare', 'FBB', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('FB-Eel', 'FBE', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('PG-Bare', 'PGB', data.melted.plus.pepsum.wide$BOTH)
data.melted.plus.pepsum.wide$BOTH <- gsub('PG-Eel', 'PGE', data.melted.plus.pepsum.wide$BOTH)

# Merge protein data with environmental summary stats 
data.pepsum.Env.Stats <- data.frame(merge(x=data.melted.plus.pepsum.wide, y=env.stats.location.wide, by.x = "BOTH", by.y = "variable", all.x=TRUE, all.y=TRUE), stringsAsFactors = F)
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/data-pepsum-Env-Stats.csv", data.pepsum.Env.Stats)

# Merge with growth data for each vial
Growth <- read.csv("../../data/GeoduckGrowth.csv", header=TRUE, stringsAsFactors = FALSE)
data.pepsum.Env.Stats <- data.frame(merge(x=data.pepsum.Env.Stats, y=Growth[,c("Perc.Growth", "PRVial")], by.x="SAMPLE", by.y="PRVial", all.x=T))
data.melted.plus.pepsum.wide <- data.frame(merge(x=data.melted.plus.pepsum.wide, y=Growth[,c("Perc.Growth", "PRVial")], by.x="SAMPLE", by.y="PRVial", all.x=T))

# Check out how growth varied between sites
par(mfrow=c(1,1))
hist(data.pepsum.Env.Stats$Perc.Growth)
growth.aov <-aov(Perc.Growth ~ REGION*SITE*TREATMENT, data=data.pepsum.Env.Stats)
growth.res <- growth.aov$residuals
hist(growth.res, main="Histogram of Growth ANOVA Residuals", xlab="Residuals") #normal distribution = check
library(car)
leveneTest(Perc.Growth*100 ~ REGION*SITE*TREATMENT, data=data.pepsum.Env.Stats) #unequal variance, BUT ANOVA p-values are so low that I am confident in the results
summary(growth.aov)
TukeyHSD(growth.aov)

View(data.pepsum.Env.Stats)
plot(data.pepsum.Env.Stats$Perc.Growth*100 ~ data.pepsum.Env.Stats$SITE)

# Plot ALL peptides against growth. Summary() shows equation with R^2
Pep1Growth <- lm(`Pep1`+`Pep2`+`Pep3` ~ `Perc.Growth`, data=data.melted.plus.pepsum.wide)
summary(Pep1Growth)
with(data.melted.plus.pepsum.wide, plot(`Perc.Growth`, `Pep1`))
abline(Pep1Growth)

# View env. summary stats plotted against each other, to ID which parameters are correlated and thus I can ignore for model 
DO.plots <- data.pepsum.Env.Stats[,grepl("DO.*loc", names(data.pepsum.Env.Stats))]
pH.plots <- data.pepsum.Env.Stats[,grepl("pH.*loc", names(data.pepsum.Env.Stats))]
Temp.plots <- data.pepsum.Env.Stats[,grepl("Temperature.*loc", names(data.pepsum.Env.Stats))]
Sal.plots <- data.pepsum.Env.Stats[,grepl("Salinity.*loc", names(data.pepsum.Env.Stats))]

# Create and save the environmental stats correlation plots
png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/DO-Stats-Correlation-Plots.png", width = 1000, height = 1000)
plot(DO.plots[,1:8], main="DO Stats Correlation Plots") #DO stats correlation plots
dev.off()
png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/pH-Stats-Correlation-Plots.png", width = 1000, height = 1000)
plot(pH.plots[,1:8], main="pH Stats Correlation Plots") #pH stats correlation plots
dev.off()
png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/Temp-Stats-Correlation-Plots.png", width = 1000, height = 1000)
plot(Temp.plots[,1:8], main="Temperature Stats Correlation Plots") #Temp stats correlation plots
dev.off()
png("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/Salinity-Stats-Correlation-Plots.png", width = 1000, height = 1000)
plot(Sal.plots[,1:8], main="Salinity Stats Correlation Plots") #Sal stats correlation plots
dev.off()

# Convert environmental stats to numeric
data.pepsum.Env.Stats[11:50] <- as.numeric(as.character(unlist(data.pepsum.Env.Stats[11:50])), na.action=na.omit)

# Isolate data related to diff. expressed proteins: 
# ====> Puromycin-sensitive aminopeptidase
# ====> HSP70
# ====> HSP90-alpha
# ====> Trifunctional enzyme subunit
data.pepsum.Env.Stats[data.pepsum.Env.Stats == "NaN"] <- NA
data.pepsum.Env.Stats.Puromycin <- data.pepsum.Env.Stats[grepl(c("Puromycin"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.HSP70 <- data.pepsum.Env.Stats[grepl(c("HSP70"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.HSP90 <- data.pepsum.Env.Stats[grepl(c("HSP90"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.Trifunctional <- data.pepsum.Env.Stats[grepl(c("Trifunctional"), data.pepsum.Env.Stats$Protein.Name),]


#Plot all pep1, pep2, and pep3 values for all proteins against PERCENT GROWTH
plot(data.pepsum.Env.Stats$Pep1 ~ data.pepsum.Env.Stats$Perc.Growth, main="All Proteins Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats$Pep2 ~ data.pepsum.Env.Stats$Perc.Growth, main="All Proteins Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats$Pep3 ~ data.pepsum.Env.Stats$Perc.Growth, main="All Proteins Pep3 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")

library(plotly)
Growth.Peps.plot <- plot_ly(data=data.pepsum.Env.Stats, x=~Perc.Growth, marker = list(size = 7)) %>%
  add_trace(y=~Pep1, color=~SITE, hovertext=~Protein.Name, showlegend = FALSE) %>%
  add_trace(y=~Pep1, color=~SITE, hovertext=~Protein.Name, showlegend = FALSE) %>%
  add_trace(y=~Pep3, color=~SITE, hovertext=~Protein.Name) %>%
  layout(title="All Peptides against Percent Groth",
         yaxis = list(title = 'Peptide Abundance'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Growth.Peps.plot), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/June2016-Growth-PepAbundance.html")





## BONEYARD

# Summary statistics by region

env.mean.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, mean)
env.mean.reg$stat <- "mean"
env.var.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, var)
env.var.reg$stat <- "var"
env.sd.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, sd)
env.sd.reg$stat <- "sd"
env.max.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, max)
env.max.reg$stat <- "max"
env.min.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, min)
env.min.reg$stat <- "min"
env.median.reg <- aggregate(value ~ Region*metric, Env.Data.Master.noOuts, median)
env.median.reg$stat <- "median"
env.stats.region <- rbind(env.mean.reg, env.var.reg, env.sd.reg, env.max.reg, env.min.reg, env.median.reg)
env.stats.region$comparison <- "Region"

# Write functions to calculate the % of data > 1 standard deviation from the mean, by REGION
Percent.sd.1.reg <- function(parameter, group) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+c))/d + sum(a<(b-c))/d)*100)
}
# Write functions to calculate the % of data > 2 standard deviation from the mean, by REGION
Percent.sd.2.reg <- function(parameter, group) {
  a <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% parameter & Env.Data.Master.noOuts$Region %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+2*c))/d + sum(a<(b-2*c))/d)*100)
}

# Calculate % each environmental parameter is > 1 sd away from mean, grouped by Region
SpH.1 <- c("South", "pH", Percent.sd.1.reg(parameter="pH", group="South"), "Percent.sd.1.reg", "Region")
NpH.1 <- c("North", "pH", Percent.sd.1.reg(parameter="pH", group="North"), "Percent.sd.1.reg", "Region")
SDO.1 <- c("South", "DO", Percent.sd.1.reg(parameter="DO", group="South"), "Percent.sd.1.reg", "Region")
NDO.1 <- c("North", "DO", Percent.sd.1.reg(parameter="DO", group="North"), "Percent.sd.1.reg", "Region")
SSal.1 <- c("South", "Salinity", Percent.sd.1.reg(parameter="Salinity", group="South"), "Percent.sd.1.reg", "Region")
NSal.1 <- c("North", "Salinity", Percent.sd.1.reg(parameter="Salinity", group="North"), "Percent.sd.1.reg", "Region")
STemp.1 <- c("South", "Temperature", Percent.sd.1.reg(parameter="Temperature", group="South"), "Percent.sd.1.reg", "Region")
NTemp.1 <- c("North", "Temperature", Percent.sd.1.reg(parameter="Temperature", group="North"), "Percent.sd.1.reg", "Region")

# Calculate % each environmental parameter is > 2 sd away from mean, grouped by Region
SpH.2 <- c("South", "pH", Percent.sd.2.reg(parameter="pH", group="South"), "Percent.sd.2.reg", "Region")
NpH.2 <- c("North", "pH", Percent.sd.2.reg(parameter="pH", group="North"), "Percent.sd.2.reg", "Region")
SDO.2 <- c("South", "DO", Percent.sd.2.reg(parameter="DO", group="South"), "Percent.sd.2.reg", "Region")
NDO.2 <- c("North", "DO", Percent.sd.2.reg(parameter="DO", group="North"), "Percent.sd.2.reg", "Region")
SSal.2 <- c("South", "Salinity", Percent.sd.2.reg(parameter="Salinity", group="South"), "Percent.sd.2.reg", "Region")
NSal.2 <- c("North", "Salinity", Percent.sd.2.reg(parameter="Salinity", group="North"), "Percent.sd.2.reg", "Region")
STemp.2 <- c("South", "Temperature", Percent.sd.2.reg(parameter="Temperature", group="South"), "Percent.sd.2.reg", "Region")
NTemp.2 <- c("North", "Temperature", Percent.sd.2.reg(parameter="Temperature", group="North"), "Percent.sd.2.reg", "Region")

# Add %> 1SD & 2SD data to stats dataframe
sd.percents.reg <- data.frame(rbind(SpH.1, NpH.1,SDO.1,NDO.1,SSal.1,NSal.1,STemp.1,NTemp.1,SpH.2,NpH.2,SDO.2,NDO.2,SSal.2,NSal.2,STemp.2,NTemp.2))
colnames(sd.percents.reg) <- c("Region", "metric", "value", "stat", "comparison")
env.stats.region <- rbind(env.stats.region, sd.percents.reg)
View(env.stats.region)

# Save the region-based stats dataframe as .csv
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/env-stats-by-region.csv", env.stats.region)

env.stats.region$comparison <- "reg"
env.stats.region$stat <- gsub("Percent.sd.1.reg", "sd.1", env.stats.region$stat)
env.stats.region$stat <- gsub("Percent.sd.2.reg", "sd.2", env.stats.region$stat)
env.stats.region$stats <- paste(env.stats.region$metric, env.stats.region$stat, env.stats.region$comparison, sep="-")
env.stats.region.wide <- cast(env.stats.region, Region~stats, value="value")

# merge env. stats with regional stats
data.pepsum.Env.Stats <- data.frame(merge(x=data.pepsum.Env.Stats, y=env.stats.region.wide, by.x = "REGION", by.y = "Region", all.x=TRUE, all.y=TRUE), stringsAsFactors = F)
