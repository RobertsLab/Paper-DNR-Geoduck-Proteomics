#Stats3 script: prep environmental and protein data for regression models
library(ggplot2)

Env.Data.Master.noOuts.noTide <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric != "Tide"),]

# Pull summary statistics on environmental data by Probe Location
env.mean.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, mean)
env.mean.loc$stat <- "mean"
env.var.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, var)
env.var.loc$stat <- "var"
env.sd.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, sd)
env.sd.loc$stat <- "sd"
env.max.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, max)
env.max.loc$stat <- "max"
env.min.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, min)
env.min.loc$stat <- "min"
env.median.loc <- aggregate(value ~ variable*metric, Env.Data.Master.noOuts.noTide, median)
env.median.loc$stat <- "median"
env.stats.location <- rbind(env.mean.loc, env.var.loc, env.sd.loc, env.max.loc, env.min.loc, env.median.loc)
env.stats.location$comparison <- "variable"
summary(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),])

aggregate(value ~ Habitat*metric, Env.Data.Master.noOuts, mean)
aggregate(value ~ Habitat*metric, Env.Data.Master.noOuts, sd)
aggregate(value ~ Habitat*metric, Env.Data.Master.noOuts, min)
aggregate(value ~ Habitat*metric, Env.Data.Master.noOuts, max)

# Write functions to calculate the % of data > 1 standard deviation from the mean, by REGION
Percent.sd.1.loc <- function(parameter, group) {
  a <- Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+c))/d + sum(a<(b-c))/d)*100)
}

# Write functions to calculate the % of data > 2 standard deviation from the mean, by REGION
Percent.sd.2.loc <- function(parameter, group) {
  a <- Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"]
  b <- mean(Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"])
  c <- sd(Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"])
  d <- length(a)
  return((sum(a>(b+2*c))/d + sum(a<(b-2*c))/d)*100)
}

Percent.high <- function(group, parameter, threshold) {
  a <- Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"]
  b <- length(a[a>threshold])/length(a)
  return((b))
}
Percent.low <- function(group, parameter, threshold) {
  a <- Env.Data.Master.noOuts.noTide[which(Env.Data.Master.noOuts.noTide$metric %in% parameter & Env.Data.Master.noOuts.noTide$variable %in% group),"value"]
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

sd.percents.loc <- data.frame(rbind(PGE.pH.high,PGB.pH.high,FBE.pH.high,FBB.pH.high,WBE.pH.high, WBB.pH.high,CIE.pH.high,CIB.pH.high,PGE.pH.low,PGB.pH.low,FBE.pH.low,FBB.pH.low,WBE.pH.low, WBB.pH.low,CIE.pH.low,CIB.pH.low,PGEpH.1, PGBpH.1, FBEpH.1, FBBpH.1, WBEpH.1, WBBpH.1, CIEpH.1, CIBpH.1, PGEDO.1, PGBDO.1, FBEDO.1, FBBDO.1, WBEDO.1, WBBDO.1, CIEDO.1, CIBDO.1, PGESal.1, PGBSal.1, FBESal.1, FBBSal.1, WBESal.1, WBBSal.1, CIESal.1, CIBSal.1, PGETemp.1, PGBTemp.1, FBETemp.1, FBBTemp.1, WBETemp.1, WBBTemp.1, CIETemp.1, CIBTemp.1, PGEpH.2, PGBpH.2, FBEpH.2, FBBpH.2, WBEpH.2, WBBpH.2, CIEpH.2, CIBpH.2, PGEDO.2, PGBDO.2, FBEDO.2, FBBDO.2, WBEDO.2, WBBDO.2, CIEDO.2, CIBDO.2, PGESal.2, PGBSal.2, FBESal.2, FBBSal.2, WBESal.2, WBBSal.2, CIESal.2, CIBSal.2, PGETemp.2, PGBTemp.2, FBETemp.2, FBBTemp.2, WBETemp.2, WBBTemp.2, CIETemp.2, CIBTemp.2), stringsAsFactors = FALSE)
colnames(sd.percents.loc) <- c("variable", "metric", "value", "stat", "comparison")
env.stats.location <- rbind(env.stats.location, sd.percents.loc)

# Save the region-based stats dataframe as .csv
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/env-stats-by-location.csv", env.stats.location)

# Plot environmental variables: are they linearly correlated?
# First, I need convert dataframe to wide form
library(reshape)
Env.Data.Master.noOuts.noTide.wide <- cast(Env.Data.Master.noOuts.noTide, DateTime+variable ~ metric, value="value")

par(mfrow = c(3, 2))
plot(Env.Data.Master.noOuts.noTide.wide$Temperature ~ Env.Data.Master.noOuts.noTide.wide$DO, xlab="DO", ylab="Temperature", main="Temp~DO")
plot(Env.Data.Master.noOuts.noTide.wide$Temperature ~ Env.Data.Master.noOuts.noTide.wide$pH, xlab="pH", ylab="Temperature", main="Temp~pH")
plot(Env.Data.Master.noOuts.noTide.wide$Temperature ~ Env.Data.Master.noOuts.noTide.wide$Salinity, xlab="Salinity", ylab="Temperature", main="Temp~Salinity")
plot(Env.Data.Master.noOuts.noTide.wide$DO ~ Env.Data.Master.noOuts.noTide.wide$pH, xlab="pH", ylab="DO", main="DO~pH")
plot(Env.Data.Master.noOuts.noTide.wide$DO ~ Env.Data.Master.noOuts.noTide.wide$Salinity, xlab="Salinity", ylab="DO", main="DO~Salinity")
plot(Env.Data.Master.noOuts.noTide.wide$pH ~ Env.Data.Master.noOuts.noTide.wide$Salinity, xlab="Salinity", ylab="pH", main="pH~Salinity")
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
data.pepsum.Env.Stats <- data.frame(merge(x=data.melted.plus.pepsum.wide, y=env.stats.location.wide, by.x = "BOTH", by.y = "variable", all.x=TRUE, all.y=TRUE), stringsAsFactors = FALSE)
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/SRM/data-pepsum-Env-Stats.csv", data.pepsum.Env.Stats)

# Import growth data 
Growth <- read.csv("../../data/GeoduckGrowth.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = "NA")

# Get growth summary stats
Growth$Both <- paste(Growth$Site, Growth$Habitat, sep="-")
mean(Growth$AvgIShell)
sd(Growth$AvgIShell)

# Run stats: 

# Generate growth boxplot

Growth4plot <- Growth[!grepl(c("SK"), Growth$Site),] %>% # Remove SK data
  group_by_at(vars(Both, Site)) %>%   # the grouping variable
  summarise(mean = mean(Perc.Growth),  # calculates the mean of each group
            sd = sd(Perc.Growth), # calculates the standard deviation of each group
            n = n(),  # calculates the sample size per group
            SE = sd(Perc.Growth)/sqrt(n())) # calculates the standard error of each group

### IMPORTANT ### 
# Barplots: mean growth, error bars = standard error. 
marker1 = c("sienna1", "goldenrod1", "steelblue2", "royalblue3")
group.colors <- c(WB = "sienna1", CI = "goldenrod1", PG ="steelblue2",  FB = "royalblue3")

mean(Growth[grepl("CI-B", Growth$Both),7:11])
subset(Growth, Both == "CI-B")[1,7:11]
subset(Growth, Both == "CI-B")[1,7:11]

Growth4plot$Both<-factor(Growth4plot$Both, levels=c("WB-B", "WB-E",  "CI-B", "CI-E", "PG-B", "PG-E", "FB-B", "FB-E"))
Growth4plot$Site<-factor(Growth4plot$Site, levels=c("WB", "CI", "PG", "FB"))

## Need to figure out how to make eelgrass/bare pattern or colors different. 

ggplot(Growth4plot, aes(x=as.factor(Both), y=100*mean, fill=Site)) +
  geom_bar(position=position_dodge(), stat="identity") + xlab("") + ylab("Mean Growth (%)") +
  geom_errorbar(aes(ymin=100*(mean-SE), ymax=100*(mean+SE)), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values=group.colors, labels=c("Willapa Bay", "Case Inlet", "Port Gamble Bay", "Fidalgo Bay")) +
  theme_light() + theme(plot.title = element_text(size=19, face="bold"), axis.text.y=element_text(size=14, angle=45, face="bold"), axis.title=element_text(size=16,face="bold"), legend.position = "none", panel.background = element_blank(), axis.text.x=element_blank()) + ggtitle("Growth by Site") + guides(fill = guide_legend(reverse = TRUE))

# Revised barplot for size, not growth for NSA
Size <- read.csv("../../data/GeoduckGrowth-for-NSA-2.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = "NA")
#marker2 = c("gray", "sienna1", "goldenrod1", "steelblue2", "royalblue3")
group.colors2 <- c(INITIAL = "gray", WB = "sienna1", CI = "goldenrod1", PG ="steelblue2",  FB = "royalblue3")
Size$BOTH<-factor(Size$BOTH, levels=c("INITIAL", "WB-B", "WB-E",  "CI-B", "CI-E", "PG-B", "PG-E", "FB-B", "FB-E"))
Size$SITE<-factor(Size$SITE, levels=c("INITIAL", "WB", "CI", "PG", "FB"))

# Size compared to initial
ggplot(Size, aes(x=as.factor(BOTH), y=Mean.Final, fill=SITE)) +
  geom_bar(position=position_dodge(), stat="identity") + xlab("") + ylab("Mean Length (mm)") +
  geom_errorbar(aes(ymin=Mean.Final-SD.Final, ymax=Mean.Final+SD.Final), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values=group.colors2, labels=c("Initial", "Willapa Bay", "Case Inlet", "Port Gamble Bay", "Fidalgo Bay")) +
  theme_light() + theme(plot.title = element_text(size=19, face="bold"), axis.text.y=element_text(size=14, angle=45, face="bold"), axis.title=element_text(size=16,face="bold"), legend.position = "none", panel.background = element_blank(), axis.text.x=element_blank()) + ggtitle("Growth by Site") + guides(fill = guide_legend(reverse = TRUE))

# % Relative Size
ggplot(data=subset(Size, SITE!="INITIAL"), aes(x=as.factor(BOTH), y=Mean.Final-14.63, fill=SITE)) +
  geom_bar(position=position_dodge(), stat="identity") + xlab("") + ylab("Relative Mean Length (mm)") +
  geom_errorbar(aes(ymin=Mean.Final-14.63-SD.Final, ymax=Mean.Final-14.63+SD.Final), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values=group.colors2, labels=c("Initial", "Willapa Bay", "Case Inlet", "Port Gamble Bay", "Fidalgo Bay")) +
  theme_light() + theme(plot.title = element_text(size=19, face="bold"), axis.text.y=element_text(size=14, angle=45, face="bold"), axis.title=element_text(size=16,face="bold"), legend.position = "none", panel.background = element_blank(), axis.text.x=element_blank()) + ggtitle("Growth by Site") + guides(fill = guide_legend(reverse = TRUE))

# Check out if growth significantly differed between habitat, site, and both
par(mfrow=c(1,1))
hist(subset(Growth, Site != "SK")$Perc.Growth) #normal dist? 
growth.aov <-aov(Perc.Growth ~ Site*Habitat*Both, data=subset(Growth, Site != "SK"))
growth.res <- growth.aov$residuals
hist(growth.res, main="Histogram of Growth ANOVA Residuals", xlab="Residuals") #normal distribution = check
library(car)
leveneTest(Perc.Growth*100 ~ Site*Habitat*Both, data=Growth) #unequal variance, BUT ANOVA p-values are so low that I am confident in the results
summary(growth.aov)
TukeyHSD(growth.aov)
summary(growth.aov <-aov(Perc.Growth ~ Habitat, data=subset(Growth, Site != "SK")))


count(Growth[Growth$Site!="SK",], Site) #not balanced, anova results below not valid. 
anova(lm(Perc.Growth ~ Site + Site/Habitat/Exclosure, data=Growth[Growth$Site!="SK",]))
0.023764/0.010545 #F-value for Exclosure 
0.015525/0.023764 #F-value for Habitat 
0.252493/0.015525 #F-value for Site 

anova(lm(Perc.Growth ~ Site*Habitat*Exclosure, data=Growth[Growth$Site!="SK",]))

# Run analysis 

#Copied and saved results in a .txt file 

data.pepsum.Env.Stats <- data.frame(merge(x=data.pepsum.Env.Stats, y=Growth[,c("Perc.Growth", "PRVial")], by.x="SAMPLE", by.y="PRVial", all.x=T), stringsAsFactors = FALSE)
data.melted.plus.pepsum.wide <- data.frame(merge(x=data.melted.plus.pepsum.wide, y=Growth[,c("Perc.Growth", "PRVial")], by.x="SAMPLE", by.y="PRVial", all.x=T))

# Replace NaN strings with official "NA" designation & convert environmental stats to numeric
data.pepsum.Env.Stats[data.pepsum.Env.Stats == "NaN"] <- NA 
for (i in 7:44){
  data.pepsum.Env.Stats[i] <- as.numeric(as.character(unlist(data.pepsum.Env.Stats[i])), na.action=na.omit)
}

data.pepsum.Env.Stats$test <- data.pepsum.Env.Stats$SITE
data.pepsum.Env.Stats$test  <- gsub("FB|PG|WB", "Non-CI", data.pepsum.Env.Stats$test)
data.pepsum.Env.Stats$test  <- as.factor(data.pepsum.Env.Stats$test)

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

# Isolate data related to diff. expressed proteins: 
# ====> Puromycin-sensitive aminopeptidase
# ====> HSP90-alpha
# ====> Trifunctional enzyme subunit

data.pepsum.Env.Stats.HSP90 <- data.pepsum.Env.Stats[grepl(c("HSP90"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.Puromycin <- data.pepsum.Env.Stats[grepl(c("Puromycin"), data.pepsum.Env.Stats$Protein.Name),]
data.pepsum.Env.Stats.Trifunctional <- data.pepsum.Env.Stats[grepl(c("Trifunctional"), data.pepsum.Env.Stats$Protein.Name),]

# Survival statistics
Survival <- read.csv("../../data/Geoduck-Survival.csv", header=TRUE, stringsAsFactors = TRUE)
chisq.test(Survival$Survival ~ Survival$Both) #not significant  
anova(lm(Survival ~ Site + Site/Both, data=Survival)) #No difference in survival between site or habitat
0.011667/0.076667 #no difference in survival between habitats, nested within sites 
0.117222/0.011667 #yes, sign. difference in survival between sites 
pf(10.04731, 3, 4)

### Need to figure out how to run chi-square test on all my data here. 
Survival4plot <- Survival %>% # Remove SK data
  group_by_at(vars(Both, Site)) %>%   # the grouping variable
  summarise(mean = mean(Survival),  # calculates the mean of each group
            sd = sd(Survival), # calculates the standard deviation of each group
            n = n(),  # calculates the sample size per group
            SE = sd(Survival)/sqrt(n())) # calculates the standard error of each group

Survival4plot$Both<-factor(Survival4plot$Both, levels=c("WB-B", "WB-E",  "CI-B", "CI-E", "PG-B", "PG-E", "FB-B", "FB-E"))
Survival4plot$Site<-factor(Survival4plot$Site, levels=c("WB", "CI", "PG", "FB"))

## Need to figure out how to make eelgrass/bare pattern or colors different. 

ggplot(Survival4plot, aes(x=as.factor(Both), y=100*mean, fill=Site)) +
  geom_bar(position=position_dodge(), stat="identity") + xlab("") + ylab("Mean Sruvival (%)") +
  geom_errorbar(aes(ymin=100*(mean-SE), ymax=100*(mean+SE)), width=.2,position=position_dodge(.9)) +
  scale_fill_manual(values=group.colors, labels=c("Willapa Bay", "Case Inlet", "Port Gamble Bay", "Fidalgo Bay")) +
  theme_light() + theme(plot.title = element_text(size=19, face="bold"), axis.text.y=element_text(size=14, angle=45, face="bold"), axis.title=element_text(size=16,face="bold"), legend.position = "none", panel.background = element_blank(), axis.text.x=element_blank()) + ggtitle("Survival by Site") + guides(fill = guide_legend(reverse = TRUE))

# Try making NMDS plot with env. summary stats ... use env.stats.location.wide 
env.stats.location.wide.noNA <- env.stats.location.wide
env.stats.location.wide.noNA[env.stats.location.wide.noNA == "NaN"] <- NA
for (i in 2:ncol(env.stats.location.wide.noNA)) {
  env.stats.location.wide.noNA[i] <- as.numeric(as.character(unlist(env.stats.location.wide.noNA[i])), na.action=na.omit)
}
env.stats.location.wide.noNA[is.na(env.stats.location.wide.noNA)] <- 0
rownames(env.stats.location.wide.noNA) <- env.stats.location.wide.noNA[,1]
env.stats.location.wide.noNA <- env.stats.location.wide.noNA[,-1]

library(vegan)
Env.stats.nmds <- metaMDS(env.stats.location.wide.noNA, distance = 'bray', k = 2, trymax = 1000, autotransform = FALSE) #Make MDS dissimilarity matrix
stressplot(Env.stats.nmds) #Make NMDS stressplot  
plot(Env.stats.nmds) #Make NMDS plot; black circle = site, red ticks = env. variable
Env.stats.nmds.samples <- scores(Env.stats.nmds, display = "sites")
plot(Env.stats.nmds.samples)
Env.stats.nmds.samples
