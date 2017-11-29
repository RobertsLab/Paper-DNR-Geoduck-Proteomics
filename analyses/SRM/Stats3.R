#Stats3 script: prep for regression models

# Pull summary statistics on environmental data

# By Region:
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

# Save the region-based stats dataframe as .csv
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/env-stats-by-region.csv", env.stats.region)

# By Probe Location
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

sd.percents.loc <- data.frame(rbind(PGEpH.1, PGBpH.1, FBEpH.1, FBBpH.1, WBEpH.1, WBBpH.1, CIEpH.1, CIBpH.1, PGEDO.1, PGBDO.1, FBEDO.1, FBBDO.1, WBEDO.1, WBBDO.1, CIEDO.1, CIBDO.1, PGESal.1, PGBSal.1, FBESal.1, FBBSal.1, WBESal.1, WBBSal.1, CIESal.1, CIBSal.1, PGETemp.1, PGBTemp.1, FBETemp.1, FBBTemp.1, WBETemp.1, WBBTemp.1, CIETemp.1, CIBTemp.1, PGEpH.2, PGBpH.2, FBEpH.2, FBBpH.2, WBEpH.2, WBBpH.2, CIEpH.2, CIBpH.2, PGEDO.2, PGBDO.2, FBEDO.2, FBBDO.2, WBEDO.2, WBBDO.2, CIEDO.2, CIBDO.2, PGESal.2, PGBSal.2, FBESal.2, FBBSal.2, WBESal.2, WBBSal.2, CIESal.2, CIBSal.2, PGETemp.2, PGBTemp.2, FBETemp.2, FBBTemp.2, WBETemp.2, WBBTemp.2, CIETemp.2, CIBTemp.2))
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

# Plot() protein peptides against each other to confirm linear correlation; equation should be ~1:1.  
View(data.melted.plus.pepsum)
data.melted.plus.pepsum$Pep <- "NA"
Protein2Peptide <- data.melted.plus.pepsum[!duplicated(data.melted.plus.pepsum$Peptide.Sequence),c("Peptide.Sequence", "Protein.Name")]
data.melted.plus.pepsum[grepl(Protein2Peptide[1,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Arach 1
data.melted.plus.pepsum[grepl(Protein2Peptide[2,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Arach 2
data.melted.plus.pepsum[grepl(Protein2Peptide[3,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #Arach 3
data.melted.plus.pepsum[grepl(Protein2Peptide[4,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Catalase 1
data.melted.plus.pepsum[grepl(Protein2Peptide[5,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Catalase 2
data.melted.plus.pepsum[grepl(Protein2Peptide[6,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Cytochrome 1
data.melted.plus.pepsum[grepl(Protein2Peptide[7,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Cytochrome 2
data.melted.plus.pepsum[grepl(Protein2Peptide[8,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #Cytochrome 3
data.melted.plus.pepsum[grepl(Protein2Peptide[9,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Glycogen 1
data.melted.plus.pepsum[grepl(Protein2Peptide[10,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Glycogen 2
data.melted.plus.pepsum[grepl(Protein2Peptide[11,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #Glycogen 3
data.melted.plus.pepsum[grepl(Protein2Peptide[12,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #HSP70 1
data.melted.plus.pepsum[grepl(Protein2Peptide[13,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #HSP70 2
data.melted.plus.pepsum[grepl(Protein2Peptide[14,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #HSP90-alpha 1
data.melted.plus.pepsum[grepl(Protein2Peptide[15,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #HSP90-alpha 2
data.melted.plus.pepsum[grepl(Protein2Peptide[16,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #HSP90-alpha 3
data.melted.plus.pepsum[grepl(Protein2Peptide[17,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #PDI 1
data.melted.plus.pepsum[grepl(Protein2Peptide[18,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #PDI 2
data.melted.plus.pepsum[grepl(Protein2Peptide[19,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Peroxiredoxin-1 1
data.melted.plus.pepsum[grepl(Protein2Peptide[20,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Puromycin-sensitive 1
data.melted.plus.pepsum[grepl(Protein2Peptide[21,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Puromycin-sensitive 2
data.melted.plus.pepsum[grepl(Protein2Peptide[22,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #Puromycin-sensitive 3
data.melted.plus.pepsum[grepl(Protein2Peptide[23,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Ras-related 1
data.melted.plus.pepsum[grepl(Protein2Peptide[24,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Sodium/potassium-transporting 1
data.melted.plus.pepsum[grepl(Protein2Peptide[25,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Sodium/potassium-transporting 2
data.melted.plus.pepsum[grepl(Protein2Peptide[26,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Superoxide 1
data.melted.plus.pepsum[grepl(Protein2Peptide[27,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "1" #Trifunctional 1
data.melted.plus.pepsum[grepl(Protein2Peptide[28,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "2" #Trifunctional 2
data.melted.plus.pepsum[grepl(Protein2Peptide[29,1], data.melted.plus.pepsum$Peptide.Sequence),][["Pep"]] <- "3" #Trifunctional 3
data.melted.plus.pepsum.wide <- cast(data.melted.plus.pepsum, Protein.Name+SAMPLE+SITE+TREATMENT+BOTH+REGION~Pep, value="lambda.t")

# Plot peptides within a protein against each other. Should be linearly correlated. summary() shows equation with R^2
# Peptide 1 x Peptide 2
pep12 <- lm(`1` ~ `2`, data=data.melted.plus.pepsum.wide)
summary(pep12)
with(data.melted.plus.pepsum.wide, plot(`1`,`2`))
abline(pep12)
# Peptide 1 x Peptide 3
pep13 <- lm(`1` ~ `3`, data=data.melted.plus.pepsum.wide)
summary(pep13)
with(data.melted.plus.pepsum.wide, plot(`1`,`3`))
abline(pep13)
# Peptide 2 x Peptide 3
pep23 <- lm(`2` ~ `3`, data=data.melted.plus.pepsum.wide)
summary(pep23)
with(data.melted.plus.pepsum.wide, plot(`2`,`3`))
abline(pep13)

# Select 1 of the peptides to develop the linear model. 
# I will select peptide #1. 

# Merge Environmental summary data with my peptide data

