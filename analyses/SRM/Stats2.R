# Statistical analysis on environmental data, which include the following from 2016-06-01 -> 2016-07-22 at 10-minute intervals: 
# ==> Temperature (T)
# ==> Dissolved Oxygen (DO)
# ==> pH (temp-adjusted)
# ==> Salinity
# ==> Tide height (estim. from http://tbone.biol.sc.edu/).
# Data was plotted in "Env-Data-Plots.R" script
# Original dataframe for pH, DO & T is "Env.Data"
# Isolated parameters are in the following dataframes: 
# ==> pH.Data, pH.Data.melted.noNA
# ==> DO.Data, DO.Data.melted.noNA
# ==> T.Data, T.Data.melted.noNA

# Sampling Dates:
# ==> Case Inlet: July 19th; cut off env. data at -1.0 (estimate)- "never fully exposed"
# ==> Willapa Bay: July 20th-22nd (?); cut off env. data at -0.25 (estimate)- "fully exposed/dry"
# ==> Port Gamble: July 20th-22nd  (?); cut off env. data at -1.0 (estimate)- "fully exposed/dry"
# ==> Fidalgo Bay: July 20th-22nd (?); cut off env. data at -1.25 (estimate)- "never fully exposed"
# ==> Skokomish: July 22nd (?)

# Create master melted dataframe with all environmental data
# First, add another column "metric" to each env. data frame
pH.Data.melted.noNA$metric <- c("pH")
T.Data.melted.noNA$metric <- c("Temperature")
DO.Data.melted.noNA$metric <- c("DO")
S.Data.melted.noNA$metric <- c("Salinity")
Tide.Data.melted.noNA$metric <- c("Tide")

# Remove any time points in pH, DO & S that where probes were exposed. NOTE: likely need to update the threshold based on Micah/Alex's input.
Tide.sites <- as.factor(c("FB", "PG", "CI", "WB"))
Tide.exposed <- c(-1.25, -1.0, -1.0, -0.25) #tide depth at which probes are exposed (estimated)
submerged.times = list()
for (i in 1:length(Tide.sites)) {
  submerged.times[[i]] <- Tide.Data.melted.noNA[which(Tide.Data.melted.noNA$value>=Tide.exposed[i] & Tide.Data.melted.noNA$variable %in% Tide.sites[[i]]), ]
}

# Filter only the env. variables
Tide.location <- as.factor(c("FBB", "FBE", "PGB", "PGE", "CIB", "CIE", "WBB", "WBE"))
submerged.data = list()
for (i in 1:length(Tide.location)) {
  for (j in c(1,1,2,2,3,3,4,4)) {
    submerged.data[[i]] <- rbind(pH.Data.melted.noNA[which(pH.Data.melted.noNA$DateTime %in% submerged.times[[j]]$DateTime & pH.Data.melted.noNA$variable %in% Tide.location[[i]]), ], DO.Data.melted.noNA[which(DO.Data.melted.noNA$DateTime %in% submerged.times[[j]]$DateTime & DO.Data.melted.noNA$variable %in% Tide.location[[i]]), ], S.Data.melted.noNA[which(S.Data.melted.noNA$DateTime %in% submerged.times[[j]]$DateTime & S.Data.melted.noNA$variable %in% Tide.location[[i]]), ])
  }
}
Env.Data.Master <- do.call(rbind, submerged.data)
Env.Data.Master <- rbind(Env.Data.Master, T.Data.melted.noNA, Tide.Data.melted.noNA) #this is a dataframe with env. data, screened for times when pH, DO & S probes were exposed
Env.Data.Master$metric <- as.factor(Env.Data.Master$metric)

# QQplots to assess normality 
Env.parameters <- unique(Env.Data.Master$metric)
par(mfrow = c(2, 3))
for (i in 1:length(Env.parameters)) {
  qqnorm(Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"value"], main = Env.parameters[[i]],
         xlab = "Theoretical Quantiles", ylab = "Parameter Value", plot.it = TRUE)
  qqline(Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"value"])
}

### Need to figure out how to filter environmental data. 

# Create box plots and ID outliers in environmental data
OutVals.e <- vector("list", length(Env.parameters))
names(OutVals) <- Env.parameters
par(mfrow = c(2, 3))
for (i in 1:length(Env.parameters)) {
  OutVals.e[[i]] = boxplot(Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"value"] ~ Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"variable"], main = Env.parameters[[i]], xlab = "Location", ylab = "Env. Parameter Unit", type="p")$out
}

# Generate a dataframe of outliers to inspect. If ANOVA results indicate differences in the peptides/locations included in the outliers list, then consider whether the outliers should be removed from the dataset.
outliers.e = list()
for (i in 1:length(Env.parameters)) {
    outliers.e[[i]] <- Env.Data.Master[Env.Data.Master$value %in% OutVals[[i]],] #THIS ISN'T WORKING - not specific enough either since there are so many values
}

### STOPPED HERE

Env.outliers  = do.call(rbind, outliers.e) #this is a dataframe with outliers, as determined by boxplots
View(Env.outliers)
