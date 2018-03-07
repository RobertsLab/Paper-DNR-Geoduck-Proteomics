#### Import environmental data, plot all data to inspect for abnormalities 
# ==> Temperature (T)
# ==> Dissolved Oxygen (DO)
# ==> pH (temp-adjusted)
# ==> Salinity
# ==> Tide height (estim. from http://tbone.biol.sc.edu/).

library(reshape)
library(plotly) #open plotly program package
setwd("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics")
Env.Data <- data.frame(read.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/data/Environmental/EnvData-Master.csv", stringsAsFactors=F, header=T, na.strings = ""))

# pH 
pH.Data <- Env.Data[,c(1,grep("pH\\.", colnames(Env.Data)))]
names(pH.Data) <- c("DateTime", "WBE", "WBB", "SKE", "SKB", "PGE", "PGB", "CIE", "CIB", "FBE", "FBB")
pH.Data.melted <- melt(pH.Data, id="DateTime")
pH.Data.melted.noNA <- pH.Data.melted[which(!is.na(pH.Data.melted$value)),]
pH.series <- plot_ly(data = pH.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="pH across sites, 2016 DNR outplant",
         yaxis = list(title = 'pH (total scale)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(pH.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-pH-series.html")
pH.box <- plot_ly(data = pH.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>% 
  layout(title="pH across sites, 2016 DNR outplant",
         yaxis = list(title = 'pH (total scale)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(pH.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-pH-box.html")

# Dissolved Oxygen
DO.Data <- Env.Data[,c(1,grep("do\\.", colnames(Env.Data)))]
DO.Data.noOuts <- DO.Data[which(DO.Data$FBE < 50),] #Remove FBE values over 50
names(DO.Data) <- c("DateTime", "WBE", "WBB", "SKE", "SKB", "PGE", "PGB", "CIE", "CIB", "FBE", "FBB")
DO.Data.melted <- melt(DO.Data, id="DateTime")
DO.Data.melted.noNA <- DO.Data.melted[which(!is.na(DO.Data.melted$value) & DO.Data.melted$value > 0),]
DO.series <- plot_ly(data = DO.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%
  layout(title="Dissolved Oxygen across sites, 2016 DNR outplant)",
         yaxis = list(title = 'DO (mg/L)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(DO.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-series.html")
DO.box <- plot_ly(data = DO.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>%
  layout(title="Dissolved Oxygen across sites, 2016 DNR outplant)",
         yaxis = list(title = 'DO (mg/L)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(DO.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-box.html")

# Temperature
T.Data <- Env.Data[,c(1,grep("doT", colnames(Env.Data)))]
names(T.Data) <- c("DateTime", "WBE", "WBB", "SKE", "SKB", "PGE", "PGB", "CIE", "CIB", "FBE", "FBB")
T.Data.melted <- melt(T.Data, id="DateTime")
T.Data.melted.noNA <- T.Data.melted[which(!is.na(T.Data.melted$value)),]
T.series <- plot_ly(data = T.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%
  layout(title="Temperature (from DO sensor) across sites, 2016 DNR outplant",
         yaxis = list(title = 'Temperature (C)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(T.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Temp-series.html")
T.box <- plot_ly(data = T.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>%
  layout(title="Temperature (from DO sensor) across sites, 2016 DNR outplant",
         yaxis = list(title = 'Temperature (C)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(T.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Temp-box.html")

# Salinity
S.Data <- Env.Data[,c(1,grep("ctS", colnames(Env.Data)))]
names(S.Data) <- c("DateTime", "CIB", "CIE", "FBB", "FBE", "PGE", "SKE", "SKB", "WBB", "WBE")
S.Data.melted <- melt(S.Data, id="DateTime")
S.Data.melted$value <- as.numeric(levels(S.Data.melted$value))[S.Data.melted$value]
S.Data.melted.noNA <- S.Data.melted[which(!is.na(S.Data.melted$value)),]
S.series <- plot_ly(data = S.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%
  layout(title="Salinity across sites, 2016 DNR outplant",
         yaxis = list(title = 'Salinity'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(S.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-series.html")
S.box <- plot_ly(data = S.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>%
  layout(title="Salinity across sites, 2016 DNR outplant",
         yaxis = list(title = 'Salinity'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(S.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-box.html")

# Tidal height
Tide.Data <- Env.Data[,c(1,grep("Tide", colnames(Env.Data)))]
names(Tide.Data) <- c("DateTime", "FB", "PG", "CI", "WB")
Tide.Data.melted <- melt(Tide.Data, id="DateTime")
# Tide.Data.melted$value <- as.numeric(levels(Tide.Data.melted$value))[Tide.Data.melted$value]
Tide.Data.melted.noNA <- Tide.Data.melted[which(!is.na(Tide.Data.melted$value)),]
Tide.series <- plot_ly(data = Tide.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%
  layout(title="Tidal height across sites, 2016 DNR outplant",
         yaxis = list(title = 'Tidal Height'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Tide.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Tide-series.html")
Tide.box <- plot_ly(data = Tide.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>%
  layout(title="Tidal height across sites, 2016 DNR outplant",
         yaxis = list(title = 'Tidal Height'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Tide.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Tide-box.html")

#### Remove erroneous data points 

# Sampling Dates:
# ==> Case Inlet: July 19th; cut off env. data at -1.0 (estimate)- "never fully exposed"
# ==> Willapa Bay: July 20th-22nd (?); cut off env. data at 1.5 (estimate)- "fully exposed/dry"
# ==> Port Gamble: July 20th-22nd; cut off env. data at 0 (estimate)- "fully exposed/dry"
# ==> Fidalgo Bay: July 20th-22nd; cut off env. data at -1.25 (estimate)- "never fully exposed"
# ==> Skokomish: July 22nd

# Create master melted dataframe with all environmental data
# First, add another column "metric" to each env. data frame
pH.Data.melted.noNA$metric <- c("pH")
T.Data.melted.noNA$metric <- c("Temperature")
DO.Data.melted.noNA$metric <- c("DO")
S.Data.melted.noNA$metric <- c("Salinity")
Tide.Data.melted.noNA$metric <- c("Tide")

# Identify any time points in pH, DO & S that where probes were exposed. NOTE: likely need to update the threshold based on Micah/Alex's input.
Tide.sites <- as.factor(c("FB", "PG", "CI", "WB"))
Tide.exposed <- c(-1.25, 0, -1.0, 1.5) #tide depth at which probes are exposed (estimated)
submerged.times = list()
for (i in 1:length(Tide.sites)) {
  submerged.times[[i]] <- Tide.Data.melted.noNA[which(Tide.Data.melted.noNA$value>=Tide.exposed[i] & Tide.Data.melted.noNA$variable %in% Tide.sites[[i]]), ]
}

# Filter pH, DO & S at time points where probes were exposed
Tide.location <- as.factor(c("FBB", "FBE", "PGB", "PGE", "CIB", "CIE", "WBB", "WBE"))
J <- c(1,1,2,2,3,3,4,4)
submerged.data = list()
for (i in 1:length(Tide.location)) {
    submerged.data[[i]] <- rbind(pH.Data.melted.noNA[which(pH.Data.melted.noNA$DateTime %in% submerged.times[[J[i]]]$DateTime & pH.Data.melted.noNA$variable %in% Tide.location[[i]]), ], DO.Data.melted.noNA[which(DO.Data.melted.noNA$DateTime %in% submerged.times[[J[i]]]$DateTime & DO.Data.melted.noNA$variable %in% Tide.location[[i]]), ], S.Data.melted.noNA[which(S.Data.melted.noNA$DateTime %in% submerged.times[[J[i]]]$DateTime & S.Data.melted.noNA$variable %in% Tide.location[[i]]), ])
}

# Combine results from the previous for loop for pH, DO & Salinity, and the Temp and Tide data (unedited) into one master env. data frame (long form)
Env.Data.Master <- do.call(rbind, submerged.data)
Env.Data.Master <- rbind(Env.Data.Master, T.Data.melted.noNA, Tide.Data.melted.noNA) #this is a dataframe with env. data, screened for times when pH, DO & S probes were exposed
Env.Data.Master$metric <- as.factor(Env.Data.Master$metric)

# Remove DO data from FBE after 6/24 @ 08:40:00, as the probe clearly malfunctioned after that time. 
Env.Data.Master <- subset(Env.Data.Master, !(variable=="FBE" & metric=="DO" & DateTime > "06/24/16 08:40:00"))

# Remove Salinity data where probes clearly malfunctioned (identified via plots)
Env.Data.Master <- subset(Env.Data.Master, !((variable=="CIE" & metric=="Salinity") | (variable=="FBB" & metric=="Salinity" & DateTime > "07/03/16 09:50:00") | (variable=="WBB" & metric=="Salinity" & DateTime > "06/25/16 05:30:00")))

# Let's plot the low-tide scrubbed salinity data. We should see that extreme low readings (~0) are gone
Salinity.series.noLows <- plot_ly(data = subset(Env.Data.Master, metric=="Salinity"), x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="Salinity across sites (low tides removed), 2016 DNR outplant",
         yaxis = list(title = 'Salinity (ppt)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Salinity.series.noLows), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-series-noLowTides.html")
Salinity.box.noLows <- plot_ly(data = subset(Env.Data.Master, metric=="Salinity"), x = ~variable, y = ~value, type="box", color=~variable) %>% 
  layout(title="Salinity across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'Salinity (ppt)'),
         legend = list(x=.95, y=.2))
htmlwidgets::saveWidget(as_widget(Salinity.box.noLows), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-box-noLowTides.html")


# Identify and remove outliers from pH, DO & Salinity data. Apply Tukey's method of removing outlying values, where values outside the inner fence removed: 
Env.Data.Master.noOuts <- Env.Data.Master

# pH Data
for(i in 1:length(Tide.location)) { #For individual site data
  IQR <- quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] - quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[2]
  upperBound <- as.numeric(quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] + 1.5*IQR) #calculate upper bound
  lowerBound <- as.numeric(quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[2] - 1.5*IQR) #calculate lower bound
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value > upperBound), "value"] <- NA #replace values higher than upper bound with NA
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value < lowerBound), "value"] <- NA #replace values lower than lower bound with NA
}

# DO Data
for(i in 1:length(Tide.location)) { #For individual site data
  IQR <- quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] - quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[2]
  upperBound <- as.numeric(quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] + 1.5*IQR) #replace values higher than upper bound with NA
  lowerBound <- 0 #DO cannot be less than 0
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value > upperBound), "value"] <- NA
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value < lowerBound), "value"] <- NA
}

# Salinity Data
for(i in 1:length(Tide.location)) { #For individual site data
  IQR <- quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] - quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[2]
  upperBound <- as.numeric(quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[4] + 1.5*IQR) #calculate upper bound
  lowerBound <- as.numeric(quantile(Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% Tide.location[i]),"value"], na.rm=TRUE)[2] - 1.5*IQR) #calculate lower bound
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value > upperBound), "value"] <- NA #replace values higher than upper bound with NA
  Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$variable %in% Tide.location[i] & Env.Data.Master.noOuts$value < lowerBound), "value"] <- NA #replace values lower than lower bound with NA
}

# Remove the NA entries 
Env.Data.Master.noOuts <- Env.Data.Master.noOuts[which(!is.na(Env.Data.Master.noOuts$value)),]

# Remove SK entries
Env.Data.Master.noOuts <- subset(Env.Data.Master.noOuts, variable!="SKE")
Env.Data.Master.noOuts <- subset(Env.Data.Master.noOuts, variable!="SKB")

# Save this outlier-scrubbed dataset as .csv
write.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/EnvData-Melted-NoOutliers.csv",Env.Data.Master.noOuts, col.names = T, row.names=F)

# Let's plot the outlier-scrubbed data , to check it out after outliers have been removed.
# ==> pH
pH.series.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="pH"), x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="pH across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'pH (total scale)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(pH.series.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-pH-series-noOutliers.html")
pH.box.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="pH"), x = ~variable, y = ~value, type="box", color=~variable) %>% 
  layout(title="pH across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'pH (total scale)'),
         legend = list(x=.97, y=.98))
htmlwidgets::saveWidget(as_widget(pH.box.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-pH-box-noOutliers.html")

# ==> Dissolved Oxygen
# Let's plot the data again, to check it out after outliers have been removed.
DO.series.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="DO"), x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="DO across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'DO (mg/L)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(DO.series.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-series-noOutliers.html")
DO.box.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="DO"), x = ~variable, y = ~value, type="box", color=~variable) %>% 
  layout(title="DO across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'DO (total scale)'),
         legend = list(x=.95, y=.97))
htmlwidgets::saveWidget(as_widget(DO.box.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-box-noOutliers.html")

# ==> Salinity
# Let's plot the data again, to check it out after outliers have been removed.
Salinity.series.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="Salinity"), x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%  #generate plotly plot
  layout(title="Salinity across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'Salinity (ppt)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(Salinity.series.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-series-noOutliers.html")
Salinity.box.noOuts <- plot_ly(data = subset(Env.Data.Master.noOuts, metric=="Salinity"), x = ~variable, y = ~value, type="box", color=~variable) %>% 
  layout(title="Salinity across sites (outliers removed), 2016 DNR outplant",
         yaxis = list(title = 'Salinity (ppt)'),
         legend = list(x=.95, y=.2))
htmlwidgets::saveWidget(as_widget(Salinity.box.noOuts), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-Salinity-box-noOutliers.html")

# Run QQplots to assess normality on raw data
par(mfrow = c(2, 3))
for (i in 1:length(Env.parameters)) {
  qqnorm(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% Env.parameters[i]),"value"], main = Env.parameters[[i]],
         xlab = "Theoretical Quantiles", ylab = "Parameter Value", plot.it = TRUE)
  qqline(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% Env.parameters[i]),"value"])
}

# Add Habitat identifier to dataframe, since some environmental parameters did not differ between habitat within sites
Env.Data.Master.noOuts$Habitat <- Env.Data.Master.noOuts$variable
Env.Data.Master.noOuts$Habitat <- gsub("FBB|PGB|WBB|CIB", "Bare", Env.Data.Master.noOuts$Habitat)
Env.Data.Master.noOuts$Habitat <- gsub("FBE|PGE|WBE|CIE", "Eelgrass", Env.Data.Master.noOuts$Habitat)
Env.Data.Master.noOuts$Habitat <- gsub("FB|PG|WB|CI", "Nope", Env.Data.Master.noOuts$Habitat)
Env.Data.Master.noOuts[Env.Data.Master.noOuts == "Nope"] <- NA
Env.Data.Master.noOuts$Habitat <- as.factor(Env.Data.Master.noOuts$Habitat)

# Add Site identifier to dataframe, since some environmental parameters did not differ between habitat within sites
Env.Data.Master.noOuts$Site <- Env.Data.Master.noOuts$variable
Env.Data.Master.noOuts$Site <- gsub("FBB|FBE|FB", "FB", Env.Data.Master.noOuts$Site)
Env.Data.Master.noOuts$Site <- gsub("PGB|PGE|PG", "PG", Env.Data.Master.noOuts$Site)
Env.Data.Master.noOuts$Site <- gsub("WBB|WBE|WB", "WB", Env.Data.Master.noOuts$Site)
Env.Data.Master.noOuts$Site <- gsub("CIB|CIE|CI", "CI", Env.Data.Master.noOuts$Site)
Env.Data.Master.noOuts$Site <- as.factor(Env.Data.Master.noOuts$Site)

# Add North & South regions identifier to dataframe, since that's how the proteins differed 
Env.Data.Master.noOuts$Region <- Env.Data.Master.noOuts$variable
Env.Data.Master.noOuts$Region <- gsub("FBB|FBE|PGB|PGE|FB|PG", "North", Env.Data.Master.noOuts$Region)
Env.Data.Master.noOuts$Region <- gsub("WBB|WBE|CIB|CIE|WB|CI", "South", Env.Data.Master.noOuts$Region)
Env.Data.Master.noOuts$Region <- as.factor(Env.Data.Master.noOuts$Region)

# ENVIRONMENTAL DATA STATISTICS
# 0. Assume unbalanced data sets across teh board due to outlier scrubbing
# 1. Assess equal variances via Bartlett Test of Homogeneity of Variances for each environmental parameter

# PH
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "FB"),]) #unequal variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "CI"),]) #unequal variance (but not as sig.)
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "WB"),]) #unequal variance 

# DISSOLVED OXYGEN
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "FB"),]) #unequal variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "PG"),]) #unequal variance 
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "CI"),]) #unequal variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "WB"),]) #unequal variance 

# TEMPERATURE
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "FB"),]) #EQUAL variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "PG"),]) #unequal variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "CI"),]) #EQUAL variance
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "WB"),]) #unequal variance 

# SALINITY
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$Site %in% "FB"),]) #EQUAL variance (marginal...)
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$Site %in% "WB"),]) #unequal variance 

install.packages("FSA")
library(FSA)

# 2. RUN STATS TO COMPARE DATA BETWEEN HABITATS, WITHIN BAYS
# STAT SELECTED BASED ON UNEQUAL VARIANCES, PARAMETRIC OR NOT  

# PH = normal distribution, unequal variance 
t.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "FB"),]$value, g=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "FB"),]$Habitat,var.equal=FALSE)
t.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "CI"),]$value, g=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "FB"),]$Habitat,var.equal=FALSE)
t.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "WB"),]$value, g=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$Site %in% "FB"),]$Habitat,var.equal=FALSE)

# DISSOLVED OXYGEN: nonparametric, unequal variance = Kolmogorov-Smirnov Test
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "FB" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "FB" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "PG" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "PG" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "CI" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "CI" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 

# TEMPERATURE: nonparametric, equal variance = krusgal-wallis; unequal=kolmogorov-smirnov
kruskal.test(value ~ Habitat, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "FB"),]) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "PG" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "PG" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 
kruskal.test(value ~ Habitat, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "CI"),]) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 

# SALINITY
kruskal.test(value ~ Habitat, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$Site %in% "FB"),]) 
ks.test(x=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Bare"),]$value, y=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity" & Env.Data.Master.noOuts$Site %in% "WB" & Env.Data.Master.noOuts$Habitat %in% "Eelgrass"),]$value) 

#### If it's OK to assume equal variance ... 

# Run Stats on Environmental Data - if we can assume equal variance ... 
summary(aov(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
TukeyHSD(aov(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),]) 
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),])
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),]) 
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")

# Run stats to compare environmental data between bays, 
TukeyHSD(aov(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
dunnTest(value ~ Site, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")

# Run stats to compare environmental data between north vs. south, 
TukeyHSD(aov(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
dunnTest(value ~ Region, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")

# PLAY AROUND with time series data 
# PH

test <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "FBB"),]$value
test1 <- ts(test, frequency=24*60/10)
test2 <- decompose(test1)
plot(test2)

test3 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "FBE"),]$value
test4 <- ts(test3, frequency=24*60/10)
test5 <- decompose(test4)
plot(test5)

test6 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "PGB"),]$value
test7 <- ts(test6, frequency=24*60/10)
test8 <- decompose(test7)
plot(test8)

test9 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "CIB"),]$value
test10 <- ts(test9, frequency=24*60/10)
test11 <- decompose(test10)
plot(test11)

test12 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "CIE"),]$value
test13<- ts(test12, frequency=24*60/10)
test14 <- decompose(test13)
plot(test14)

test15 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "WBB"),]$value
test16 <- ts(test15, frequency=24*60/10)
test17 <- decompose(test16)
plot(test17)

test18 <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH" & Env.Data.Master.noOuts$variable %in% "WBE"),]$value
test19<- ts(test18, frequency=24*60/10)
test20 <- decompose(test19)
plot(test20)


# CORRELATION PLOTS ! 

# Isolate dataframes per environmental variable
pH <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]
DO <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),]
Temperature <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),]
Salinity <- Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),]

# Make dataframe wide format
pH.wide <- dcast(pH, DateTime ~ variable)
DO.wide <- dcast(DO, DateTime ~ variable)
Temperature.wide <- dcast(Temperature, DateTime ~ variable)
Salinity.wide <- dcast(Salinity, DateTime ~ variable)

# Correlation plots, between habitats within bays 

# pH Correlation Plots
ggscatter(data=pH.wide, x="FBE", y="FBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="FBE pH", ylab="FBB pH", main="Fidalgo Bay pH Correlation Plot")
ggscatter(data=pH.wide, x="WBE", y="WBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="WBE pH", ylab="WBB pH", main="Willapa Bay pH Correlation Plot")
ggscatter(data=pH.wide, x="CIE", y="CIB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="CIE pH", ylab="CIB pH", main="Case Inlet pH Correlation Plot")

# DO Correlation Plots 
ggscatter(data=DO.wide, x="FBE", y="FBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="FBE DO", ylab="FBB DO", main="Fidalgo Bay DO Correlation Plot")
ggscatter(data=DO.wide, x="PGE", y="PGB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="PGE DO", ylab="PGB DO", main="Port Gamble Bay DO Correlation Plot")
ggscatter(data=DO.wide, x="WBE", y="WBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="WBE DO", ylab="WBB DO", main="Willapa Bay DO Correlation Plot")
ggscatter(data=DO.wide, x="CIE", y="CIB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="CIE DO", ylab="CIB DO", main="Case Inlet DO Correlation Plot")

# Temperature Correlation Plots 
ggscatter(data=Temperature.wide, x="FBE", y="FBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="FBE Temperature", ylab="FBB Temperature", main="Fidalgo Bay Temperature Correlation Plot")
ggscatter(data=Temperature.wide, x="PGE", y="PGB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="PGE Temperature", ylab="PGB Temperature", main="Port Gamble Bay Temperature Correlation Plot")
ggscatter(data=Temperature.wide, x="WBE", y="WBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="WBE Temperature", ylab="WBB Temperature", main="Willapa Bay Temperature Correlation Plot")
ggscatter(data=Temperature.wide, x="CIE", y="CIB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="CIE Temperature", ylab="CIB Temperature", main="Case Inlet Temperature Correlation Plot")

# Salinity Correlation Plots 
ggscatter(data=Salinity.wide, x="FBE", y="FBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="FBE Salinity", ylab="FBB Salinity", main="Fidalgo Bay Salinity Correlation Plot")
ggscatter(data=Salinity.wide, x="WBE", y="WBB", add="reg.line", conf.int = TRUE, cor.coef=TRUE, cor.method="pearson", xlab="WBE Salinity", ylab="WBB Salinity", main="Willapa Bay Salinity Correlation Plot")

# Random stats
# Survival statistics
do.variance <- data.frame(rbind(c(22.5, 11.7, 7.2, 2.0), c(31.4, 13.3, 4.7, 2.3)), row.names = c("EELGRASS", "BARE"), stringsAsFactors = F)
names(do.variance) <- c("FB","PG","CI","WB")
chisq.test(t(do.variance), simulate.p.value = TRUE) #not significant ?!
