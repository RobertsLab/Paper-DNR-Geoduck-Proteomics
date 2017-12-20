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

# Let's plot the low-tide scrubbed salinity data. We should see that extreme low readings (~0) are gone
library(plotly)
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


# Identify and remove outliers from pH, DO & Salinity data
# Use outliersKD script from https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/ to identify and remove outliers:
#Check & removal outliers for each variable for all locations. 
#--- I tried running this via loop, but it didn't work for me. I think b/c I need to remove or NOT remove outliers after each time I run the script. 
#--- I also tried running the function on the same dataframe, but subsetting. While I was able to plot boxplots/histograms and calculate statistics, as it didn't actually remove the outliers from the dataframe. 
#--- I did not scrub outliers from the temperature data, as it is less sensitive than the other parameters to exposure, and it a good representation of what the animals actually experienced. 
# Before running this script, I must split my data into separate dataframes for each environmental parameter / probe location.  I did try to run this by subsetting the dataframe within the function, but the script is not equipped to handle this. 

pH.FBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "FBB"),]
pH.FBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "FBE"),]
pH.PGB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "PGB"),]
# pH.PGE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "PGE"),] ==> NO DATA 
pH.CIB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "CIB"),]
pH.CIE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "CIE"),]
pH.WBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "WBB"),]
pH.WBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "pH" & Env.Data.Master$variable %in% "WBE"),]
DO.FBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "FBB"),]
DO.FBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "FBE"),]
DO.PGB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "PGB"),]
DO.PGE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "PGE"),]
DO.CIB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "CIB"),]
DO.CIE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "CIE"),]
DO.WBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "WBB"),]
DO.WBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "DO" & Env.Data.Master$variable %in% "WBE"),]
Salinity.FBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "FBB"),]
Salinity.FBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "FBE"),]
# Salinity.PGB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "PGB"),] ==> No Data
Salinity.PGE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "PGE"),]
Salinity.CIB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "CIB"),]
Salinity.CIE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "CIE"),]
Salinity.WBB.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "WBB"),]
Salinity.WBE.4outliers <- Env.Data.Master[which(Env.Data.Master$metric %in% "Salinity" & Env.Data.Master$variable %in% "WBE"),]

# Run the outlier identification and removal script. If you want to save each plot with custom name you edit the "outliersKD.R" script 
# source("https://goo.gl/4mthoF") 
source("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/references/outliersKD.R")
outlierKD(pH.FBB.4outliers, value)
yes
outlierKD(pH.FBE.4outliers, value)
yes
outlierKD(pH.PGB.4outliers, value)
yes
# pH.PGE.4outliers ==> No Data 
outlierKD(pH.CIB.4outliers, value)
yes
outlierKD(pH.CIE.4outliers, value)
yes
outlierKD(pH.WBB.4outliers, value)
yes
outlierKD(pH.WBE.4outliers, value)
yes
outlierKD(DO.FBB.4outliers, value)
yes
outlierKD(DO.FBE.4outliers, value) 
yes
outlierKD(DO.PGB.4outliers, value)
yes
outlierKD(DO.PGE.4outliers, value) 
yes
outlierKD(DO.CIB.4outliers, value) 
yes
outlierKD(DO.CIE.4outliers, value) 
yes
outlierKD(DO.WBB.4outliers, value) 
yes
outlierKD(DO.WBE.4outliers, value) 
yes
outlierKD(Salinity.FBB.4outliers, value) 
yes
outlierKD(Salinity.FBE.4outliers, value) 
yes
# Salinity.PGB.4outliers ==> No Data
outlierKD(Salinity.PGE.4outliers, value) 
yes
outlierKD(Salinity.CIB.4outliers, value) 
yes
outlierKD(Salinity.CIE.4outliers, value) 
yes
outlierKD(Salinity.WBB.4outliers, value) 
yes
outlierKD(Salinity.WBE.4outliers, value) 

# Combine the outlier-scrubbed dataframes back into one master, long-form dataframe
Env.Data.Master.noOuts <- rbind(pH.FBB.4outliers, pH.FBE.4outliers, pH.PGB.4outliers, pH.CIB.4outliers, pH.CIE.4outliers, pH.WBB.4outliers, pH.WBE.4outliers, DO.FBB.4outliers, DO.FBE.4outliers, DO.PGB.4outliers, DO.PGE.4outliers, DO.CIB.4outliers, DO.CIE.4outliers, DO.WBB.4outliers, DO.WBE.4outliers, Salinity.FBB.4outliers, Salinity.FBE.4outliers, Salinity.PGE.4outliers, Salinity.CIB.4outliers, Salinity.CIE.4outliers, Salinity.WBB.4outliers, Salinity.WBE.4outliers,Env.Data.Master[which(Env.Data.Master$metric %in% "Tide"),],Env.Data.Master[which(Env.Data.Master$metric %in% "Temperature"),])
View(Env.Data.Master.noOuts.wide)
# Remove the NA entries 
Env.Data.Master.noOuts <- Env.Data.Master.noOuts[which(!is.na(Env.Data.Master.noOuts$value)),]
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

# Run QQplots to assess normality, since I'm going to run ANOVAs and must have normal distribution
par(mfrow = c(2, 3))
for (i in 1:length(Env.parameters)) {
  qqnorm(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% Env.parameters[i]),"value"], main = Env.parameters[[i]],
         xlab = "Theoretical Quantiles", ylab = "Parameter Value", plot.it = TRUE)
  qqline(Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% Env.parameters[i]),"value"])
}

# Run tests on each probe location -------

# Assess equal variances via Bartlett Test of Homogeneity of Variances for each environmental parameter
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),])
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),])
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),])
bartlett.test(value~variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),])

# Is the data balanced? Result: FB-Bare has less data points b/c I removed G057 from the data, which is from FB-B. Not sure exactly how to incorporate that into the analysis... 
replications(value ~ metric*variable, data=Env.Data.Master.noOuts)
# Not balanced

install.packages("FSA")
library(FSA)
# Run Krusgal Wallis tests
summary(aov(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
TukeyHSD(aov(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "pH"),]))
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),]) 
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "DO"),], method="bonferroni")
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),])
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Temperature"),], method="bonferroni")
kruskal.test(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),]) 
dunnTest(value ~ variable, data=Env.Data.Master.noOuts[which(Env.Data.Master.noOuts$metric %in% "Salinity"),], method="bonferroni")

# Add North & South regions identifier to dataframe, since that's how the proteins differed 
Env.Data.Master.noOuts$Region <- Env.Data.Master.noOuts$variable
Env.Data.Master.noOuts$Region <- gsub("FBB|FBE|PGB|PGE|FB|PG", "North", Env.Data.Master.noOuts$Region)
Env.Data.Master.noOuts$Region <- gsub("WBB|WBE|CIB|CIE|WB|CI", "South", Env.Data.Master.noOuts$Region)
Env.Data.Master.noOuts$Region <- as.factor(Env.Data.Master.noOuts$Region)

aggregate(value ~ variable*metric, Env.Data.Master.noOuts, mean)

View(aggregate(DO ~ variable, Env.Data.Master.noOuts.wide, mean))
View(aggregate(pH ~ variable, Env.Data.Master.noOuts.wide, mean))

### BONEYARD
# Create box plots and ID outliers in environmental data
OutVals.e <- vector("list", length(Env.parameters))
names(OutVals) <- Env.parameters
par(mfrow = c(2, 3))
for (i in 1:length(Env.parameters)) {
  OutVals.e[[i]] = boxplot(Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"value"] ~ Env.Data.Master[which(Env.Data.Master$metric %in% Env.parameters[i]),"variable"], main = Env.parameters[[i]], xlab = "Location", ylab = "Env. Parameter Unit", type="p")$out
}
