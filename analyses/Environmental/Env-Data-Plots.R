library(reshape)
library(plotly) #open plotly program package
setwd("~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics")
Env.Data <- data.frame(read.csv(file="~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/data/EnvDataSync.csv", stringsAsFactors=F, header=T, na.strings = ""))

# pH 
pH.Data <- Env.Data[-nrow(Env.Data),c(1,grep("pH\\.", colnames(Env.Data)))]
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
DO.Data <- Env.Data[-nrow(Env.Data),c(1,grep("do\\.", colnames(Env.Data)))]
DO.Data.noOuts <- DO.Data[which(DO.Data$FBE < 20),] #Remove FBE values over 20
names(DO.Data) <- c("DateTime", "WBE", "WBB", "SKE", "SKB", "PGE", "PGB", "CIE", "CIB", "FBE", "FBB")
DO.Data.melted <- melt(DO.Data, id="DateTime")
DO.Data.melted.noNA <- DO.Data.melted[which(!is.na(DO.Data.melted$value) & DO.Data.melted$value < 20 & DO.Data.melted$value > 0),]
DO.series <- plot_ly(data = DO.Data.melted.noNA, x = ~DateTime, y = ~value, type="scatter", mode="lines", color=~variable, hovertext=~value) %>%
  layout(title="Dissolved Oxygen across sites, 2016 DNR outplant \n(Values <0 and >20 not included)",
         yaxis = list(title = 'DO (mg/L)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(DO.series), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-series.html")
DO.box <- plot_ly(data = DO.Data.melted.noNA, x = ~variable, y = ~value, type="box", color=~variable) %>%
  layout(title="Dissolved Oxygen across sites, 2016 DNR outplant \n(Values <0 and >20 not included)",
         yaxis = list(title = 'DO (mg/L)'),
         legend = list(x=.95, y=.95))
htmlwidgets::saveWidget(as_widget(DO.box), "~/Documents/Roberts Lab/Paper-DNR-Geoduck-Proteomics/analyses/Environmental/June2016-Outplant-DO-box.html")


# Temperature
T.Data <- Env.Data[-nrow(Env.Data),c(1,grep("doT", colnames(Env.Data)))]
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

stats.EnvData <- data.frame(matrix(vector(), 12, 10, dimnames=list(c(), c("WBE", "WBB", "SKE", "SKB", "PGE", "PGB", "CIE", "CIB", "FBE", "FBB"))), stringsAsFactors = F, row.names = c("pH-Mean", "pH-SD", "pH-Var", "pH-CV", "DO-Mean", "DO-SD", "DO-Var", "DO-CV", "T-Mean", "T-SD", "T-Var", "T-CV"))
stats.EnvData[1,1:10] <- apply(pH.Data[,-1], 2, mean, na.rm=TRUE)
stats.EnvData[2,1:10] <- apply(pH.Data[,-1], 2, sd, na.rm=TRUE)
stats.EnvData[3,1:10] <- apply(pH.Data[,-1], 2, var, na.rm=TRUE)
stats.EnvData[4,1:10] <- stats.EnvData[3,1:10]/stats.EnvData[1,1:10]
stats.EnvData[5,1:10] <- apply(DO.Data.noOuts[,-1], 2, mean, na.rm=TRUE)
stats.EnvData[6,1:10]<- apply(DO.Data.noOuts[,-1], 2, sd, na.rm=TRUE)
stats.EnvData[7,1:10] <- apply(DO.Data.noOuts[,-1], 2, var, na.rm=TRUE)
stats.EnvData[8,1:10] <- stats.EnvData[7,1:10]/stats.EnvData[5,1:10]
stats.EnvData[9,1:10] <- apply(T.Data[,-1], 2, mean, na.rm=TRUE)
stats.EnvData[10,1:10] <- apply(T.Data[,-1], 2, sd, na.rm=TRUE)
stats.EnvData[11,1:10] <- apply(T.Data[,-1], 2, var, na.rm=TRUE)
stats.EnvData[12,1:10] <- stats.EnvData[11,1:10]/stats.EnvData[9,1:10]
round(stats.EnvData[1:12,1:10], digits = 3)
write.csv(file="../../analyses/Environmental/Env-stats.csv", stats.EnvData)

# Prepare data for Habitat / Site / Regional Comparisons

pH.Data4anova <- pH.Data.melted.noNA
colnames(pH.Data4anova) <- c("DateTime", "Treatment", "pH")
pH.Data4anova$Habitat <- substring(pH.Data4anova$Treatment, 3)
pH.Data4anova$Site <- substr(pH.Data4anova$Treatment, start=1, stop=2)
pH.Data4anova$Region <- gsub("WB|CI", "South", pH.Data4anova$Site)
pH.Data4anova$Region <- gsub("FB|PG", "North", pH.Data4anova$Region)
pH.Data4anova$Region <- gsub("SK", "NA", pH.Data4anova$Region)
write.csv(file="../../analyses/Environmental/pH.csv", pH.Data4anova)

DO.Data4anova <- DO.Data.melted.noNA
colnames(DO.Data4anova) <- c("DateTime", "Treatment", "DO")
DO.Data4anova$Habitat <- substring(DO.Data4anova$Treatment, 3)
DO.Data4anova$Site <- substr(DO.Data4anova$Treatment, start=1, stop=2)
DO.Data4anova$Region <- gsub("WB|CI", "South", DO.Data4anova$Site)
DO.Data4anova$Region <- gsub("FB|PG", "North", DO.Data4anova$Region)
DO.Data4anova$Region <- gsub("SK", "NA", DO.Data4anova$Region)
write.csv(file="../../analyses/Environmental/DO.csv", DO.Data4anova)

T.Data4anova <- T.Data.melted.noNA
colnames(T.Data4anova) <- c("DateTime", "Treatment", "Temp")
T.Data4anova$Habitat <- substring(T.Data4anova$Treatment, 3)
T.Data4anova$Site <- substr(T.Data4anova$Treatment, start=1, stop=2)
T.Data4anova$Region <- gsub("WB|CI", "South", T.Data4anova$Site)
T.Data4anova$Region <- gsub("FB|PG", "North", T.Data4anova$Region)
T.Data4anova$Region <- gsub("SK", "NA", T.Data4anova$Region)
T.Data4anova$Temp <- log(log(T.Data4anova$Temp))
write.csv(file="../../analyses/Environmental/T.csv", T.Data4anova)

# Bartlett Test of Homogeneity of Variances
bartlett.test(value~variable, data=pH.Data.melted.noNA)

Variance.EnvData <- data.frame(matrix(vector(), 6, 4, dimnames=list(c(), c("Treatment", "Habitat", "Site", "Region"))), stringsAsFactors = F, row.names = c("pH-F", "pH-P", "DO-F", "DO-P", "T-F", "T-P"))

for( i in 1:length(ds)) {
  Variance.EnvData[k[i]*1,1] <- bartlett.test(ds[[i]][,3]~ds[[i]][,2], data=ds[[i]])[[1]] #treatment F
  Variance.EnvData[(k[i]*1)+1,1] <- bartlett.test(ds[[i]][,3]~ds[[i]][,2], data=ds[[i]])[[3]] # P #treatment P
  Variance.EnvData[k[i]*1,2] <- bartlett.test(ds[[i]][,3]~ds[[i]][,4], data=ds[[i]])[[1]]  #habitat F
  Variance.EnvData[(k[i]*1)+1,2] <- bartlett.test(ds[[i]][,3]~ds[[i]][,4], data=ds[[i]])[[3]]  #habitat P
  Variance.EnvData[k[i]*1,3] <- bartlett.test(ds[[i]][,3]~ds[[i]][,5], data=ds[[i]])[[1]] #site F
  Variance.EnvData[(k[i]*1)+1,3] <- bartlett.test(ds[[i]][,3]~ds[[i]][,5], data=ds[[i]])[[3]] #site P
  Variance.EnvData[k[i]*1,4] <- bartlett.test(ds[[i]][,3]~ds[[i]][,6], data=ds[[i]])[[1]] #region F
  Variance.EnvData[(k[i]*1)+1,4] <- bartlett.test(ds[[i]][,3]~ds[[i]][,6], data=ds[[i]])[[3]] #region P
}
write.csv(file="../../analyses/Environmental/Env-Bartlett-Variance.csv", Variance.EnvData)

png("../../analyses/Env-Normality.png", width = 800, height = 800)
par(mfrow = c(3, 2), mar=c(1,1,3,1), oma=c(1,2,3,1), cex.main= 1.3, cex=.75)
qqnorm(pH.Data4anova$pH, main="pH QQ-Norm") #looks normal
qqline(pH.Data4anova$pH) 
hist(pH.Data4anova$pH, main="pH Histogram")
qqnorm(DO.Data4anova$DO, main="DO QQ-Norm") #looks normal
qqline(DO.Data4anova$DO)
hist(DO.Data4anova$DO, main="DO Histogram")
qqnorm(T.Data4anova$Temp, main="T QQ-Norm \n(2x log transformed") #does not look normal
qqline(T.Data4anova$Temp)
hist(T.Data4anova$Temp, main="Temp Histogram, \n2x log transformed")
dev.off()

anova.EnvData <- data.frame(matrix(vector(), 6, 4, dimnames=list(c(), c("Treatment", "Habitat", "Site", "Region"))), stringsAsFactors = F, row.names = c("pH-F", "pH-P", "DO-F", "DO-P", "T-F", "T-P"))

ds <- list(pH.Data4anova, DO.Data4anova, T.Data4anova)
k <- c(1,3,5)
for( i in 1:length(ds)) {
    round(anova.EnvData[k[i]*1,1] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,2], data=ds[[i]]))[[1]][[1,4]], digits=4) #treatment F
    round(anova.EnvData[(k[i]*1)+1,1] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,2], data=ds[[i]]))[[1]][[1,5]], digits=4) #treatment P
    round(anova.EnvData[k[i]*1,2] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,4], data=ds[[i]]))[[1]][[1,4]], digits=4)  #habitat F
    round(anova.EnvData[(k[i]*1)+1,2] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,4], data=ds[[i]]))[[1]][[1,5]], digits=4)  #habitat P
    round(anova.EnvData[k[i]*1,3] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,5], data=ds[[i]]))[[1]][[1,4]], digits=4) #site F
    round(anova.EnvData[(k[i]*1)+1,3] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,5], data=ds[[i]]))[[1]][[1,5]], digits=4) #site P
    round(anova.EnvData[k[i]*1,4] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,6], data=ds[[i]]))[[1]][[1,4]], digits=4) #region F
    round(anova.EnvData[(k[i]*1)+1,4] <- summary(aov(ds[[i]][,3] ~ ds[[i]][,6], data=ds[[i]]))[[1]][[1,5]], digits=4) #region P
  }

write.csv(file="../../analyses/Environmental/Env-ANOVA.csv", anova.EnvData)
