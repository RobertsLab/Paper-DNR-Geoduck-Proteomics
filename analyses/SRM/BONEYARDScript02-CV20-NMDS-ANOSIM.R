library(reshape2)
SRM.data20.mean <- data.frame(cast(SRM.reps4stats.20[,c(1,2,8)], Sample ~ Transition, value.var = mean))
rownames(SRM.data20.mean) <- SRM.data20.mean$Sample
SRM.data20.mean <- SRM.data20.mean[,-1]

#### 1. CREATE NMDS PLOT, MEAN OF TECH REPS - NOT LOG TRANSFORMED ########

#Replace NA cells with 0; metaMDS() does not handle NA's
SRM.data20.mean.noNA <- t(t(SRM.data20.mean))
SRM.data20.mean.noNA[is.na(SRM.data20.mean.noNA)] <- 0
sum(count(SRM.data20.mean.noNA >0)) / ncol(SRM.data20.mean.noNA)*nrow(SRM.data20.mean.noNA)
(sum(count(SRM.data20.mean.noNA >0)))/(ncol(SRM.data20.mean.noNA)*nrow(SRM.data20.mean.noNA))*100 #how many transitions have >0 abundance in my samples after filtering due to CV>20
View(SRM.data20.mean.noNA)
#======

#Make MDS dissimilarity matrix
SRM.mean20.nmds <- metaMDS(SRM.data20.mean.noNA, distance = 'bray', k = 2, trymax = 1000, autotransform = FALSE)

#Make NMDS stressplot
png("../../analyses/SRM/NMDS-meanbysample-stressplot.png")
stressplot(SRM.mean20.nmds) 
dev.off()

#Make NMDS plot 
plot(SRM.mean20.nmds)
# site (sample) in black circle
# species (variable) in red ticks

# make figure with sample annotations & plot using ordiplot() https://stat.ethz.ch/pipermail/r-sig-ecology/2011-September/002371.html
SRM.nmds.mean20.samples <- scores(SRM.mean20.nmds, display = "sites")
library(RColorBrewer)
marker = c("indianred1", "forestgreen", "turquoise3", "mediumpurple1")

png("../../analyses/SRM/NMDS-meaned.png")
ordiplot(SRM.mean20.nmds, type="n", main="SRM NMDS, unzoomed")
points(SRM.nmds.mean20.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean20.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean20.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean20.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean20.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean20.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean20.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean20.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-2.5,-0.3, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in ### 

png("../../analyses/SRM/NMDS-meaned-zoomed.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-3,1), ylim=c(-0.5,0.5), asp=NA, main="SRM NMDS, zoomed")
points(SRM.nmds.mean20.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean20.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean20.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean20.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean20.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean20.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean20.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean20.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-2.5,0.4, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

### Create plot with sample #'s to ID outliers AND with forced aspect ratio to zoom in
png("../../analyses/SRM/NMDS-meaned-zoomed-coded.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-3,1), ylim=c(-0.5,0.5), asp=NA, main="SRM NMDS, zoomed")
text(SRM.nmds.mean20.samples[c(CI.B.samples),], label=rownames(SRM.nmds.mean20.samples[c(CI.B.samples),]), col=marker[2], pch=8)
text(SRM.nmds.mean20.samples[c(CI.E.samples),], label=rownames(SRM.nmds.mean20.samples[c(CI.E.samples),]),  col=marker[2], pch=15)
text(SRM.nmds.mean20.samples[c(PG.B.samples),], label=rownames(SRM.nmds.mean20.samples[c(PG.B.samples),]),  col=marker[3], pch=8)
text(SRM.nmds.mean20.samples[c(PG.E.samples),], label=rownames(SRM.nmds.mean20.samples[c(PG.E.samples),]),  col=marker[3], pch=15)
text(SRM.nmds.mean20.samples[c(WB.B.samples),], label=rownames(SRM.nmds.mean20.samples[c(WB.B.samples),]),  col=marker[1], pch=8)
text(SRM.nmds.mean20.samples[c(WB.E.samples),], label=rownames(SRM.nmds.mean20.samples[c(WB.E.samples),]),  col=marker[1], pch=15)
text(SRM.nmds.mean20.samples[c(FB.B.samples),], label=rownames(SRM.nmds.mean20.samples[c(FB.B.samples),]),  col=marker[4], pch=8)
text(SRM.nmds.mean20.samples[c(FB.E.samples),], label=rownames(SRM.nmds.mean20.samples[c(CI.E.samples),]),  col=marker[4], pch=15)
legend(-2.5,0.4, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in, not including CI outliers ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-coded-nooutliers.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-1.2,1), ylim=c(-0.2,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600)
points(SRM.nmds.mean20.samples[c(CI.B.samples),], col=marker[2], pch=8, cex=2)
points(SRM.nmds.mean20.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(PG.B.samples),], col=marker[3], pch=8, cex=2)
points(SRM.nmds.mean20.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(WB.B.samples),], col=marker[1], pch=8, cex=2)
points(SRM.nmds.mean20.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(FB.B.samples),], col=marker[4], pch=8, cex=2)
points(SRM.nmds.mean20.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-1.2,-0.07, pch=c(rep(16,4), 8, 15), cex=1.1, pt.cex=1.3, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in, no eel/bare separation ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-sitecoded.png")
plot.default(x=NULL, y=NULL, type="n", xlab="Dimension 1", ylab="Dimension 2", xlim=c(-2.8,1), ylim=c(-0.4,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600, cex.axis=1.4, cex.lab=1.6)
points(SRM.nmds.mean20.samples[c(CI.B.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(PG.B.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(WB.B.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(FB.B.samples),], col=marker[4], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-2.7,0.2, pch=c(rep(16,4)), cex=1.1, pt.cex=1.3, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay"), col=c(marker[4], marker[3], marker[2], marker[1]))
dev.off()

#### Create plot with forced aspect ratio to zoom in, no eel/bare separation, no outliers ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-sitecoded-nooutliers.png")
par(mar=c(5.1,4.8,4.1,2.1))
plot.default(x=NULL, y=NULL, type="n", xlab="Dimension 1", ylab="Dimension 2",  xlim=c(-1.2,1), ylim=c(-0.2,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600, cex.axis=1.4, cex.lab=1.6, cex.main=1.8)
points(SRM.nmds.mean20.samples[c(CI.B.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(PG.B.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(WB.B.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(FB.B.samples),], col=marker[4], pch=15, cex=2)
points(SRM.nmds.mean20.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-1.2,-0.07, pch=c(rep(15,4)), cex=1.6, pt.cex=2, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay"), col=c(marker[4], marker[3], marker[2], marker[1]))
dev.off()

#Create interactive graph in Plotly
SRM.nmds20.4plotly <- as.data.frame(SRM.nmds.mean20.samples)
SRM.nmds20.4plotly$Sample <- rownames(SRM.nmds20.4plotly)
SRM.nmds20.4plotly.annotated <- merge(x=SRM.nmds20.4plotly, y=sample.key[,c(3,5,8,9)], by.x="Sample", by.y="PRVial")
SRM.nmds20.4plotly.annotated$Site <- factor(SRM.nmds20.4plotly.annotated$Site, levels=c("WB", "CI", "PG", "FB"))
library(plotly)
p.NMDS20 <- plot_ly(data=SRM.nmds20.4plotly.annotated, x=~NMDS1, y=~NMDS2, color=~Site, symbol=~Patch, type="scatter", mode="markers", marker = list(size = 20), colors=marker, hoverinfo = 'text', text = ~Sample) %>% 
  layout(title="SRM NMDS of all proteins by Site, Treatment",
         xaxis = list(title = 'NMDS Axis 1'),
         yaxis = list(title = 'NMDS Axis 2'))
htmlwidgets::saveWidget(as_widget(p.NMDS20), "NMDS-meaned-Plotly.html") #Save plotly plot as html widget

# OPTIONAL: POST PLOTLY GRAPHS ONLINE 
# Sys.setenv("plotly_username"="<PLOTLY USERNAME HERE>") #Insert Plotly username
# Sys.setenv("plotly_api_key"="<PLOTLY ACCOUNT API KEY HERE>") #Insert Plotly API key, find key @ https://plot.ly/settings/api
api_create(p.NMDS20, filename = "Geoduck-SRM-NMDS") #Pushes plot to Plotly online


#### 2. CREATE NMDS PLOT, MEAN OF TECH REPS - LOG TRANSFORMED ########

#Transpose the file so that rows and columns are switched and normalized by log(x+1)
SRM.data.mean20.t.log <- SRM.data.mean20.t
SRM.data.mean20.t.log[is.na(SRM.data.mean20.t.log)] <- 0
SRM.data.mean20.t.log <- (SRM.data.mean20.t.log+1)
SRM.data.mean20.t.log <- data.trans(SRM.data.mean20.t.log, method = 'log', plot = FALSE)

#Make MDS dissimilarity matrix
SRM.mean20.log.nmds <- metaMDS(SRM.data.mean20.t.log, distance = 'bray', k = 2, trymax = 3000, autotransform = FALSE)
stressplot(SRM.mean20.log.nmds) 
plot(SRM.mean20.log.nmds)
# site (sample) in black circle
# species (variable) in red ticks

SRM.nmds.mean20.log.samples <- scores(SRM.mean20.log.nmds, display = "sites")

png("../../analyses/SRM/NMDS-mean-log.png")
ordiplot(SRM.mean20.log.nmds, type="n", main="SRM NMDS, log+1 transformed, unzoomed")
points(SRM.nmds.mean20.log.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean20.log.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean20.log.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean20.log.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean20.log.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean20.log.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean20.log.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean20.log.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-.3,-0.1, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in ### 

png("../../analyses/SRM/NMDS-mean-log-zoomed.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-.15,.15), ylim=c(-0.05,.06), asp=NA, main="SRM NMDS, log+1 transformed, zoomed")
points(SRM.nmds.mean20.log.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean20.log.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean20.log.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean20.log.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean20.log.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean20.log.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean20.log.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean20.log.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-.13,0.06, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

################################################################################################################################
# RUN ANALYSIS OF SIMILARITY (ANOSIM) ON DIFFERENT ITERATIONS OF DATA

###### PREPARE DATA FOR ANOSIM ##########

CI.b <- data.frame(SAMPLE=CI.B.samples, SITE=rep("CI", times=length(CI.B.samples)), TREATMENT=rep("Bare", times=length(CI.B.samples)), BOTH=rep("CI-Bare", times=length(CI.B.samples)))
CI.e <- data.frame(SAMPLE=CI.E.samples, SITE=rep("CI", times=length(CI.E.samples)), TREATMENT=rep("Eelgrass", times=length(CI.E.samples)), BOTH=rep("CI-Eel", times=length(CI.E.samples)))
PG.b <- data.frame(SAMPLE=PG.B.samples, SITE=rep("PG", times=length(PG.B.samples)), TREATMENT=rep("Bare", times=length(PG.B.samples)), BOTH=rep("PG-Bare", times=length(PG.B.samples)))
PG.e <- data.frame(SAMPLE=PG.E.samples, SITE=rep("PG", times=length(PG.E.samples)), TREATMENT=rep("Eelgrass", times=length(PG.E.samples)), BOTH=rep("PG-Eel", times=length(PG.E.samples)))
WB.b <- data.frame(SAMPLE=WB.B.samples, SITE=rep("WB", times=length(WB.B.samples)), TREATMENT=rep("Bare", times=length(WB.B.samples)), BOTH=rep("WB-Bare", times=length(WB.B.samples)))
WB.e <- data.frame(SAMPLE=WB.E.samples, SITE=rep("WB", times=length(WB.E.samples)), TREATMENT=rep("Eelgrass", times=length(WB.E.samples)), BOTH=rep("WB-Eel", times=length(WB.E.samples)))
FB.b <- data.frame(SAMPLE=FB.B.samples, SITE=rep("FB", times=length(FB.B.samples)), TREATMENT=rep("Bare", times=length(FB.B.samples)), BOTH=rep("FB-Bare", times=length(FB.B.samples)))
FB.e <- data.frame(SAMPLE=FB.E.samples, SITE=rep("FB", times=length(FB.E.samples)), TREATMENT=rep("Eelgrass", times=length(FB.E.samples)), BOTH=rep("FB-Eel", times=length(FB.E.samples)))

samples4anosim <- rbind.data.frame(CI.b, CI.e, PG.b, PG.e, WB.b, WB.e, FB.e, FB.b, stringsAsFactors = TRUE)
samples4anosim$SAMPLE <- as.character(samples4anosim$SAMPLE)

#Add region category for ANOSIMS
samples4anosim$REGION <-
  ifelse(grepl("FB|PG", samples4anosim$SITE)==T,"North",
         ifelse(grepl("CI|WB",samples4anosim$SITE)==T,"South","Error"))

# ANOSIM of data (not log transformed, no zeros in data (instead, ignore NAs))
data20.4anosim <- merge(x=SRM.data20.mean, y=samples4anosim, by.x="row.names", by.y="SAMPLE") 
rownames(data20.4anosim) <- data20.4anosim$Row.names
data20.4anosim <- data20.4anosim[,-1]
data20.4anosim$SITE <- as.factor(data20.4anosim$SITE)
data20.4anosim$TREATMENT <- as.factor(data20.4anosim$TREATMENT)
data20.4anosim$BOTH <- as.factor(data20.4anosim$BOTH)
data20.4anosim$REGION <- as.factor(data20.4anosim$REGION)

sdms20.vegdist <- vegdist(data20.4anosim[,-(ncol(data20.4anosim)-4):-(ncol(data20.4anosim))], 'bray', na.rm=TRUE) #this also removes the last 5 columns of data, since they are factors

# ANOSIM between sites
site20.anos<-anosim(sdms20.vegdist, grouping=data20.4anosim$SITE, permutations = 2000)
summary(site20.anos)
png("../../analyses/SRM/ANOSIM-site-ignoreNA.png")
plot(site20.anos)
dev.off()
graphics.off()

# ANOSIM between treatments
treatment20.anos<-anosim(sdms20.vegdist, grouping=data20.4anosim$TREATMENT, permutations = 2000)
summary(treatment20.anos)
png("../../analyses/SRM/ANOSIM-treatment-ignoreNA.png")
plot(treatment20.anos)
dev.off()
graphics.off()

# ANOSIM between both site/treatments
siteANDtreatment20.anos<-anosim(sdms20.vegdist, grouping=data20.4anosim$BOTH, permutations = 2000)
summary(siteANDtreatment20.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-ignoreNA.png")
plot(siteANDtreatment20.anos)
dev.off()

# ANOSIM between region
region20.anos<-anosim(sdms20.vegdist, grouping=data20.4anosim$REGION, permutations = 2000)
summary(region20.anos)
png("../../analyses/SRM/ANOSIM-region-ignoreNA.png")
plot(region20.anos)
dev.off()
graphics.off()

############
# ANOSIM of data (not log transformed, including zeros where no peak found)
data20.4anosim.noNA <- cbind.data.frame(SRM.data20.mean.t.noNA[order(rownames(SRM.data20.mean.t.noNA)),], samples4anosim[order(samples4anosim$SAMPLE),])
sdms20.noNA.vegdist <- vegdist(data20.4anosim.noNA[,-(ncol(data20.4anosim)-4):-(ncol(data20.4anosim))], 'bray', na.rm=TRUE)

# ANOSIM between sites, no NA
site20.noNA.anos<-anosim(sdms20.noNA.vegdist, grouping=data20.4anosim.noNA$SITE, permutations = 2000)
summary(site20.noNA.anos)
png("../../analyses/SRM/ANOSIM-site-zeroNA.png")
plot(site20.noNA.anos)
dev.off()
graphics.off()

# ANOSIM between treatments, no NA
treatment20.noNA.anos<-anosim(sdms20.noNA.vegdist, grouping=data20.4anosim.noNA$TREATMENT, permutations = 2000)
summary(treatment20.noNA.anos)
png("../../analyses/SRM/ANOSIM-treatment-zeroNA.png")
plot(treatment20.noNA.anos)
dev.off()
graphics.off()

# ANOSIM between both site/treatments, no NA
siteANDtreatment20.noNA.anos <- anosim(sdms20.noNA.vegdist, grouping=data20.4anosim.noNA$BOTH, permutations = 2000)
siteANDtreatment20.noNA.anos
summary(siteANDtreatment20.noNA.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-zeroNA.png")
plot(siteANDtreatment20.noNA.anos)
dev.off()

# ANOSIM between region, no NA
region20.noNA.anos <- anosim(sdms20.noNA.vegdist, grouping=data20.4anosim.noNA$REGION, permutations = 2000)
region20.noNA.anos
summary(region20.noNA.anos)
png("../../analyses/SRM/ANOSIM-region-zeroNA.png")
plot(region20.noNA.anos)
dev.off()
graphics.off()

#############
# ANOSIM of data after log+1 transformation  
data20.4anosim.log <- cbind.data.frame(SRM.data20.mean.t.log[order(rownames(SRM.data20.mean.t.log)),], samples4anosim[order(samples4anosim$SAMPLE),])
sdms20.log.vegdist <- vegdist(data20.4anosim.log[,-(ncol(data20.4anosim)-4):-(ncol(data20.4anosim))], 'bray', na.rm=TRUE)

# ANOSIM between sites, log+1 transf.
site20.log.anos<-anosim(sdms20.log.vegdist, grouping=data20.4anosim.log$SITE, permutations = 2000)
summary(site20.log.anos)
png("../../analyses/SRM/ANOSIM-site-log.png")
plot(site20.log.anos)
dev.off()
graphics.off()

# ANOSIM between treatments, log+1 transf.
treatment20.log.anos<-anosim(sdms20.log.vegdist, grouping=data20.4anosim.log$TREATMENT, permutations = 2000)
summary(treatment20.log.anos)
png("../../analyses/SRM/ANOSIM-treatment-log.png")
plot(treatment20.log.anos)
dev.off()
graphics.off()

# ANOSIM between site/treatments, log+1 transf.
siteANDtreatment20.log.anos <- anosim(sdms20.log.vegdist, grouping=data20.4anosim.log$BOTH, permutations = 2000)
summary(siteANDtreatment20.log.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-log.png")
plot(siteANDtreatment20.log.anos)
dev.off()

# ANOSIM between region, log+1 transf.
region20.log.anos <- anosim(sdms20.log.vegdist, grouping=data20.4anosim.log$REGION, permutations = 2000)
summary(region20.log.anos)
png("../../analyses/SRM/ANOSIM-region-log.png")
plot(region20.log.anos)
dev.off()
graphics.off()