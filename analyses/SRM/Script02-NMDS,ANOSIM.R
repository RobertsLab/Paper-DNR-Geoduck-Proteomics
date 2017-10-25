# Script #2 in data processing for SRM data (not normalized)

#### 1. CREATE NMDS PLOT, MEAN OF TECH REPS - NOT LOG TRANSFORMED ########

#Transpose the file so that rows and columns are switched 
SRM.data.mean.t <- t(SRM.data.mean) # t() function transposes, removes PRTC transitions & extraneous info

#Replace NA cells with 0; metaMDS() does not handle NA's
SRM.data.mean.t.noNA <- SRM.data.mean.t
SRM.data.mean.t.noNA[is.na(SRM.data.mean.t.noNA)] <- 0

#Make MDS dissimilarity matrix
SRM.mean.nmds <- metaMDS(SRM.data.mean.t.noNA, distance = 'bray', k = 2, trymax = 3000, autotransform = FALSE)

#Make NMDS stressplot
png("../../analyses/SRM/NMDS-meanbysample-stressplot.png")
stressplot(SRM.mean.nmds) 
dev.off()

#Make NMDS plot 
plot(SRM.mean.nmds)
# site (sample) in black circle
# species (variable) in red ticks

# make figure with sample annotations & plot using ordiplot() https://stat.ethz.ch/pipermail/r-sig-ecology/2011-September/002371.html
SRM.nmds.mean.samples <- scores(SRM.mean.nmds, display = "sites")
library(RColorBrewer)
marker = c("indianred1", "forestgreen", "turquoise3", "mediumpurple1")

png("../../analyses/SRM/NMDS-meaned.png")
ordiplot(SRM.mean.nmds, type="n", main="SRM NMDS, unzoomed")
points(SRM.nmds.mean.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-2.5,-0.3, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in ### 

png("../../analyses/SRM/NMDS-meaned-zoomed.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-3,1), ylim=c(-0.5,0.5), asp=NA, main="SRM NMDS, zoomed")
points(SRM.nmds.mean.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-2.5,0.4, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

### Create plot with sample #'s to ID outliers AND with forced aspect ratio to zoom in
png("../../analyses/SRM/NMDS-meaned-zoomed-coded.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-3,1), ylim=c(-0.5,0.5), asp=NA, main="SRM NMDS, zoomed")
text(SRM.nmds.mean.samples[c(CI.B.samples),], label=rownames(SRM.nmds.mean.samples[c(CI.B.samples),]), col=marker[2], pch=8)
text(SRM.nmds.mean.samples[c(CI.E.samples),], label=rownames(SRM.nmds.mean.samples[c(CI.E.samples),]),  col=marker[2], pch=15)
text(SRM.nmds.mean.samples[c(PG.B.samples),], label=rownames(SRM.nmds.mean.samples[c(PG.B.samples),]),  col=marker[3], pch=8)
text(SRM.nmds.mean.samples[c(PG.E.samples),], label=rownames(SRM.nmds.mean.samples[c(PG.E.samples),]),  col=marker[3], pch=15)
text(SRM.nmds.mean.samples[c(WB.B.samples),], label=rownames(SRM.nmds.mean.samples[c(WB.B.samples),]),  col=marker[1], pch=8)
text(SRM.nmds.mean.samples[c(WB.E.samples),], label=rownames(SRM.nmds.mean.samples[c(WB.E.samples),]),  col=marker[1], pch=15)
text(SRM.nmds.mean.samples[c(FB.B.samples),], label=rownames(SRM.nmds.mean.samples[c(FB.B.samples),]),  col=marker[4], pch=8)
text(SRM.nmds.mean.samples[c(FB.E.samples),], label=rownames(SRM.nmds.mean.samples[c(CI.E.samples),]),  col=marker[4], pch=15)
legend(-2.5,0.4, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in, not including CI outliers ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-coded-nooutliers.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-1.2,1), ylim=c(-0.2,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600)
points(SRM.nmds.mean.samples[c(CI.B.samples),], col=marker[2], pch=8, cex=2)
points(SRM.nmds.mean.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(PG.B.samples),], col=marker[3], pch=8, cex=2)
points(SRM.nmds.mean.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(WB.B.samples),], col=marker[1], pch=8, cex=2)
points(SRM.nmds.mean.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(FB.B.samples),], col=marker[4], pch=8, cex=2)
points(SRM.nmds.mean.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-1.2,-0.07, pch=c(rep(16,4), 8, 15), cex=1.1, pt.cex=1.3, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in, no eel/bare separation ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-sitecoded.png")
plot.default(x=NULL, y=NULL, type="n", xlab="Dimension 1", ylab="Dimension 2", xlim=c(-2.8,1), ylim=c(-0.4,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600, cex.axis=1.4, cex.lab=1.6)
points(SRM.nmds.mean.samples[c(CI.B.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(PG.B.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(WB.B.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(FB.B.samples),], col=marker[4], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-2.7,0.2, pch=c(rep(16,4)), cex=1.1, pt.cex=1.3, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay"), col=c(marker[4], marker[3], marker[2], marker[1]))
dev.off()

#### Create plot with forced aspect ratio to zoom in, no eel/bare separation, no outliers ### 
png("../../analyses/SRM/NMDS-meaned-zoomed-sitecoded-nooutliers.png")
par(mar=c(5.1,4.8,4.1,2.1))
plot.default(x=NULL, y=NULL, type="n", xlab="Dimension 1", ylab="Dimension 2",  xlim=c(-1.2,1), ylim=c(-0.2,0.2), asp=NA, main="Geoduck Gill NMDS, Similarity Plot", width=600,height=600, cex.axis=1.4, cex.lab=1.6, cex.main=1.8)
points(SRM.nmds.mean.samples[c(CI.B.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(CI.E.samples),], col=marker[2], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(PG.B.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(PG.E.samples),], col=marker[3], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(WB.B.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(WB.E.samples),], col=marker[1], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(FB.B.samples),], col=marker[4], pch=15, cex=2)
points(SRM.nmds.mean.samples[c(FB.E.samples),], col=marker[4], pch=15, cex=2)
legend(-1.2,-0.07, pch=c(rep(15,4)), cex=1.6, pt.cex=2, legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay"), col=c(marker[4], marker[3], marker[2], marker[1]))
dev.off()

#Create interactive graph in Plotly
SRM.nmds4plotly <- as.data.frame(SRM.nmds.mean.samples)
SRM.nmds4plotly$Sample <- rownames(SRM.nmds4plotly)
SRM.nmds4plotly.annotated <- merge(x=SRM.nmds4plotly, y=sample.key[,c(3,5,8,9)], by.x="Sample", by.y="PRVial")
SRM.nmds4plotly.annotated$Site <- factor(SRM.nmds4plotly.annotated$Site, levels=c("WB", "CI", "PG", "FB"))
library(plotly)
p.NMDS <- plot_ly(data=SRM.nmds4plotly.annotated, x=~NMDS1, y=~NMDS2, color=~Site, symbol=~Patch, type="scatter", mode="markers", marker = list(size = 20), colors=marker, hoverinfo = 'text', text = ~Sample) %>% 
  layout(title="SRM NMDS of all proteins by Site, Treatment",
         xaxis = list(title = 'NMDS Axis 1'),
         yaxis = list(title = 'NMDS Axis 2'))
htmlwidgets::saveWidget(as_widget(p.NMDS), "NMDS-meaned-Plotly.html") #Save plotly plot as html widget

# OPTIONAL: POST PLOTLY GRAPHS ONLINE 
# Sys.setenv("plotly_username"="<PLOTLY USERNAME HERE>") #Insert Plotly username
# Sys.setenv("plotly_api_key"="<PLOTLY ACCOUNT API KEY HERE>") #Insert Plotly API key, find key @ https://plot.ly/settings/api
api_create(p.NMDS, filename = "Geoduck-SRM-NMDS") #Pushes plot to Plotly online


#### 2. CREATE NMDS PLOT, MEAN OF TECH REPS - LOG TRANSFORMED ########

#Transpose the file so that rows and columns are switched and normalized by log(x+1)
SRM.data.mean.t.log <- SRM.data.mean.t
SRM.data.mean.t.log[is.na(SRM.data.mean.t.log)] <- 0
SRM.data.mean.t.log <- (SRM.data.mean.t.log+1)
SRM.data.mean.t.log <- data.trans(SRM.data.mean.t.log, method = 'log', plot = FALSE)

#Make MDS dissimilarity matrix
SRM.mean.log.nmds <- metaMDS(SRM.data.mean.t.log, distance = 'bray', k = 2, trymax = 3000, autotransform = FALSE)
stressplot(SRM.mean.log.nmds) 
plot(SRM.mean.log.nmds)
# site (sample) in black circle
# species (variable) in red ticks

SRM.nmds.mean.log.samples <- scores(SRM.mean.log.nmds, display = "sites")

png("../../analyses/SRM/NMDS-mean-log.png")
ordiplot(SRM.mean.log.nmds, type="n", main="SRM NMDS, log+1 transformed, unzoomed")
points(SRM.nmds.mean.log.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean.log.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean.log.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean.log.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean.log.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean.log.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean.log.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean.log.samples[c(FB.E.samples),], col=marker[4], pch=15)
legend(-.3,-0.1, pch=c(rep(16,4), 8, 15), legend=c("Fidalgo Bay", "Port Gamble", 'Case Inlet', "Willapa Bay", "Bare", "Eelgrass"), col=c(marker[4], marker[3], marker[2], marker[1], "black", "black"))
dev.off()

#### Create plot with forced aspect ratio to zoom in ### 

png("../../analyses/SRM/NMDS-mean-log-zoomed.png")
plot.default(x=NULL, y=NULL, type="n", xlab="NMDS axis 1", ylab="NMDS axis 2", xlim=c(-.15,.15), ylim=c(-0.05,.06), asp=NA, main="SRM NMDS, log+1 transformed, zoomed")
points(SRM.nmds.mean.log.samples[c(CI.B.samples),], col=marker[2], pch=8)
points(SRM.nmds.mean.log.samples[c(CI.E.samples),], col=marker[2], pch=15)
points(SRM.nmds.mean.log.samples[c(PG.B.samples),], col=marker[3], pch=8)
points(SRM.nmds.mean.log.samples[c(PG.E.samples),], col=marker[3], pch=15)
points(SRM.nmds.mean.log.samples[c(WB.B.samples),], col=marker[1], pch=8)
points(SRM.nmds.mean.log.samples[c(WB.E.samples),], col=marker[1], pch=15)
points(SRM.nmds.mean.log.samples[c(FB.B.samples),], col=marker[4], pch=8)
points(SRM.nmds.mean.log.samples[c(FB.E.samples),], col=marker[4], pch=15)
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

###############

# ANOSIM of data (not log transformed, no zeros in data (instead, ignore NAs))
data4anosim <- cbind.data.frame(SRM.data.mean.t[order(rownames(SRM.data.mean.t)),], samples4anosim[order(samples4anosim$SAMPLE),])
data4anosim$SITE <- as.factor(data4anosim$SITE)
data4anosim$TREATMENT <- as.factor(data4anosim$TREATMENT)
data4anosim$BOTH <- as.factor(data4anosim$BOTH)
data4anosim$REGION <- as.factor(data4anosim$REGION)

sdms.vegdist <- vegdist(data4anosim[,-(ncol(data4anosim)-4):-(ncol(data4anosim))], 'bray', na.rm=TRUE) #this also removes the last 5 columns of data, since they are factors

# ANOSIM between sites
site.anos<-anosim(sdms.vegdist, grouping=data4anosim$SITE, permutations = 2000)
summary(site.anos)
png("../../analyses/SRM/ANOSIM-site-ignoreNA.png")
plot(site.anos)
dev.off()
graphics.off()

# ANOSIM between treatments
treatment.anos<-anosim(sdms.vegdist, grouping=data4anosim$TREATMENT, permutations = 2000)
summary(treatment.anos)
png("../../analyses/SRM/ANOSIM-treatment-ignoreNA.png")
plot(treatment.anos)
dev.off()
graphics.off()

# ANOSIM between both site/treatments
siteANDtreatment.anos<-anosim(sdms.vegdist, grouping=data4anosim$BOTH, permutations = 2000)
summary(siteANDtreatment.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-ignoreNA.png")
plot(siteANDtreatment.anos)
dev.off()

# ANOSIM between region
region.anos<-anosim(sdms.vegdist, grouping=data4anosim$REGION, permutations = 2000)
summary(region.anos)
png("../../analyses/SRM/ANOSIM-region-ignoreNA.png")
plot(region.anos)
dev.off()
graphics.off()

############
# ANOSIM of data (not log transformed, including zeros where no peak found)
data4anosim.noNA <- cbind.data.frame(SRM.data.mean.t.noNA[order(rownames(SRM.data.mean.t.noNA)),], samples4anosim[order(samples4anosim$SAMPLE),])
sdms.noNA.vegdist <- vegdist(data4anosim.noNA[,-(ncol(data4anosim)-4):-(ncol(data4anosim))], 'bray', na.rm=TRUE)

# ANOSIM between sites, no NA
site.noNA.anos<-anosim(sdms.vegdist, grouping=data4anosim.noNA$SITE, permutations = 2000)
summary(site.noNA.anos)
png("../../analyses/SRM/ANOSIM-site-zeroNA.png")
plot(site.noNA.anos)
dev.off()
graphics.off()

# ANOSIM between treatments, no NA
treatment.noNA.anos<-anosim(sdms.noNA.vegdist, grouping=data4anosim.noNA$TREATMENT, permutations = 2000)
summary(treatment.noNA.anos)
png("../../analyses/SRM/ANOSIM-treatment-zeroNA.png")
plot(treatment.noNA.anos)
dev.off()
graphics.off()

# ANOSIM between both site/treatments, no NA
siteANDtreatment.noNA.anos <- anosim(sdms.noNA.vegdist, grouping=data4anosim.noNA$BOTH, permutations = 2000)
siteANDtreatment.noNA.anos
summary(siteANDtreatment.noNA.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-zeroNA.png")
plot(siteANDtreatment.noNA.anos)
dev.off()

# ANOSIM between region, no NA
region.noNA.anos <- anosim(sdms.noNA.vegdist, grouping=data4anosim.noNA$REGION, permutations = 2000)
region.noNA.anos
summary(region.noNA.anos)
png("../../analyses/SRM/ANOSIM-region-zeroNA.png")
plot(region.noNA.anos)
dev.off()
graphics.off()

#############
# ANOSIM of data after log+1 transformation  
data4anosim.log <- cbind.data.frame(SRM.data.mean.t.log[order(rownames(SRM.data.mean.t.log)),], samples4anosim[order(samples4anosim$SAMPLE),])
sdms.log.vegdist <- vegdist(data4anosim.log[,-(ncol(data4anosim)-4):-(ncol(data4anosim))], 'bray', na.rm=TRUE)

# ANOSIM between sites, log+1 transf.
site.log.anos<-anosim(sdms.log.vegdist, grouping=data4anosim.log$SITE, permutations = 2000)
summary(site.log.anos)
png("../../analyses/SRM/ANOSIM-site-log.png")
plot(site.log.anos)
dev.off()
graphics.off()

# ANOSIM between treatments, log+1 transf.
treatment.log.anos<-anosim(sdms.log.vegdist, grouping=data4anosim.log$TREATMENT, permutations = 2000)
summary(treatment.log.anos)
png("../../analyses/SRM/ANOSIM-treatment-log.png")
plot(treatment.log.anos)
dev.off()
graphics.off()

# ANOSIM between site/treatments, log+1 transf.
siteANDtreatment.log.anos <- anosim(sdms.log.vegdist, grouping=data4anosim.log$BOTH, permutations = 2000)
summary(siteANDtreatment.log.anos)
png("../../analyses/SRM/ANOSIM-sitetreatment-log.png")
plot(siteANDtreatment.log.anos)
dev.off()

# ANOSIM between region, log+1 transf.
region.log.anos <- anosim(sdms.log.vegdist, grouping=data4anosim.log$REGION, permutations = 2000)
summary(region.log.anos)
png("../../analyses/SRM/ANOSIM-region-log.png")
plot(region.log.anos)
dev.off()
graphics.off()
