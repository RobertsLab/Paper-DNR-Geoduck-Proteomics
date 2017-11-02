
# Calculate CV between technical reps for each transition and plot using Plotly

# Reformat data
SRM.temp1 <- t(SRM.data.screened.noPRTC[,-1:-4]) #Remove extraneous protein info
SRM.temp2 <- as.data.frame(melt(SRM.temp1, id=rownames(SRM.temp1))) #melt data into long-format
SRM.temp2$Var1 <- gsub('-remake-', '-', SRM.temp2$Var1)
SRM.temp2$Var1 <- as.character(SRM.temp2$Var1) #convert sample ID to character strings
library(tidyr) #open tidyr program package
SRM.temp3 <- separate(data=SRM.temp2, col=Var1, into = c('Sample', 'Replicate'), sep = -3, convert = TRUE) #split sample ID into number, replicate 

# Calculate stats
library(reshape2)
SRM.reps4stats <- dcast(SRM.temp3, Sample + Var2 ~ Replicate) #widen data to create a column for each replicate with area data
SRM.reps4stats$sd <- apply(SRM.reps4stats[,3:6], 1, sd, na.rm=TRUE) #calculate standard deviation across all replicates for each sample
SRM.reps4stats$mean <- apply(SRM.reps4stats[,3:6], 1, mean, na.rm=TRUE) #calculate mean across all replicates for each sample
SRM.reps4stats$variance <- (SRM.reps4stats$sd/SRM.reps4stats$mean)*100 #calculate coefficient of variace across all replicates for each sample

# Merge protein info back # save data set
SRM.reps4stats.plots <- merge(x=SRM.reps4stats, y=SRM.data.screened.noPRTC[,1:3], by.x=2, by.y=0, all.x=TRUE, all.y=FALSE)
names(SRM.reps4stats.plots) <- c("Transition","Sample","-A","-B","-C","-D","sd","mean","variance","Protein", "Fragment","Peptide") #simplify column names
write.csv(SRM.reps4stats.plots, file="../../analyses/SRM/SRM-techrep-stats.csv") #Save tech rep stats data as .csv 

# Plot via Plotly
library(plotly) #open plotly program package
p.techrep <-  plot_ly(data = SRM.reps4stats.plots, x = ~Sample, y = ~variance, type="scatter", mode="markers", color=~Protein, hovertext=~paste(Protein, Transition)) %>%  #generate plotly plot
  layout(title="Coefficient of Variance among technical reps, by sample",
         yaxis = list(title = 'Coefficient of Variance'),
         legend = list(x=.75, y=.95))
htmlwidgets::saveWidget(as_widget(p.techrep), "SRM-tech-rep-CV.html") #Save plotly plot as html widget 
api_create(p.techrep, filename = "Geoduck-SRM-tech-rep-CV") #Pushes plot to Plotly online, may need to log in via next 2 lines: 
# Sys.setenv("plotly_username"="username") #Insert Plotly username
# Sys.setenv("plotly_api_key"="api key") #Insert Plotly API key



################ REMOVE POOR QUALITY TECH REPS & re-do

# Tech reps removed: 3C, 42C, 53B, 53D, 70C, 73B, 104B, 104D, 127B, 128A, 55A, 114A, 114D
# Entire sample removed: 57
SRM.temp2.screened <- SRM.temp2[!grepl("3-C|42-C|53-B|53-D|70-C|73-B|104-B|104-D|127-B|128-A|55-A|114-A|114-D|57", SRM.temp2$Var1),]
SRM.temp3.s <- separate(data=SRM.temp2.screened, col=Var1, into = c('Sample', 'Replicate'), sep = -3, convert = TRUE) #split sample ID into number, replicate 

# Calculate stats
SRM.reps4stats.s <- dcast(SRM.temp3.s, Sample + Var2 ~ Replicate) #widen data to create a column for each replicate with area data
SRM.reps4stats.s$sd <- apply(SRM.reps4stats.s[,3:6], 1, sd, na.rm=TRUE) #calculate standard deviation across all replicates for each sample
SRM.reps4stats.s$mean <- apply(SRM.reps4stats.s[,3:6], 1, mean, na.rm=TRUE) #calculate mean across all replicates for each sample
SRM.reps4stats.s$variance <- (SRM.reps4stats.s$sd/SRM.reps4stats.s$mean)*100 #calculate coefficient of variace across all replicates for each sample

# Merge protein info back # save data set
SRM.reps4stats.s.plots <- merge(x=SRM.reps4stats.s, y=SRM.data.screened.noPRTC[,1:3], by.x=2, by.y=0, all.x=TRUE, all.y=FALSE)
names(SRM.reps4stats.s.plots) <- c("Transition","Sample","-A","-B","-C","-D","sd","mean","variance","Protein", "Fragment","Peptide") #simplify column names
write.csv(SRM.reps4stats.s.plots, file="../../analyses/SRM/SRM-techrep-stats.csv") #Save tech rep stats data as .csv 

# Plot via Plotly
library(plotly) #open plotly program package
p.techrep.s <-  plot_ly(data = SRM.reps4stats.s.plots, x = ~Sample, y = ~variance, type="scatter", mode="markers", color=~Protein, hovertext=~paste(Protein, Transition)) %>%  #generate plotly plot
  layout(title="Coefficient of Variance among screened technical reps, by sample",
         yaxis = list(title = 'Coefficient of Variance'))
htmlwidgets::saveWidget(as_widget(p.techrep.s), "SRM-screened-tech-rep-CV.html") #Save plotly plot as html widget 
api_create(p.techrep.s, filename = "Geoduck-SRM-screened-tech-rep-CV") #Pushes plot to Plotly online; may need to log in via next 2 lines: 


##### Pull dataset that removes any transitions/samples with cv >20
SRM.reps4stats.20 <- SRM.reps4stats.plots[which(SRM.reps4stats.plots$variance <= 20),]
p.techrep.20 <- plot_ly(data = SRM.reps4stats.20, x = ~Sample, y = ~variance, type="scatter", mode="markers", color=~Protein, hovertext=~paste(Protein, Transition)) %>%  #generate plotly plot
  layout(title="Technical reps with CV =< 20, by sample",
         yaxis = list(title = 'Coefficient of Variance'))
htmlwidgets::saveWidget(as_widget(p.techrep.20), "SRM-screened-tech-rep-CV20.html") #Save plotly plot as html widget 
api_create(p.techrep.20, filename = "Geoduck-SRM-tech-rep-CV20") #Pushes plot to Plotly online; may need to log in via next 2 lines: 
