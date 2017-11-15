# Import Prected RT and actual RT datasets
RT.predicted <- read.csv("../../data/SRM/Predicted-RT-SRM.csv", header=TRUE, na.strings = "#N/A", stringsAsFactors = FALSE)
RT.actual <- read.csv("../../data/SRM/2017-08-11_SRM-Retention-Times.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = "#N/A")
RT.actual.mean <- aggregate(Peptide.Retention.Time ~ Peptide.Sequence + Protein.Name, RT.actual, mean)
RT.table <- merge(x=RT.predicted, y=RT.actual.mean, by.x="PEPTIDE", by.y="Peptide.Sequence")
RT.reg <- lm(RT.table$Peptide.Retention.Time ~ RT.table$PREDICTED.SRM.RT)
RT.reg.sum <- summary(RT.reg)
RT.R2 <- summary(RT.reg)$r.squared
library(plotly)
RT.p <- plot_ly(data=RT.table, x=~PREDICTED.SRM.RT, y=~Peptide.Retention.Time) %>%
  layout(title="Predicted vs. Actual Mean Retention Time \nGeoduck SRM Peptides")