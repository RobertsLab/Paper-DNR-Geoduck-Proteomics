# Import Prected RT and actual RT datasets
RT.predicted <- read.csv("../../data/SRM/Predicted-RT-SRM.csv", header=TRUE, na.strings = "#N/A", stringsAsFactors = FALSE)
RT.actual <- read.csv("../../data/SRM/2017-08-11_SRM-Retention-Times.csv", header=TRUE, stringsAsFactors = FALSE, na.strings = "#N/A")
RT.actual.mean <- aggregate(Peptide.Retention.Time ~ Peptide.Sequence + Protein.Name, RT.actual, mean)
RT.table <- merge(x=RT.predicted, y=RT.actual.mean, by.x="PEPTIDE", by.y="Peptide.Sequence")
RT.reg <- lm(RT.table$Peptide.Retention.Time ~ RT.table$PREDICTED.SRM.RT)
RT.reg.sum <- summary(RT.reg)
RT.reg$coefficients
RT.R2 <- summary(RT.reg)$r.squared
png(filename = "../../analyses/SRM/SRM-Predicted-vs-Actual-RT-plot.png")
plot(Peptide.Retention.Time ~ PREDICTED.SRM.RT, data=RT.table, main="Predicted vs. Actual Mean Retention Time \nGeoduck SRM Peptides")
abline(RT.reg$coefficients[1], RT.reg$coefficients[2])
legend("bottomright", inset=0.05, bty="n", legend=paste("R2-adjusted: ", format(summary(RT.reg)$adj.r.squared, digits=4), "\nCoefficient", format(RT.reg$coefficients[2], digits=4)))
dev.off()
