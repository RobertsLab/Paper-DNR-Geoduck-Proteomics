pairs(data.pepsum.Env.Stats.Puromycin[,c(7,10:17)], main="Plot Matrix, Puromycin - Pep1 vs DO Stats", upper.panel = panel.cor)
pairs(data.pepsum.Env.Stats.Puromycin[,c(7,18:25)], main="Plot Matrix, Puromycin - Pep1 vs pH Stats")
pairs(data.pepsum.Env.Stats.Puromycin[,c(7,26:33)], main="Plot Matrix, Puromycin - Pep1 vs Salinity Stats")
pairs(data.pepsum.Env.Stats.Puromycin[,c(7,34:41)], main="Plot Matrix, Puromycin - Pep1 vs Temp Stats")


# Plot growth against Puromycin Pep abundance
par(mfrow=c(1,3))
plot(data.pepsum.Env.Stats.Puromycin$Pep1 ~ data.pepsum.Env.Stats.Puromycin$Perc.Growth, main="Puromycin Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.Puromycin$Pep2 ~ data.pepsum.Env.Stats.Puromycin$Perc.Growth, main="Puromycin Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.Puromycin$Pep3 ~ data.pepsum.Env.Stats.Puromycin$Perc.Growth, main="Puromycin Pep3 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")

best.lm.Puromycin <- lm(`Pep1`+`Pep2`+`Pep3` ~ pH.sd.2.loc, data=data.pepsum.Env.Stats.Puromycin)
summary(best.lm.Puromycin)
ols_stepwise(best.lm.Puromycin )
anova(best.lm.Puromycin)

# Plot peptides against growth & calculate coefficients, R^2, Pvalue. Summary() shows equation with R^2
Puromycin.Growth <- lm(`Pep1`+`Pep2`+`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Puromycin)
summary(Puromycin.Growth )
with(data.pepsum.Env.Stats.Puromycin, plot(`Perc.Growth`, `Pep1`+`Pep2`+`Pep3`))
abline(Puromycin.Growth)

Puromycin.Pep1Growth <- lm(`Pep1` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Puromycin)
summary(Puromycin.Pep1Growth)
with(data.pepsum.Env.Stats.Puromycin, plot(`Perc.Growth`, `Pep1`))
abline(Puromycin.Pep1Growth)
Puromycin.Pep2Growth <- lm(`Pep2` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Puromycin)
summary(Puromycin.Pep2Growth)
with(data.pepsum.Env.Stats.Puromycin, plot(`Perc.Growth`, `Pep2`))
abline(Puromycin.Pep2Growth)
Puromycin.Pep3Growth <- lm(`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Puromycin)
summary(Puromycin.Pep3Growth)
with(data.pepsum.Env.Stats.Puromycin, plot(`Perc.Growth`, `Pep3`))
abline(Puromycin.Pep3Growth)


