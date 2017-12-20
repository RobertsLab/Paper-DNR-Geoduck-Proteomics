pairs(data.pepsum.Env.Stats.Trifunctional[,c(7,10:17)], main="Plot Matrix, Trifunctional - Pep1 vs DO Stats", upper.panel = panel.cor)
pairs(data.pepsum.Env.Stats.Trifunctional[,c(7,18:25)], main="Plot Matrix, Trifunctional - Pep1 vs pH Stats")
pairs(data.pepsum.Env.Stats.Trifunctional[,c(7,26:33)], main="Plot Matrix, Trifunctional - Pep1 vs Salinity Stats")
pairs(data.pepsum.Env.Stats.Trifunctional[,c(7,34:41)], main="Plot Matrix, Trifunctional - Pep1 vs Temp Stats")


# Plot growth against Trifunctional Pep abundance
par(mfrow=c(1,3))
plot(data.pepsum.Env.Stats.Trifunctional$Pep1 ~ data.pepsum.Env.Stats.Trifunctional$Perc.Growth, main="Trifunctional Pep1 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.Trifunctional$Pep2 ~ data.pepsum.Env.Stats.Trifunctional$Perc.Growth, main="Trifunctional Pep2 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")
plot(data.pepsum.Env.Stats.Trifunctional$Pep3 ~ data.pepsum.Env.Stats.Trifunctional$Perc.Growth, main="Trifunctional Pep3 ~ Percent Growth", xlab = "Percent Growth", ylab = "Peptide Abundance (lambda-t)")

best.lm.Trifunctional <- lm(`Pep1`+`Pep2`+`Pep3` ~ DO.var.loc + pH.sd.2.loc, data=data.pepsum.Env.Stats.Trifunctional)
summary(best.lm.Trifunctional)
ols_stepwise(best.lm.Trifunctional )
anova(best.lm.Trifunctional)


# Plot peptides against growth & calculate coefficients, R^2, Pvalue. Summary() shows equation with R^2
Trifunctional.Growth <- lm(`Pep1`+`Pep2`+`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Trifunctional)
summary(Trifunctional.Growth )
with(data.pepsum.Env.Stats.Trifunctional, plot(`Perc.Growth`, `Pep1`+`Pep2`+`Pep3`))
abline(Trifunctional.Growth)

Trifunctional.Pep1Growth <- lm(`Pep1` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Trifunctional)
summary(Trifunctional.Pep1Growth)
with(data.pepsum.Env.Stats.Trifunctional, plot(`Perc.Growth`, `Pep1`))
abline(Trifunctional.Pep1Growth)
Trifunctional.Pep2Growth <- lm(`Pep2` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Trifunctional)
summary(Trifunctional.Pep2Growth)
with(data.pepsum.Env.Stats.Trifunctional, plot(`Perc.Growth`, `Pep2`))
abline(Trifunctional.Pep2Growth)
Trifunctional.Pep3Growth <- lm(`Pep3` ~ `Perc.Growth`, data=data.pepsum.Env.Stats.Trifunctional)
summary(Trifunctional.Pep3Growth)
with(data.pepsum.Env.Stats.Trifunctional, plot(`Perc.Growth`, `Pep3`))
abline(Trifunctional.Pep3Growth)


