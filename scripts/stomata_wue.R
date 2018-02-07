source("master_scripts/plot_objects.R")

iso <- read.csv("calculated_data/isotopes_species.csv")

stodens <- read.csv("calculated_data/stomden_species_means.csv")
  
wue <- merge(iso, stodens)

#wue and stomata density
wue_mod <- lm(d13C ~ sto_mm2.m2, data=wue)

visreg(wue_mod)
residualPlot(wue_mod)
qqPlot(wue_mod)

summary(wue_mod)
anova(wue_mod) #no effect of stomdens on longterm wue
