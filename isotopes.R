#format isotope data
m2 <- function(x) mean(x, na.rm=TRUE)

niche <- read.csv("raw_data/species_niches.csv")

iso <- read.csv("raw_data/fern_isotopes.csv")
iso <- merge(iso, niche, by="species")

#determine elemental%, sample is in mg, C,N is in ug

iso$c_perc <- with(iso, 1-((sample_mg-(c_ug/1000))/sample_mg))
iso$n_perc <- with(iso, 1-((sample_mg-(n_ug/1000))/sample_mg))

iso_agg <- doBy::summaryBy(d13C+d15N+c_perc+n_perc ~
                           site+species+niche, data=iso, FUN=m2, 
                           keep.names = TRUE)
write.csv(iso_agg, "calculated_data/isotopes_species.csv", row.names = FALSE)

####decide to run on raw samples or means

#cperc by niche
cmod <- lm(c_perc~niche, data=iso_agg)
summary(cmod)
anova(cmod)  
visreg(cmod) #no difference

#nperc by niche
nmod <- lm(n_perc~niche, data=iso_agg)
summary(nmod)
anova(nmod)  
visreg(nmod) #no difference

#c13 by niche
cisomod <- lm(d13C~niche, data=iso_agg)
summary(cisomod)
anova(cisomod)  
visreg(cisomod) #epiphyte more less negative

#n15 by niche
nisomod <- lm(d15N~niche, data=iso_agg)
summary(nisomod)
anova(nisomod)  
visreg(nisomod) #epiphyte more less negative
tukey_n15 <- glht(nisomod, linfct = mcp(niche = "Tukey"))
n15_siglets <-cld(tukey_n15)
n15_siglets2 <- n15_siglets$mcletters$Letters

#climber way different but low samples??? terrestrial not different from epi
