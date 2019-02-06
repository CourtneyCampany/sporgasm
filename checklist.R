### here is a checklist of what is compelted for both measurement campaigns

##FINISHED: all stats in indiviudal scripts in stats folder

#at the moment the final model looks like"
variable ~ niche + site + (1|species)
#this assumes are question is not about individual species effects

#method is too check interaction: niche*site, if not there, then niche + site

#1) frond length (terrestrial > epi/hemi), no site (lots of species variation)
#2) Lamina area (terrestrial greater than epe, hemi is intermediate,
    #lots of species effects)
#3) Stipe length (terrestrial > epi/hemi, minor interaction p=.08)
#####likely need a non parametric with #3

#4) chlorophyll:with full model no diffs, with simple model there is 
#(most variation associated with species effects)

##TODO:

change vcruve function to make unique id from species_id (need to change laselva name)


sla
sd
ss
pv
vcurve
