library(ape)


mytree <- read.nexus.data("phylogeny/alignment_v1.nexus")
library(tidytree)

test <- as_tibble(mytree)
test2 <- as.phylo(test)

plot(test, no.margin=TRUE)


library(Rphylip)

library(phytools)
test3 <- read.tree("phylogeny/alignment_v1x.phy")
plotTree(test3)
