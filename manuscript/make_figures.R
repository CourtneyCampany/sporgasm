to_pdf <- function(expr, filename, ..., verbose=TRUE) {
  if(!file.exists(dirname(filename)))
    dir.create(dirname(filename), recursive=TRUE)
  if ( verbose )
    cat(sprintf("Creating %s\n", filename))
  pdf(filename, ...)
  on.exit(dev.off())
  eval.parent(substitute(expr))
}


fn <- function(...)file.path("manuscript",...)

to_pdf(source("master_scripts/figure_1.R"), fn("Figure1.pdf"), width=12, height=6)
to_pdf(source("master_scripts/figure2.R"), fn("Figure2.pdf"), width=12, height=6)
to_pdf(source("master_scripts/figure3.R"), fn("Figure3.pdf"), width=8, height=8)
to_pdf(source("master_scripts/figure4.R"), fn("Figure4.pdf"), width=12, height=6)
to_pdf(source("master_scripts/figure5.R"), fn("Figure5.pdf"), width=12, height=12)
# to_pdf(source("phylogeny/stipe_phylo.R"), fn("Figure6.pdf"), width=8, height=8)
# to_pdf(source("phylogeny/stomata_phylo.R"), fn("Figure7.pdf"), width=8, height=8)
