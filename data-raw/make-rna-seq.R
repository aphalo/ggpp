library(dplyr)
load("data-raw/rna-seq-arabidopsis/volcano-test-data.Rda")
UV350nm_short.df %>%
  filter(genotype == "Ler") %>%
  select(tag, gene, outcome, logFC, PValue, genotype) -> volcano_example.df
nrow(volcano_example.df)
rm(UV350nm_short.df)
save(volcano_example.df, file = "data/volcano-example-df.rda")

load("data-raw/rna-seq-arabidopsis/quadrat-plots-data.Rda")
UV350pm.df %>%
  filter(genotype == "Ler" & !is.na(gene)) %>%
  select(tag, gene,  outcome.x = outcome.long.wb, outcome.y = outcome.short.wb,
         logFC.x = logFC.long.wb, logFC.y = logFC.short.wb, genotype) %>%
  sample_frac(size = 1/6) -> quadrant_example.df
nrow(quadrant_example.df)
rm(UV350pm.df)
save(quadrant_example.df, file = "data/quadrant-example-df.rda")
