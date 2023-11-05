library(foreign)

birch.df <- read.systat("data-raw/birch-1993/BI93DENS.SYS")

colnames(birch.df) <- tolower(colnames(birch.df))

birch.df$Container <-
  factor(birch.df$containe, labels = c("Small", "Large"))
birch.df$Density <-
  factor(birch.df$density, labels = c("Dense", "Medium", "Sparse"))

colnames(birch.df)
birch.df <-
  birch.df[ , c("Container", "Density", "block", "height",
                "diameter", "dwstem", "dwroot", "healthy")]
colnames(birch.df)
nrow(birch.df)
birch.df <- na.omit(birch.df)
nrow(birch.df)

save(birch.df, file = "data/birch.rda")
