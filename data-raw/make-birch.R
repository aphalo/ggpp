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
comment(birch.df) <- "Data for silver birch seedlings."

colnames(birch.df)
stem.df <- birch.df[ , c("Container", "Density",   "block", "dwstem")]
colnames(stem.df)[4] <- "dry.weight"
stem.df$Part <- "stem"
root.df <- birch.df[ , c("Container", "Density",   "block", "dwroot")]
colnames(root.df)[4] <- "dry.weight"
root.df$Part <- "root"
birch_dw.df <- rbind(stem.df, root.df)
birch_dw.df$Part <- factor(birch_dw.df$Part, levels = c("stem", "root"))
rm(stem.df, root.df)
colnames(birch_dw.df)
comment(birch.df) <- "Data for silver birch seedlings."

save(birch.df, birch_dw.df, file = "data/birch.rda")
