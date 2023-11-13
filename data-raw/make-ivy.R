ivy.df <- read.table("./data-raw/photosynthesis-ivy/A-light-response-1989.txt",
           header = TRUE)
ivy.df$plant <- factor(ivy.df$plant)
summary(ivy.df)

header <- readLines("./data-raw/photosynthesis-ivy/A-light-response-1989.txt",
                    n = 7) |> paste(collapse = "\n")

comment(ivy.df) <- header
cat(comment(ivy.df))

save(ivy.df, file = "./data/ivy-df.rda")
