# Init
source("2020-07-07-deep-learning-sys-rev/init.r")

# Generate search string --------------------------------------------------------------------------

file <- paste0(data_dir, "keywords.csv")
keywords <- fread(file, encoding="UTF-8")

dl <- keywords[dl != ""]$dl
bd <- keywords[biod != ""]$biod
paste(paste0('"', bd, '"'), collapse=", ")

dl <- gsub(" ", "+", dl)
bd <- gsub(" ", "+", bd)

comb <- expand.grid(dl, bd)
comb <- paste0("(", comb$Var1, " AND ", comb$Var2, ")")
paste0(comb, collapse = " OR ")

