
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Section - get distribution
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
print(paste0(Sys.time(), " --- get distribution from global mapper"))

df <- fread(paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_4.3-clean-coll.csv"), integer64='character', na.strings=c('', 'NA'), encoding='UTF-8')

# =================
# DONE ONCE ONLY ##
# =================
# Get countries from global mapper

run_loop <- function() {

    df_mapper <- df[,c("idx", "global.mapper")]
    global_mapper_split <- function(x) {
        x <- strsplit(x, " ")                                   # split by space
        x <- lapply(x, function(x) gsub("(:|\\[)(.+?)$", "", x)) # get words before :
        x <- lapply(x, function(x) gsub(":", "", x))[[1]]        # remove :
        x[!(grepl("\\[|\\]", x) | x=="")]                # remove uncertain countries
    }
    # https://www.hackerearth.com/practice/machine-learning/advanced-techniques/regular-expressions-string-manipulation-r/tutorial/

    df$global.mapper[1308]
    lapply(df$global.mapper[1308], global_mapper_split)
    df$global.mapper[3767]
    lapply(df$global.mapper[3767], global_mapper_split)

    df_mapper$global.mapper.cty <- lapply(df_mapper$global.mapper, global_mapper_split)
    df_mapper$global.mapper.cty[1:5]

    df_mapper2 <- data.frame(idx=character(), country=character())
    for (i in 1:dim(df_mapper)[1]) {
        idx_row <- df_mapper[i]$idx
        cty_row <- df_mapper[i]$global.mapper.cty[[1]]

        if (!identical(cty_row, character(0))) {
            for (j in 1:length(cty_row)) {
                if (is.na(cty_row[j])) {
                    to_merge <- data.frame(idx=idx_row, country=NA)
                } else {
                    to_merge <- data.frame(idx=idx_row, country=cty_row[j])
                    df_mapper2 <- rbind(df_mapper2, to_merge)
                }
            }
        } else {
            to_merge <- data.frame(idx=idx_row, country=NA)
            df_mapper2 <- rbind(df_mapper2, to_merge)
        }
        print(paste0("Row ", i , " completed for ", cty_row[j]))
    }

    df_mapper2 <- merge(df_mapper2, lookup.cty, by.x="country", by.y="A.2", suffix=c(3,4), all.x=T, all.y=F)
    df_mapper2 <- merge(df_mapper2, lookup.cty, by.x="country", by.y="GEC", suffix=c(1,2), all.x=T, all.y=F)

    df_mapper2 <- data.table(df_mapper2)

    df_mapper2[country=="UR"]$Country1 <- "Ukraine"
    df_mapper2[country=="AY"]$Country1 <- "Armenia"
    df_mapper2[country=="RS"]$Country1 <- "Russian Federation"

    write.csv(df_mapper2[,c("country", "idx", "Country1", "Country2")], 
            paste0(dir, "2019-05-23-Apoidea world consensus file Sorted by name 2019 filtered_5-species-cty1.csv"), na='', row.names=F, fileEncoding="UTF-8")
}

if (loop_4 == 'Y'){
    run_loop()
}

# YY, RV, OH, ZC, YA, RZ, KD, WE, KK, OG
