print(paste0(Sys.time(), " --- Describers profiles (for appendix)"))

# For describer metrics, see df01.r in the cleaning scripts.
# Data for describers is "basefile" + "-describers_2.csv"

# Filter describers with dod/dob and
# those who described at least 1 syn or valid sp.

des <- df_describers[
    (ns_spp_N + syn_spp_N) >= 1 & 
    !(is.na(dod.describer) | is.na(dob.describer)), 
    c(
        "full.name.of.describer", "last.name", 
        "dob.describer", "dod.describer", "max", "spp_N", 
        "spp_per_pub_mean", "n_pubs",
        "spp_per_pub_mean_20y", "n_pubs_20y",
        "spp_per_pub_mean_5y", "n_pubs_5y",
        "n_spp_20y", "n_spp_5y"
    )
]

# missing variables:
# spp_per_pub_mean, n_pubs, 
# n_spp_20y, 

# n_spp_5y, pub_years
# spp_per_pub_mean_20y, n_pubs_20y, 
# spp_per_pub_mean_5y, n_pubs_5y, 

des$years_last_pub_death <- des$dod.describer - des$max
des$age_at_death <- des$dod.describer - des$dob.describer

mround <- function(x,base) base*round(x/base)

des$date.century <- substr(
    as.character((des$dod.describer - des$dob.describer)/2+des$dob.describer),
    1, 3
)

des[((des$dod.describer - des$dob.describer)/2+des$dob.describer) <1000]

des$date.century <- mround(as.numeric(des$date.century), 5)
des$date.century <- paste0(des$date.century, "0s")

des$check_young <- ifelse(des$age_at_death >=40, "T", "F") # young
des$check_few.spp <- ifelse(des$spp_N <= 12, "T", "F") # few spp (than median)
des$check_few.spp.pub <- ifelse(des$spp_per_pub_mean <=10, "T", "F") # few sp/pb
des$check_few.pub <- ifelse(des$n_pubs <=3, "T", "F") # few pub

table(is.na(des$check_young))
table(is.na(des$check_few.spp))
table(is.na(des$check_few.spp.pub))

des$check_few.pub.near.death <- ifelse(
    is.na(des$n_spp_20y), "T", ifelse(
    des$n_spp_20y/des$spp_N >=0.8, "F", "T")
) # few pub

summary(des$age_at_death)

des1 <- des[, 
    list(
        names=paste0(full.name.of.describer, collapse="; "),
        med_age_at_death=median(as.numeric(age_at_death), na.rm=T),
        med_spp_N = median(spp_N, na.rm=T),
        med_spp_per_pub_mean = median(spp_per_pub_mean, na.rm=T),
        med_prop_20y = median(n_spp_20y/spp_N, na.rm=T)
    ),
    by = c(
        "check_young", "check_few.spp", 
        "check_few.spp.pub", "check_few.pub.near.death"
    )
]

des2 <- des[, list(.N), by=c(
    "check_young", "check_few.spp", 
    "check_few.spp.pub", "check_few.pub.near.death", 
    "date.century"
)]

des3 <- dcast(
    des2, 
    
    check_young + check_few.spp.pub + 
    check_few.pub.near.death + check_few.spp ~ date.century, 

    value.var="N"
)

des3[is.na(des3)] <- 0

des4 <- merge(
    des1, des3, by=c(
        "check_young", "check_few.spp.pub", 
        "check_few.pub.near.death", "check_few.spp"
    )
)

fwrite(
    des4, paste0(dir_table_ch1, '2019-10-02-taxonomist-one-large-mono.csv'),
    row.names=F
)

# Checks
table(is.na(des$check_young))
table(is.na(des$check_few.spp.pub))
table(is.na(des$check_few.spp))
table(is.na(des$check_few.pub.near.death))

# Copy and paste into report

# Table 1

# Caption
dim(des)
dim(des[check_young == "T"])
dim(des[check_young == "T" & check_few.pub.near.death == "T"])
auth_throughout_life <- table(des[check_young=="T",]$check_few.pub.near.death)
prop.table(auth_throughout_life)*100

# First row
des[check_young == "F", 
    list(
        med_age_at_death=median(age_at_death, na.rm=T),
        med_spp_N = median(as.numeric(spp_N), na.rm=T),
        med_spp_per_pub_mean = median(spp_per_pub_mean, na.rm=T),
        med_prop_20y = median(n_spp_20y/spp_N, na.rm=T),
        N = .N
    ),
by=c("check_young")]

# Second row
des[check_young == "T" & check_few.spp.pub == "F", 
    list(
        med_age_at_death=as.numeric(median(age_at_death, na.rm=T)),
        med_spp_N = as.numeric(median(spp_N, na.rm=T)),
        med_spp_per_pub_mean = as.numeric(median(spp_per_pub_mean, na.rm=T)),
        med_prop_20y = as.numeric(median(n_spp_20y/spp_N, na.rm=T)),
        N = .N
    ),
    by=c("check_few.pub.near.death")
]

# Group that died young, few publications, few species, average number of species per pub
# Group that did not die young, few publications, few species, average number of species per pub
# Group that did not die young, large number of publications, large number of species, large number of species per pub
# Group that did not die young, few publications, large number of species {N and when}

