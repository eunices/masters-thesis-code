source('2020-08-31-jsa-type-v2-ch2/01-prep/init.r')
print(paste0(Sys.time(), " --- make dataset"))

if (model_params$dataset == "BG") {        # biogeographic realm
    source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/bgy.r")
} else if (model_params$dataset == "BM") { # biome
    source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/bmy.r")
} else if (model_params$dataset == "LT") { # latitude
    # source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/lty.r")
} else if (model_params$dataset == "FA") { # family
    source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/fam.r")
} else if (model_params$dataset == "HA") { # halictidae
    source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/hal.r")
} else if (model_params$dataset == "GE") { # genus
    source("2020-08-31-jsa-type-v2-ch2/01-prep/prep1/gen.r")
}