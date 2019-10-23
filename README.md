# README

This repository contains all the code to do with my masters thesis for reproducible research.

## Folder structure

General folder structure guidelines:

- All "simpler" or exploratory analyses will be stored in the folder itself as `.R` or `.ipynb`.
- More complex, standalone analyses will be stored in its own folder named as so `<yyyy>-<mm>-<dd>-<topic>` and its own structure which should consist of at least a `data` folder.
- Commits made by me (Eunice) will be prepended with `[EJYS] <commit message>`.
- `tmp/` is a folder for read/write purposes of temporary datasets.

Specific files:

- For efficiency purposes, one conda environment is used and its dependencies should be in `requirements.txt` in the main folder. However, if the analyses is niche and requires such unique environment, a `requirements.txt` will be found within the analyses' folder.
- Standard files such as `.gitignore` and `README.md`.
- `backup_*.sh` and `recover_*.sh` enables me to backup my data to Dropbox.
- `keys.r.template` is a template for `keys.r` which contain API keys and working directory used in R.
- To facilitate faster environment setup, a few scripts have been written respectively for `R` and `python`:
    - `install.r` is a file used to parse for all `.R` scripts for packages and install packages that are not already installed, and to be used after following steps in `setup_r.sh` (whether manually or otherwise); this is in contrary to python where
    - `environment-py.yaml` is the holder of all packages to be installed with accompanying script `setup_py.sh`. This means packages are installed based on environment-py-msc.yaml instead of parsing through all `.py` files for libraries.

Setting up:

- Get code: ```git clone -–depth 1 https://github.com/eunices/masters-thesis-code.git ```
- Get data: from external hardisk, or some cloud storage [to be updated]

## Analyses notes

### Edie et al's issues with bee count data

1. Problem 1: chains not mixed

```Warning messages:
1: package ‘dplyr’ was built under R version 3.6.1 
2: package ‘maptools’ was built under R version 3.6.1 
3: There were 969 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
4: Examine the pairs() plot to diagnose sampling problems
5: The largest R-hat is 1.79, indicating chains have not mixed.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#r-hat 
6: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#bulk-ess 
7: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
Running the chains for more iterations may help. See
http://mc-stan.org/misc/warnings.html#tail-ess
```

```fit <- stan( file="2019-07-15-edie-et-al/zip_count.stan",
                   data=data,
                   chains=4,
                   warmup=2500,
                   iter=5000,
                   init=0,
                   thin=5,
                   cores=4,
                   verbose=TRUE, seed=301,
                   control = list(max_treedepth = 15))```

Note: global dataset used

- Values are vastly different from Edie et al [range to 10] and bees [range to 200+]; not zero-inflated?

2. Problem 2: Does not work for "single" column (using year only).

### Calculating areas

- Equal-area projection used is EPSG 3035
- All countries with exception of 5 (Mali, Mexico, Niger, Pakistan, Yemen) all were >60% in area for biogeographic realm
- All countries with exception of 10 (Argentina, Bangladesh, Chile, China, India, Mexico, Paraguay, Saint Helena, Saudi Arabia, Syria) all were >60% in area for latitude

### Ecoregions shp file

- Ecoregions 2017 shp file cannot be dissolved by BIOME_NAME due to 8 topology errors that are not trivial to resolve automatically.

Last updated 23 Oct 2019
