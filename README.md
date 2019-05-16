# README

Note: README is written in the first-person voice.

This repository contains all the code to do with my masters thesis, in order for the research to be reproducible.

## How I arranged the files

Some general guidelines that I follow:

- My main purpose for this repository is more for personal use so that I can share code between my computers and to backup important artifacts, such as images. Thus, I will include images in `plots/` folder.
- All "simpler" or exploratory analyses will be stored in the folder itself as `.R` or `.ipynb`.
- The aforementioned analyses share a common data folder, with three subfolders `01-in`, `02-out`, `03-clean` which represent the three steps taken in data wrangling.
- More complex, standalone analyses will be stored in its own folder named as so `<yyyy>-<mm>-<dd>-<topic>` and its own structure which should consist of at least a `data` folder.
- Commits made by me (Eunice) will be prepended with `[EJYS] <commit message>`.
- For efficiency purposes, I work from one conda environment, instead of creating one for each analyses, as the tools likely to be used are common to most of them. However, if the analyses is niche and requires such unique environment, a `requirements.txt` will be output.
- Standard files such as `.gitignore` and `README.md` are also present.

Last updated 16 May 2019