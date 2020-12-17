# How to use this folder

- Update variables in `00-init/var.r`.

- Prep data using `01-prep/main.r` by formatting names (`format.r`) and renaming new names with old names (`map.r`). This outputs a file with `<base file>_2.csv`

- Make checks of the data using `02-check/main.r`. Outputs checks to a folder with `data/2019-05-23-ascher-bee-data/<version>/check/`. Go through this to see if the data has any issues and be aware, even if I can't fix them.

- Clean the data using `03-clean/main.r`. If it is the first time, make manual edits. Some checks are incorporated into clean for the ease of checking but will be written to the `check/` not `clean/` folder. The latter is for manual cleaning purposes with manual cleaning performed and appended with `_edit` in `.csv` files.

Manual cleaning stuff:

DONE 16/12 1. Clean country and lat/lon mismatch (clean01.r)
DONE 17/12 2. Create lookup file for authors and surname `lp-surname.csv` (clean02.r)
DONE 17/12 3. Clean full name author inconsistency (clean02.r)
DONE 17/12 4. Clean full name author and author mismatch (clean02.r)
TODO 18/12 5. Clean journal names (clean04.r)
TODO 21/12 6. Clean journal associated info (clean04.r)
TODO 22/12 7. Clean publication duplicates (clean04.r)
TODO 23/12 8. Clean biodata of authors (clean05.r)

- Use the pipeline `main.r` to call the entire pipeline to create cleaned data used in analyses later (called with functions in `subset.r`).

1. Chapter 2: come up list of models to run
2. Chapter 1: for different biogeographic regions
3. Chapter 3: update results

Last updated 15 December 2020