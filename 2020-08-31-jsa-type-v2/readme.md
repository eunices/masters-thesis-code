# How to use this folder

Note: always open .csv with "UTF-8" encoding

- Update variables in `00-init/var.r`.

- Prep data using `01-prep/main.r` by formatting names (`format.r`) and renaming new names with old names (`map.r`). This outputs a file with `<base file>_2.csv`

- Make checks of the data using `02-check/main.r`. Outputs checks to a folder with `data/2019-05-23-ascher-bee-data/<version>/check/`. Go through this to see if the data has any issues and be aware, even if I can't fix them.

- Clean the data using `03-clean/main.r`. If it is the first time, make manual edits. Some checks are incorporated into clean for the ease of checking but will be written to the `check/` not `clean/` folder. The latter is for manual cleaning purposes with manual cleaning performed and appended with `_edit` in `.csv` files.

Manual cleaning stuff:

DONE 16/12/2020 1. Clean country and lat/lon mismatch (clean01.r)

DONE 17/12/2020 2. Create lookup file for authors and surname `lp-surname.csv` (clean02.r)

DONE 17/12/2020 3. Clean full name author inconsistency (clean02.r)

DONE 17/12/2020 4. Clean full name author and author mismatch (clean02.r)

DONE 18/12/2020 5. Clean journal names (clean04.r)
* new series considered separate e.g. due to [mergers](https://en.wikipedia.org/wiki/Annales_de_la_Soci%C3%A9t%C3%A9_Entomologique_de_France)
* alternative names merged as one, e.g. based on [BHL](https://www.biodiversitylibrary.org/bibliography/8097#/summary)
* supplement is subset of journal

DONE 21/12/2020 6. Clean journal associated info (clean04.r)
* check for city as subset of country
* use most updated location
* consistency of journal country and city

DONE 22/12/2020 7. Clean publication duplicates (clean04.r)

* leave page numbers out of counting unique publications (referring to specific pages of section in which description is found)
* clean "author" field to original publication (instead of author in publication); if "paper.editors" exists, use it in place of "paper.authors" as it is one publication (remove "In[i]") (created  `paper.authors_n_edit` to reflect this)
* treat book sections, books and encylopedia as books (created `paper.type_n` to reflect this)

DONE 23/12/2020 8. Clean biodata of authors (clean05.r)

* update `lp-auth-bio-v1.csv` with new names
* most important fields to add are current location of residence and whether alive as of 2019

- Use the pipeline `main.r` to call the entire pipeline to create cleaned data used in analyses later (called with functions in `subset.r`).

Last updated 7 Jan 2020