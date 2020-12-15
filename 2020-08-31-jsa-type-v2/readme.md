# How to use this folder

- Update variables in `00-init/var.r`.
- Prep data using `01-prep/main.r` by formatting names (`format.r`) and renaming new names with old names (`map.r`). This outputs a file with `<base file>_2.csv`
- Make checks of the data using `02-check/main.r`. Outputs checks to a folder with `data/2019-05-23-ascher-bee-data/<version>/check/`. Go through this to see if the data has any issues.
- Clean the data using `03-clean/main.r`. If it is the first time, make manual edits. 
- Use the pipeline `main.r` to call the entire pipeline to create cleaned data used in analyses later (called with functions in `subset.r`).

Last updated 15 December 2020