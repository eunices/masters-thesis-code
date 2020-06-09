# Datasets required in various analyses

1. Go through each file in the folders below
2. List datasets that are using subset.r below
3. List datasets that are not using subset.r below, also put them into subset.r
4. Rename where necessary so that the names are all consistent so that when the new dataset comes, it can be cleaned and piped quickly into the old scripts.

## 2019-06-19-jsa-type-ch1

### plots_main.r
df = get_df1(write=F)
df2 = get_df2(write=F)
df_publications = get_pub(write=F)

taxonomic_effort = get_n_active_describers_by_year()

### plots_oth.r
df = get_df1(write=F)
df_describers = get_des(write=F)



TODO: do the rest of the folders

## 2019-06-19-jsa-type-ch2


## 2019-06-19-jsa-type-ch3-coauth


## 2019-06-19-jsa-type-ch3-flow


## 2019-06-19-jsa-type-ch3-gender

