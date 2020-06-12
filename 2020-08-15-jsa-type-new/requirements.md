# Creating a pipe

## Datasets required in various analyses

1. Go through each file in the folders below
2. List datasets that are using subset.r below
3. List datasets that are not using subset.r below, also put them into subset.r
4. Rename where necessary so that the names are all consistent so that when the new dataset comes, it can be cleaned and piped quickly into the old scripts.



### 2019-06-19-jsa-type-ch1

#### init.r
lp_pop = get_lp_pop()

#### plots_main.r
df = get_df1(write=F)
df2 = get_df2(write=F)
df_publications = get_pub(write=F)

taxonomic_effort = get_n_active_describers_by_year()

#### plots_oth.r
df = get_df1(write=F)
df_describers = get_des(write=F)



### 2019-06-19-jsa-type-ch2

#### prep1.r
df = get_df1(write=F)

dat = get_dis()
shp_grp = get_shp_biogeo()
shp_grp = get_shp_biomes()



### 2019-06-19-jsa-type-ch3-coauth

#### util.py
data_dir = '../data/'

ddir = data_dir + "2019-05-23-ascher-bee-data/"
basefile = '2019-05-23-Apoidea world consensus file Sorted by name 2019 '
fn_nodes = ddir + basefile + 'describers_5.0-describers-final.csv'
fn_edges = ddir + basefile + 'describers_7.0-author-networks.csv' 
fn_auth = ddir + basefile + 'describers_4.0-denormalised2.csv'
fn_spp = ddir + basefile + 'describers_2.0-denormalised.csv'

ldir = data_dir + "lookup/"
fn_statoids = ldir + '2019-05-29-statoid-country-codes.csv'

#### plots_main.r
df0a = get_df1(write=F)
df0b = get_df2(write=F)

#### plots_oth.r
nw = get_describer_network()


### 2019-06-19-jsa-type-ch3-flow

#### analysis1b.r
auth = get_des(write=F)

#### data_analysis1b.r
lu = get_lp_statoid()
lu_col = get_lp_col()
lu_adj = get_lp_adj_countries()

### 2019-06-19-jsa-type-ch3-gender

#### model.r
auth_full <- get_des(write=F)

dat = get_species_denormalised()

lu = get_lp_statoid()

#### data-un.r
lu = get_lp_statoid()


## TODO: Identify fields that need to be cleaned

## variables cleaned with new names

### used
date = date.n
type.country = type.country.n
type.country = type.country.n.full (lookup)

### not used
type.state = type.state.n
type.state = type.state.n.full
date.of.type = date.of.type.string
date.of.type = date.of.type.dd
date.of.type = date.of.type.mm
date.of.type = date.of.type.yyyy
date.of.type = date.of.type.yyyy
type.repository = type.repository.n_short
type.repository = type.repository.n_long 
country.of.type.repository = country.of.type.repository.n_long

## variables cleaned but with old names

SHOULD BE NONE!


## new variables
### used
flag
source.of.latlon.n
duplicated.row

N_synonyms
N_ss
N_var

### not used
years.lag

# derived datasets

## used only for cleaning purposes
lat-lon-edit.csv for initial cleaned lat/lon*
df-state-check_edit.csv for correspondence of lat/lon to country/state*
df-geocoded_edit.csv for new geocoded lat/lon*
df-check-state2_edit.csv to add country/state for those with lat/lon but no country/state*

check-type-repo2_edit.csv for repositories*

date_discrepancy.csv for years.lag <0*
missing_authors_edit.csv for missing authors*

idx-idx_original.csv which is the coloured version of excel converted into csv

journal_names_edit.csv for journal names

collectors_2.0-denormalised_edit4.csv to clean collectors which is fed back into the main collector file

describer_edits.csv for edited describers

last_name.csv for last names


# others
nonvalid species: 2019-05-23-Apoidea world consensus file Sorted by name 2019 oth_4.3-clean-coll
describers: 2019-05-23-Apoidea world consensus file Sorted by name 2019 describers_5.0-describers-final-view.csv
collectors: 2019-05-23-Apoidea world consensus file Sorted by name 2019 collectors_3.0-collectors.csv 
