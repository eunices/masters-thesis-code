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

dat = get_species_country_distribution()
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





## TODO: Identify fields that need to be cleaned

Check clean folder.

## TODO: Identify functions to be reused from cleaning scripts

Ideally the derived datasets.