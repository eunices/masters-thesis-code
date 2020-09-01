# Initialize
source('2020-08-31-jsa-type-v2/init/init.r')


# To use old functions from v1
# source(paste0(v2_dir_script, "/init/util.r"))
# source(paste0(v2_dir_script, "subset.r"))
# source(paste0(v2_dir_script, "/clean/functions.r"))


# Have a table old v new variables

# variables_v1 = names(rename_df_names(get_raw_data()))
file <- paste0(v2_dir_data_raw_clean,"old-names.csv")
# fwrite(data.table(variables_v1), file)
variables_v1 = fread(file)$variables_v1

variables_v2 = names(read_escaped_data_v2(
	paste0(v2_dir_data_raw, v2_basefile, "_1 format.csv")
))


# Fuzzy match the old v new variables after transformation
variables_v2_matches = lapply(variables_v2,
	function(word) closestWordMatch(word, variables_v1)
)

variable_v2_matches = cbind(new = variables_v2, old = variables_v2_matches)


# Output this table
fwrite(
	data.table(variable_v2_matches),
	paste0(v2_dir_data_raw_clean, "map-names.csv"), 
	na = ''
)



# !MANUAL: Check through this table
matched_names = fread(paste0(v2_dir_data_raw_clean, "map-names_edit.csv"))


# Remove those columns that did not exist in old dataset
matched_names = matched_names[old != ""]


# Replace the name in dataset

for (i in 1:dim(matched_names)[1]) {
	old_name = matched_names[i,]$old
	new_name = matched_names[i,]$new
	if (new_name != old_name) names(df)[which(names(df)==old_name)] = new_name
}


# Output data
write.csv(
	df,
	paste0(v2_dir_data_raw, v2_basefile, "_2 map.csv"),
	na = '', 
	row.names = F, 
	fileEncoding = "UTF-8"
)



