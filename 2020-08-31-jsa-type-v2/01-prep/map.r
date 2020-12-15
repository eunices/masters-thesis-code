# Initialize
source('2020-08-31-jsa-type-v2/00-init/main.r')


# To use old functions from v1
# source(paste0(v2_dir_script, "/00-init/util.r"))
# source(paste0(v2_dir_script, "subset.r"))
# source(paste0(v2_dir_script, "/03-clean/functions.r"))


# Have a table old v new variables

# variables_v1 = names(rename_df_names(get_raw_data()))
file <- paste0(v2_dir_data_raw_clean,"map-old-names.csv")
# fwrite(data.table(variables_v1), file)
variables_v1 = fread(file)$variables_v1

variables_v2 = names(read_escaped_data_v2(
	paste0(v2_dir_data_raw, v2_basefile, "_1.csv")
))


# Fuzzy match the old v new variables after transformation
variables_v2_matches = lapply(variables_v2, closestWordMatch, variables_v1)
variable_v2_matches = cbind(new = variables_v2, old = variables_v2_matches)


# Output this table
fwrite(
	data.table(variable_v2_matches),
	paste0(v2_dir_data_raw_clean, "map-names.csv"), 
	na = ''
)



# !MANUAL: Check through this table
cfile <- paste0(v2_dir_data_raw_clean, "map-names_edit.csv")
if(file.exists(cfile)) {
	matched_names <- fread(cfile)

	# Do not match columns that did not exist in old dataset
	# These are new columns that will remain in the dataset
	matched_names = matched_names[old != ""]

	# Replace the new name (from the updated dataset) with the old name
	for (i in 1:dim(matched_names)[1]) {
		old_name = matched_names[i,]$old
		new_name = matched_names[i,]$new
		if (new_name != old_name) {
			print(
				paste0("OLD: ", old_name, " replaced NEW: ", new_name, "  @", i)
			)
			names(df)[which(names(df) == new_name)] <- old_name
		}
	}

}

# Output data
fwrite(
	df,
	paste0(v2_dir_data_raw, v2_basefile, "_2.csv"),
	na = '', 
	row.names = F, 
)



