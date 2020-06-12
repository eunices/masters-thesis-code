# Initialize
source("2020-08-15-jsa-type-v2/init/var.r")

source(paste0(dir_ref, "init/util.r"))
source(paste0(dir_ref, "subset.r"))
source(paste0(dir_ref, "clean/functions.r"))

# TODO: may need to handle line carriages/ and cleaning data here


# Have a table old v new variables
variables_v1 = names(rename_df_names(get_raw_data()))
# variables_v2 = names(rename_df_names(read_escaped_data(paste0(v2_dir_data_raw, v2_basefile, ".csv"))))
variables_v2 = c("date.z", "type.country", "type.states", "author")



# Fuzzy match the old v new variables after transformation
variables_v2_matches = 
  lapply(variables_v2, function(word) closestWordMatch(word, variables_v1))
variable_v2_matches = cbind(new=variables_v2, old=variables_v2_matches)



# Output this table
write.csv(paste0(dir_data_raw_clean, "map_names.csv"), na='', row.names=F, fileEncoding="UTF-8")



# !MANUAL: Check through this table
matched_names = fread(paste0(dir_data_raw_clean, "map_names_edit.csv"))


# Replace the name in dataset

for (i in 1:dim(matched_names)[1]) {
	old_name = matched_names[i,]$old
	new_name = matched_names[i,]$new
	if (new_name != old_name) names(df)[which(names(df)==old_name)] = new_name
}





# Output data
write.csv(paste0(v2_dir_data_raw, v2_basefile, "_1 renamed.csv"),
		      na='', row.names=F, fileEncoding="UTF-8")



