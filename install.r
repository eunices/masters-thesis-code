# Installs all R packages. Not R itself.
# Need to ensure that JAVA_HOME environment variable is already set.
# Set R_LIBS_USER also for new library path

# Read all R files
r_files <- list.files('.', pattern=".r$", recursive=T)
lib_li <- c()
for (i in 1:length(r_files)) {
    libs <- readLines(r_files[i])[-1]
    libs <- libs[grepl("library\\(", libs)]
    lib_li <- c(lib_li, libs)
}

lib_li <-gsub("#.*", "", lib_li) # remove all words after hash
lib_li <- gsub("suppress(Warnings|Messages)\\(library\\(|library\\(|\\)", "", lib_li) # remove functions
lib_li <- unique(unlist(strsplit(lib_li, "; ")))

install_libs <- function(packages) {
    packages <- unique(lib_li)
    packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
    if(length(packages_to_install)) install.packages(packages_to_install)
}

install_libs(lib_li)