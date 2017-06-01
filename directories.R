##### Directories creation #####


working_directory <- getwd()
temp_dir <- 'Downloaded files'
data_dir <- 'Processed data'
graphs_dir <- 'Plots'
dir.create(file.path(working_directory, temp_dir))
dir.create(file.path(working_directory, data_dir))
dir.create(file.path(working_directory, graphs_dir))