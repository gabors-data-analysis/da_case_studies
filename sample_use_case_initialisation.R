source("global.R")

use_case_dir <- file.path("ch22-airline-merger-prices/")
loadLibraries(use_case_dir)

data_in <- paste(data_dir,"airline-tickets-usa","clean", sep = "/")

data_out <- use_case_dir
output <- paste0(use_case_dir,"output/")
create_output_if_doesnt_exist(output)