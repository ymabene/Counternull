# Loads sample data
sample_data <- read.table(file ="C:\\Users\\yasmi\\OneDrive\\My Documents\\Harvard2021\\sample_data.csv", header = T, sep = ",", stringsAsFactors = T)
readr::write_csv(sample_data, file="inst/extdata/sample_data.csv")
usethis::use_data(sample_data, compress = "xz")



