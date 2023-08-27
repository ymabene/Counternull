# Loads sample matrix
sample_matrix <- read.table(file ="C:\\Users\\yasmi\\OneDrive\\My Documents\\Harvard2021\\sample_matrix.rda")
newColumnNames <- paste("w", 1:1000, sep = "") # change names
colnames(sample_matrix) <- newColumnNames
usethis::use_data(sample_matrix, compress = "xz")
