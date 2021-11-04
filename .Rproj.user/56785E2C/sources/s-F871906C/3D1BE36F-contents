# Loads sample police data
# Data taken from "https://www.pnas.org/content/116/21/10329"
sample_data <- read.table(file ="C:\\Users\\yasmi\\OneDrive\\My Documents\\Harvard2021\\dataset.csv", header = T, sep = ",", stringsAsFactors = T)
sample_district_1DS<-subset(sample_data,district=="1Ds") # shows data in sample police district
sample_district_1DS<-sample_district_1DS[order(sample_district_1DS$block_id),] # puts data in order of block ID
readr::write_csv(sample_district_1DS, file="inst/extdata/sample_district_1DS.csv")
usethis::use_data(sample_district_1DS, compress = "xz")
