# CORPUS
setwd("~/Coursera/DataScienceSpecialization/10-Capstone/Week3/Assignment")
dir <- "../../final/en_US/"
files <- paste0(dir, list.files(dir) )
nFiles <- length(files)

# SAMPLING
set.seed(10) # for reproducibility
nSample <- 750000
sampleFile <- function(fileName) {
    file <- readLines(fileName, skipNul = TRUE, encoding = "UTF-8")
    sample <- file[sample(length(file), nSample)]
    
    return(sample)
}

for (i in seq(nFiles) ) {
    if (i == 1) selected <- sampleFile(files[i])
    else selected <- rbind(selected, sampleFile(files[i]) )
}

writeLines(selected, con = "sample.txt")
