# LOAD VARIABLES
setwd("~/Coursera/DataScienceSpecialization/10-Capstone/Week3/Assignment")

load("onegram.RData")
load("twogram.RData")
load("threegram.RData")
load("fourgram.RData")
load("fivegram.RData")

require("ngram")
require("data.table")

# SPLIT FIRST WORDS / LAST WORD
SplitNGram <- function(ngram) {
    nRow <- nrow(ngram)
    all   <- character(length = nRow)
    first <- character(length = nRow)
    last  <- character(length = nRow)
    for (i in seq(nRow) ) {
        sentence <- preprocess(ngram[i,1], fix.spacing = TRUE)
        words <- unlist(strsplit(sentence, " ") )
        nWords <- length(words)
        
        if (i/25000 == i %/% 25000) print(i)
        
        all[i]   <- sentence
        first[i] <- paste(words[1:nWords-1], collapse = " ")
        last[i]  <- words[nWords]
    }
    df <- data.frame(all = all, first = first, last = last, freq = ngram$freq, stringsAsFactors = FALSE)
    
    return(data.table(df) )
}

for(i in seq(nrow(onegram) ) ) onegram[i,1] <- preprocess(onegram[i,1], fix.spacing = TRUE)
colnames(onegram)[1] <- "last"
onegram   <- data.table(onegram)
twogram   <- SplitNGram(twogram)
threegram <- SplitNGram(threegram)
fourgram  <- SplitNGram(fourgram)
fivegram  <- SplitNGram(fivegram)

save(onegram, twogram, threegram, fourgram, fivegram, file = 'ngrams_final.RData')
