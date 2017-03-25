# READ FILE
setwd("~/Coursera/DataScienceSpecialization/10-Capstone/Week3/Assignment")
sample <- readLines("sample.txt", skipNul = TRUE, encoding = "UTF-8")

require("tm")
require("ngram")

# CLEANING
## lowercase
clean <- tolower(sample); rm("sample")

## urls, html, email addresses, hashtags and twitter usernames.
clean <- gsub("http\\S+\\s*", "", clean); clean <- gsub("www\\S+\\s*", "", clean) # urls
html  <- grep("style=", clean); if (length(html) >= 1) clean <- clean[-html]      # html
clean <- gsub('\\S+@\\S+', "", clean)                                             # emails
clean <- gsub('#\\S+', "", clean); clean <- gsub('@\\S+', "", clean)              # twitter

## punctuation
clean <- removePunctuation(clean)

## numbers
clean <- removeNumbers(clean)

## non-ASCII
clean <- iconv(clean, "UTF-8", "ASCII", sub = "")

## white spaces
clean <- stripWhitespace(clean)

## profanity. Remove entire entry when encountered.
badwords <- readLines("badwords.txt", skipNul = TRUE, encoding = "UTF-8")
for (i in seq(length(badwords) ) ) {
    index <- grep(badwords[i], clean)
    if (length(index) >= 1) clean <- clean[-index]
    if (i/25 == i %/% 25) print(paste0(i, "/", length(badwords) ) )
}


# N-GRAM
words <- concatenate(clean); rm("clean")
save(words, file = 'words.RData')

getNGram <- function(words, n, freqmin = 1) {
    ngram <- ngram(words, n = n)
    df <- get.phrasetable(ngram)
    
    df <- subset(df, freq >= freqmin)
    
    return(df[, 1:2])
}

## 1-gram
onegram <- getNGram(words, 1, freqmin = 4)
save(onegram, file = 'onegram.RData')

## 2-gram
twogram <- getNGram(words, 2, freqmin = 4)
save(twogram, file = 'twogram.RData')

## 3-gram
threegram <- getNGram(words,  3, freqmin = 4)
save(threegram, file = 'threegram.RData')

## 4-gram
fourgram  <- getNGram(words, 4, freqmin = 3)
save(fourgram, file = 'fourgram.RData')

## 5-gram
fivegram <- getNGram(words, 5, freqmin = 3)
save(fivegram, file = 'fivegram.RData')
