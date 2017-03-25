require("data.table")
require("ngram")

# MODEL
model <- function(sentence, onegram, twogram, threegram, fourgram, fivegram, lambda = 0.4, nPredict = 3) {
    
    sentence <- preprocess(sentence, remove.punct = TRUE, remove.numbers = TRUE, fix.spacing = TRUE)
    
    words <- unlist(strsplit(sentence, " ") )
    nWords <- length(words)
    
    if (nWords >= 4) candidateIsFiveGram  = TRUE else candidateIsFiveGram  = FALSE
    if (nWords >= 3) candidateIsFourGram  = TRUE else candidateIsFourGram  = FALSE
    if (nWords >= 2) candidateIsThreeGram = TRUE else candidateIsThreeGram = FALSE
    if (nWords >= 1) candidateIsTwoGram   = TRUE else candidateIsTwoGram   = FALSE
    if (nWords >= 0) candidateIsOneGram   = TRUE
    
    output <- data.table()

    # 5-gram
    if (candidateIsFiveGram) {
        inputFourGram  <- paste(words[(nWords-3):nWords], collapse = " ")
        matchedFiveGram <- fivegram[first == inputFourGram, .(last, freq)]
        if (nrow(matchedFiveGram) >= 1) {
            inputFourGramCount <- fourgram[all == inputFourGram, sum(freq)]
            outputFiveGram <- matchedFiveGram[, score := freq/inputFourGramCount]
            output <- rbind(output, outputFiveGram[, .(last, score)])
        }
        if (nrow(output) >= nPredict ) return(output[1:nPredict, .(last)])
    }
    
    # 4-gram
    if (candidateIsFourGram) {
        inputThreeGram <- paste(words[(nWords-2):nWords], collapse = " ")
        matchedFourGram <- fourgram[first == inputThreeGram, .(last, freq)]
        if (nrow(matchedFourGram) >= 1) {
            inputThreeGramCount <- threegram[all == inputThreeGram, sum(freq)]
            outputFourGram <- matchedFourGram[, score := lambda * freq/inputThreeGramCount]
            output <- rbind(output, outputFourGram[, .(last, score)])
            output <- subset(output, !duplicated(output[,last]) )
        }
        if (nrow(output) >= nPredict ) return(output[1:nPredict, .(last)])
    }
    
    
    # 3-gram
    if (candidateIsThreeGram) {
        inputTwoGram   <- paste(words[(nWords-1):nWords], collapse = " ")
        matchedThreeGram <- threegram[first == inputTwoGram, .(last, freq)]
        if (nrow(matchedThreeGram) >= 1) {
            inputTwoGramCount <- twogram[all == inputTwoGram, sum(freq)]
            outputThreeGram <- matchedThreeGram[, score := lambda * lambda * freq/inputTwoGramCount]
            output <- rbind(output, outputThreeGram[, .(last, score)])
            output <- subset(output, !duplicated(output[,last]) )
        }
        if (nrow(output) >= nPredict ) return(output[1:nPredict, .(last)])
    }
    
    # 2-gram
    if (candidateIsTwoGram) {
        inputOneGram   <- paste(words[nWords:nWords],     collapse = " ")
        matchedTwoGram <- twogram[first == inputOneGram, .(last, freq)]
        if (nrow(matchedTwoGram) >= 1) {
            inputOneGramCount <- onegram[last == inputOneGram, sum(freq)]
            outputTwoGram <- matchedTwoGram[, score := lambda * lambda * lambda * freq/inputOneGramCount]
            output <- rbind(output, outputTwoGram[, .(last, score)])
            output <- subset(output, !duplicated(output[,last]) )
        }
        if (nrow(output) >= nPredict ) return(output[1:nPredict, .(last)])
    }
    
    
    # 1-gram
    if(nrow(output) < nPredict) {
        matchedOneGram <- onegram[1:nPredict, .(last, freq)]
        outputOneGram <- matchedOneGram[, score := freq/49842012] # number of words in the corpus
        output <- rbind(output, outputOneGram[, .(last, score)])
        output <- subset(output, !duplicated(output[,last]) )
        return(output[1:nPredict, .(last)])
    }
}
