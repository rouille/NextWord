NextWord is a simple application that aims at predicting the word most likely to follow 
a meaningful sequence of words. The application runs on Shiny Server: 
https://rouille.shinyapps.io/NextWord/

NextWord relies on a 5-gram language model and a simple Backoff algorithm to rank the 
next word candidates. The n-grams are extracted from a large corpus made of blog posts, 
news articles and tweets. 

This project has been carried out in the framework of The Data Science Capstone by Johns 
Hopkins University deployed on Coursera. The data can be downloaded at the following url: 
https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip


