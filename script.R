setwd("C:/Users/Michael Crocco/scripts/SwiftKey_Capstone")
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "Coursera-Swiftkey.zip"
if(!file.exists(fileName)) {
        download.file(fileUrl, destfile = fileName)
}
dateDownloaded <- date()

if(!file.exists('final/en_US/en_US.blogs.txt')) {
        unzip(fileName, 'final/en_US/en_US.blogs.txt', junkpaths = TRUE)
}
library(tm)

blogs <- readLines('en_US.blogs.txt')