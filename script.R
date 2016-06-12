setwd("C:/Users/Michael Crocco/scripts/SwiftKey_Capstone")

## GET RAW DATA
fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
fileName <- "Coursera-Swiftkey.zip"
if(!file.exists(fileName)) {
        download.file(fileUrl, destfile = fileName)
}

if(!file.exists('badwords.txt')) {
        download.file('http://www.bannedwordlist.com/lists/swearWords.txt',
                      destfile = 'badwords.txt')
}
dateDownloaded <- date()

if(!file.exists('final/en_US/en_US.blogs.txt')) {
        unzip(fileName, 'final/en_US/en_US.blogs.txt', junkpaths = TRUE)
        unzip(fileName, 'final/en_US/en_US.twitter.txt', junkpaths = TRUE)
        unzip(fileName, 'final/en_US/en_US.news.txt', junkpaths = TRUE)
}

# raw data files
blogs <- readLines('en_US.blogs.txt', skipNul = TRUE)
news <- readLines('en_US.news.txt', skipNul = TRUE)
twitter <- readLines('en_US.twitter.txt', skipNul = TRUE)

# function to get corpus size (with simplified filtering)
rawCorpusStats = function(corps){
# Takes a raw text file (read line-by-line),
# and returns a list consisting of the number of
# rows and words in the file''
        rows = NROW(corps)
        totalWords = 0
        max = length(strsplit(corps[1], ' ')[[1]])
        min = length(strsplit(corps[1], ' ')[[1]])
        

        for (row in 1:rows) {
                corps[row] = gsub(' {2,}',' ',corps[row])
                words = length(strsplit(corps[row], ' ')[[1]])
                if (words < min) {
                        min = words
                }
                if (words > max) {
                        max = words
                }
                totalWords = totalWords + words
        }
        
        return(as.list(c(rows, totalWords/rows, min, max, totalWords)))
}
blogStats <- rawCorpusStats(blogs)
newsStats <- rawCorpusStats(news)
twitterStats <- rawCorpusStats(twitter)
summaryStats <- matrix(c(blogStats, newsStats, twitterStats), byrow = TRUE,
                       nrow = 3, ncol = 5)
rownames(summaryStats) <- c("Blogs", "News", "Twitter")
colnames(summaryStats) <- c("No. of Lines", "Avg. Line Length",
                            "Min. Length", "Max. Length", "Total Words")

print(summaryStats)


# function to clean corpera
corpusClean = function(corps, minimum){
# Cleans a sample corpus of text for NLP,
# looks for lines greater than a minimum
        rows = NROW(corps)
        keep = vector(mode = "logical", length = rows)
        for (row in 1:rows) {
                words = length(strsplit(corps[row], ' ')[[1]])
                if (words >= minimum) {
                        keep[row] = TRUE
                } else {
                        keep[row] = FALSE
                }
        }
        corps = subset(corps, keep)
        
        library(tm)
        corps = removePunctuation(corps, 
                                  preserve_intra_word_dashes = TRUE)
        corps = tolower(corps)
        corps = stripWhitespace(corps)
        corps = removeNumbers(corps)
        corps = removeWords(corps, 'badwords.txt')
        
}
minLength <- 4


## LOAD REQUIRED LIBRARIES

library(tm)

## SUBSET DATA at 10% for simplification
set.seed(1337)
blogs_sample <- sample(blogs, NROW(blogs) * 0.01)
set.seed(1337)
news_sample <- sample(news, NROW(news) * 0.1)
set.seed(1337)
twitter_sample <- sample(twitter, NROW(news) * 0.1)

## CLEAN SUBSET DATA
blogs_clean <- corpusClean(blogs_sample, 4)
news_clean <- corpusClean(news_sample, 4)
twitter_clean <- corpusClean(twitter_sample, 4)



# Combine all sample data into a single corpus and convert to tm format
#sampleCorpus <- as.list(c(blogs, news))
#rm(blogs); rm(news); rm(twitter)
#gc()


blogsCorp <- Corpus(VectorSource(blogs_clean))
newsCorp <- Corpus(VectorSource(news_clean))
twitterCorp <- Corpus(VectorSource(twitter_clean))

# tell R to treat corpera as plain text documents

blogsCorp <- tm_map(blogsCorp, PlainTextDocument)
newsCorp <- tm_map(newsCorp, PlainTextDocument)
twitterCorp <- tm_map(twitterCorp, PlainTextDocument)

## CONVERT TO document-term-matrix
blogsDTM <- DocumentTermMatrix(blogsCorp)
blogsDTM
newsDTM <- DocumentTermMatrix(newsCorp)
newsDTM
twitterDTM <- DocumentTermMatrix(twitterCorp)
twitterDTM

#look at term frequency
#do frequencies for each
blogsfreq = colSums(as.matrix(blogsDTM))
blogsfreq[tail(order(blogsfreq),20)]

#create plots
#library(ggplot2)   
#p <- ggplot(subset(wf, freq>50), aes(word, freq))    
#p <- p + geom_bar(stat="identity")   
#p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
#p   


#remove stopwords


## Parallel Computing
#CPU <- parallel::detectCores()
#registerDoParallel(CPU)

                ## do calculations here
