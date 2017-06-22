################# IMPORT DATA #########################
# twitter
twitter <- readLines("C:/Users/figinim/Documents/Studies/Capstone Project/Coursera-SwiftKey/final/en_US/en_US.twitter.txt", encoding="UTF-8")
twitter <- iconv(twitter, from = "latin1", to = "UTF-8", sub="")

# news
conn <- file("C:/Users/figinim/Documents/Studies/Capstone Project/Coursera-SwiftKey/final/en_US/en_US.news.txt", open="rb")
news <- readLines(conn, encoding="UTF-8")
close(conn)
rm(conn)

# blogs
blogs <- readLines("C:/Users/figinim/Documents/Studies/Capstone Project/Coursera-SwiftKey/final/en_US/en_US.blogs.txt", encoding="UTF-8")
blogs <- iconv(blogs, from = "latin1", to = "UTF-8", sub="")

DataComplete <- c(twitter,blogs,news)



################# CLEAN DATASET #########################
# clean the Data
DataClean <- gsub("#"," ", DataComplete)

# DataClean <- gsub("\'"," ", DataClean) ??? Lo tengo o no?
DataClean <- gsub("\""," ", DataClean)
DataClean <- gsub("-"," ", DataClean)
DataClean <- gsub("_"," ", DataClean)
DataClean <- gsub(","," ", DataClean)
DataClean <- gsub(","," ", DataClean)
DataClean <- gsub(","," ", DataClean)

# split each line When there is a punctuation (except for comma).
DataClean <- unlist(strsplit(DataClean, ".", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, "!", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, "?", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, ":", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, ";", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, "(", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, ")", fixed=TRUE))
DataClean <- unlist(strsplit(DataClean, "/", fixed=TRUE))

# insert symbol | after each sentence
DataClean <- paste(DataClean,"|")

# convert in lower all the letter
DataClean <- tolower(DataClean)

# delete white spaces before and after any sentence
DataClean <- trimws(DataClean)

# convert double spaces or more in a single one
DataClean <- gsub("\\s+", " ", DataClean)

# divide the single words
DataClean <- unlist(strsplit(DataClean, "\\s"))



################# BIGRAM TABLE #########################
library(quanteda)

# Two consecutive words
Data2Gram <- tokens_ngrams(DataClean, concatenator=" ")

# Delete couples with "|"
Data2Gram <- Data2Gram[!grepl("\\|", Data2Gram)]

# convert in data.frame
DF2gram <- data.frame(unlist(Data2Gram))

# find frequencies and order
Freq2Gram <- as.data.frame(table(DF2gram))
Freq2Gram <- Freq2Gram[order(-Freq2Gram$Freq),]
#load("~/Studies/Capstone Project/Tables/Freq2Gram.RData")

# separate words
Freq2Table <- data.frame(do.call('rbind', strsplit(as.character(Freq2Gram$DF2gram),' ',fixed=TRUE)),Freq2Gram$Freq)

# take the top for every word
Total2Gram <- Freq2Table[order(Freq2Table$Freq2Gram.Freq,decreasing=T),]
Top2Gram <- Total2Gram[!duplicated(Total2Gram$X1),]

# second word
library(dplyr)
Rest2Gram <- anti_join(Total2Gram, Top2Gram, by = c("X1" = "X1", "X2" = "X2"))
Rest2Gram <- Rest2Gram[order(Rest2Gram$Freq2Gram.Freq,decreasing=T),]
Second2Gram <- Rest2Gram[!duplicated(Rest2Gram$X1),]

# third word
Other2Gram <- anti_join(Rest2Gram, Second2Gram, by = c("X1" = "X1", "X2" = "X2"))
Other2Gram <- Other2Gram[order(Other2Gram$Freq2Gram.Freq,decreasing=T),]
Third2Gram <- Other2Gram[!duplicated(Other2Gram$X1),]

#table with 3 top bigram
WordBigram <- left_join(Top2Gram, Second2Gram, by=c("X1"="X1"))
WordBigram <- left_join(WordBigram, Third2Gram, by=c("X1"="X1"))
colnames(WordBigram) <- c("Start","First","Freq1","Second","Freq2","Third","Freq3")

# delete rows with starting word contains "<" or ">"
WordBigram <- WordBigram[!grepl("<", WordBigram$Start),]
WordBigram <- WordBigram[!grepl(">", WordBigram$Start),]

# clear other columns contain "<" or ">"
WordBigram <- as.data.frame(sapply(WordBigram,sub,pattern='<',replacement=NA))
WordBigram <- as.data.frame(sapply(WordBigram,sub,pattern='>',replacement=NA))

# SEARCH!
WordBigram %>% filter(Start == "word") %>% select(First,Second,Third)



################# TRIGRAM TABLE #########################

# not enough memory for trigram with compete data in my notebook... only from 30 percent is OK
set.seed(190587)
DataClean <- sample(DataClean, length(DataClean)*0.3)

# Three consecutive words
Data3Gram <- tokens_ngrams(DataClean, n = 3L, concatenator=" ")

# Delete couples with "|"
Data3Gram <- Data3Gram[!grepl("\\|", Data3Gram)]

# convert in data.frame
DF3gram <- data.frame(unlist(Data3Gram))

# find frequencies and order
Freq3Gram <- as.data.frame(table(DF3gram))
Freq3Gram <- Freq3Gram[order(-Freq3Gram$Freq),]
#load("~/Studies/Capstone Project/Tables/Freq3Gram03.RData")
Freq3Table <- data.frame(do.call('rbind', strsplit(as.character(Freq3Gram$DF3gram),' ',fixed=TRUE)),Freq3Gram$Freq)

# filter only if frequency > 1
Freq3Table <- filter(Freq3Table, Freq3Gram.Freq>1)

# separate bigram for other word
Freq3Table <- mutate(Freq3Table,X=paste(X1,X2))
Freq3Table <- select(Freq3Table,X,X3,Freq3Gram.Freq)

# take the top for every word
Total3Gram <- Freq3Table[order(Freq3Table$Freq3Gram.Freq,decreasing=T),]
Top3Gram <- Total3Gram[!duplicated(Total3Gram$X),]

# second word
Rest3Gram <- anti_join(Total3Gram, Top3Gram, by = c("X" = "X", "X3" = "X3"))
Rest3Gram <- Rest3Gram[order(Rest3Gram$Freq3Gram.Freq,decreasing=T),]
Second3Gram <- Rest3Gram[!duplicated(Rest3Gram$X),]

# third word
Other3Gram <- anti_join(Rest3Gram, Second3Gram, by = c("X" = "X", "X3" = "X3"))
Other3Gram <- Other3Gram[order(Other3Gram$Freq3Gram.Freq,decreasing=T),]
Third3Gram <- Other3Gram[!duplicated(Other3Gram$X),]

#table with 3 top bigram
WordTrigram <- left_join(Top3Gram, Second3Gram, by=c("X"="X"))
WordTrigram <- left_join(WordTrigram, Third3Gram, by=c("X"="X"))
colnames(WordTrigram) <- c("Start","First","Freq1","Second","Freq2","Third","Freq3")

# delete rows with starting word contains "<" or ">"
WordTrigram <- WordTrigram[!grepl("<", WordTrigram$Start),]
WordTrigram <- WordTrigram[!grepl(">", WordTrigram$Start),]

# clear other columns contain "<" or ">"
WordTrigram <- as.data.frame(sapply(WordTrigram,sub,pattern='<',replacement=NA))
WordTrigram <- as.data.frame(sapply(WordTrigram,sub,pattern='>',replacement=NA))

# SEARCH!
WordTrigram %>% filter(Start == "as soon") %>% select(First,Second,Third)



################# QUADRIGRAM TABLE #########################

# not enough memory for quadrigram with compete data in my notebook... only from 25 percent is OK
set.seed(190587)
DataClean <- sample(DataClean, length(DataClean)*0.25)

# Three consecutive words
Data4Gram <- tokens_ngrams(DataClean, n = 4L, concatenator=" ")

# Delete couples with "|"
Data4Gram <- Data4Gram[!grepl("\\|", Data4Gram)]

# convert in data.frame
DF4gram <- data.frame(unlist(Data4Gram))

# find frequencies and order
Freq4Gram <- as.data.frame(table(DF4gram))
Freq4Gram <- Freq4Gram[order(-Freq4Gram$Freq),]

# separate trigram for other word, take only if there is > 1 frequency
Freq4GramFilter <- filter(Freq4Gram, Freq>1)
Freq4Table <- data.frame(do.call('rbind', strsplit(as.character(Freq4GramFilter$DF4gram),' ',fixed=TRUE)),Freq4GramFilter$Freq)
#load("~/Studies/Capstone Project/Tables/Freq4Table.RData")
Freq4Table <- mutate(Freq4Table,X=paste(X1,X2,X3))
Freq4Table <- select(Freq4Table,X,X4,Freq4GramFilter.Freq)

# take the top for every word
Total4Gram <- Freq4Table[order(Freq4Table$Freq4GramFilter.Freq,decreasing=T),]
Top4Gram <- Total4Gram[!duplicated(Total4Gram$X),]

# second word
Rest4Gram <- anti_join(Total4Gram, Top4Gram, by = c("X" = "X", "X4" = "X4"))
Rest4Gram <- Rest4Gram[order(Rest4Gram$Freq4GramFilter.Freq,decreasing=T),]
Second4Gram <- Rest4Gram[!duplicated(Rest4Gram$X),]

# third word
Other4Gram <- anti_join(Rest4Gram, Second4Gram, by = c("X" = "X", "X4" = "X4"))
Other4Gram <- Other4Gram[order(Other4Gram$Freq4GramFilter.Freq,decreasing=T),]
Third4Gram <- Other4Gram[!duplicated(Other4Gram$X),]

#table with 3 top bigram
WordQuadrigram <- left_join(Top4Gram, Second4Gram, by=c("X"="X"))
WordQuadrigram <- left_join(WordQuadrigram, Third4Gram, by=c("X"="X"))
colnames(WordQuadrigram) <- c("Start","First","Freq1","Second","Freq2","Third","Freq3")

# delete rows with starting word contains "<" or ">"
WordQuadrigram <- WordQuadrigram[!grepl("<", WordQuadrigram$Start),]
WordQuadrigram <- WordQuadrigram[!grepl(">", WordQuadrigram$Start),]

# clear other columns contain "<" or ">"
WordQuadrigram <- as.data.frame(sapply(WordQuadrigram,sub,pattern='<',replacement=NA))
WordQuadrigram <- as.data.frame(sapply(WordQuadrigram,sub,pattern='>',replacement=NA))

# SEARCH!
WordQuadrigram %>% filter(Start == "as soon as") %>% select(First,Second,Third)