
######## Author: Iswarya Murali ##################################################################################
#######  Purpose: Get all ngrams from ham_lyrics.csv #########################################################
####### Source ham_lyrics.csv file comes from  https://www.kaggle.com/lbalter/hamilton-lyrics# #####################



############ !!!!!PLEASE READ!!!!!!!!!!################################################################################
##  If you want to run the code directly in R studio (ie without PowerBI)  PLEASE UNCOMMENT THIS LINE FIRST!!!

 dataset <- read.csv('ham_lyrics.csv')

# Make sure to also download the file to the same folder and  set working directory to source file location!
#######################################################################################################################





x <- dataset$lines
x

library(ngram)
library(ggplot2)
library(stringr)

#1) concat the entire column of lines into ONE STRING value
all_lines_combined <- paste(dataset$lines,collapse=" ")
all_lines_combined
string.summary (all_lines_combined)

#2) Lets get 5-grams using get.phrasetable and the ngram function
num = 5
all_ng <- get.phrasetable(
                  ngram(all_lines_combined, n=num))
head(all_ng)

# 3) removing prop colu,n because I don't need it
all_ng<- all_ng[,1:2];

#4) sort by frequency of appearance
all_ng <- all_ng[order(-all_ng$freq),]
head(all_ng)

#5) Plot the top ngrams from this text
ggplot(data=head(all_ng,20), 
            aes(x=reorder(ngrams, freq), y=freq)) +
            geom_bar(stat="identity") + 
  coord_flip() + 
  theme_grey(base_size = 22);






## Predictor

all_ng_predictor <- get.phrasetable(
  ngram(tolower(all_lines_combined), n=5))

#txt_to_search <- "room where it"
txt_to_search <- "must be nice"
x <- all_ng_predictor[grep(txt_to_search,all_ng_predictor$ngrams), ]
#match(x, txt_to_search)
#str_locate(x$ngrams, txt_to_search)
y = str_locate(x$ngrams, txt_to_search)
#y[,1];#y[,2]

head(substr(x$ngrams,y[,1],str_length(x$ngrams)),5)

