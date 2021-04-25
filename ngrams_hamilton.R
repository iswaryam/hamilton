
######## Author: Iswarya Murali ##################################################################################
#######  Purpose: Get all ngrams from ham_lyrics.csv #########################################################
####### Source ham_lyrics.csv file comes from  https://www.kaggle.com/lbalter/hamilton-lyrics# #####################


dataset <- read.csv('ham_lyrics.csv')


library(ngram)
library(ggplot2)
library(stringr)

#1) concat the entire column of lines into ONE STRING value
all_lines_combined <- paste(dataset$lines,collapse=" ")
all_lines_combined
string.summary (all_lines_combined)

#2) Lets get 5-grams using get.phrasetable and the ngram function
num = 2
all_ng <- get.phrasetable(
                  ngram(all_lines_combined, n=num))
head(all_ng)

# 3) removing prop columnn because I don't need it
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


# Babble
# 2) Lets have some fun! babble is a markov chain babbler (random text generator based on ngrams from text)

num_of_words_to_generate=32
num <- sample(4:12,1) # generate a RANDOM n between 4 to 12
my_ng <- ngram(all_lines_combined, n=2)
random_generated_txt <- babble (my_ng, num_of_words_to_generate)

random_generated_txt

ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label =  random_generated_txt) + 
  theme_void()

# 3) Wrap the text and insert \n to make it look cleaner

cleaned_up_text = paste(strwrap(random_generated_txt,30),collapse = "\n")

#cleaned_up_text

ggplot() + 
  annotate("text", x = 4, y = 25, size=4, label =  cleaned_up_text) + 
  theme_void()



## Basic Predictor

all_ng_predictor <- get.phrasetable(
  ngram(tolower(all_lines_combined), n=5))

#txt_to_search <- "room where it"
txt_to_search <- "must be nice"
#txt_to_search <- "my name is "
x <- all_ng_predictor[grep(txt_to_search,all_ng_predictor$ngrams), ]
#match(x, txt_to_search)
#str_locate(x$ngrams, txt_to_search)
y = str_locate(x$ngrams, txt_to_search)
#y[,1];#y[,2]

head(substr(x$ngrams,y[,1],str_length(x$ngrams)),6)

