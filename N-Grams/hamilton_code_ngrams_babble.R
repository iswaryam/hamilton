

######## Author: Iswarya Murali ################################################################################
#######  Purpose: Generate a babble (random text generator) using ngrams from ham_lyrics.csv #####################
####### Source ham_lyrics.csv file comes from  https://www.kaggle.com/lbalter/hamilton-lyrics# #####################


############ !!!!!PLEASE READ!!!!!!!!!!################################################################################
##  If you want to run the code directly in R studio (ie without PowerBI)  PLEASE UNCOMMENT THIS LINE FIRST!!!
dataset <- read.csv('ham_lyrics.csv')
# Make sure to also download the file to the same folder and set working directory to source file location!
#######################################################################################################################



library(ngram)
library(ggplot2)
library(anlp)
par(mar = c(0,0,0,0))

dataset$lines

#1) concat the entire column of lines into ONE STRING value
all_lines_combined <- paste(dataset$lines,collapse=" ")


#2) Lets have some fun! babble is a markov chain babbler (random text generator based on ngrams from text)

#3) First, we want to define the 'n' in the n-gram. We generate a RANDOM number between 4 to 10
num <- sample(4:10,1) 
#4)  Get all the n-grams for the n that was returned
my_ng <- ngram(all_lines_combined, n=num)

#5) Now using the list of ngrams, lets generate a random lyric of 100 words
num_of_words_to_generate=100
random_generated_txt = babble (my_ng, num_of_words_to_generate)
random_generated_txt

#6) Plot the text
ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label =  random_generated_txt) + 
  theme_void()

# 7) It's not very pretty! Lets word wrap the text and insert a few \n to make it fit in the graph
cleaned_up_text = paste(strwrap(random_generated_txt,50),collapse = "\n")
ggplot() + 
  annotate("text", x = 4, y = 25, size=8, label =  cleaned_up_text) + 
  theme_void()



for(word in names(list_freq)){
  prd_gram <- c(prd_gram,predict_most_frequent(list_freq[[word]]))}

