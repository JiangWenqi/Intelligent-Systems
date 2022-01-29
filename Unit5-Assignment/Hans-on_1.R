urlQuijoteGutenber <- "https://www.gutenberg.org/files/2000/2000-0.txt" 
lines <- readLines(urlQuijoteGutenber,  encoding = "UTF-8") #It takes a few seconds

grep(pattern = "***", lines, fixed = TRUE) #Warning! Without fixed the regex is "\\*\\*\\*"
linesQ <- lines[25:37703] 
length(linesQ) #37,679

grep(pattern = "En un lugar de", linesQ, fixed = TRUE) #Lines 1045 and 13513. The good one is the first one
linesQ <- linesQ[-c(1:1044)] #Remove the prologue 
length(linesQ) #36,635
linesQ[1:5]
paste(linesQ[1:5], collapse = " ")

# 2. Basic checks

library(utf8)
#Chack encoding
linesQ[!utf8_valid(linesQ)] #character(0) ==> All lines are made of correct UTF-8 characters
#Check character normalization. Specifically, the normalized composed form (NFC)
linesQ_NFC <- utf8_normalize(linesQ)
sum(linesQ_NFC != linesQ) #0 means all right. The text is in NFC.

# 3. Basic structuration
stringQ <- paste(linesQ, collapse = "\n") #One big string
paragraphs <- unlist(strsplit(stringQ, "\\n\\n\\n"))#Warn! (1)strsplit returns a list,
                                                    #      (2)escape \n and
                                                    #      (3)by default, fixed=FALSE
                                                    #Using fixed=TRUE this should be
                                                    #     "\n\n\n", fixed = TRUE
parEmpty <- which(paragraphs == "") #No empty paragraphs 
#paragraphs <- paragraphs[-parEmpty]
length(paragraphs) # 128
substring(paragraphs[1], 1, 200)

# 4. Some cleaning
#Testing the regex
gsub("[\n]{1,}", " ", c(par1="with one \nbut also\n", par2="with a seq of \n\nlike this"))
paragraphswoNL <- gsub("[\n]{1,}", " ", paragraphs) #wo = without 
substring(paragraphswoNL[1], 1, 200)

paragraphs <- gsub("[ ]{2,}", " ", paragraphswoNL) #We reassign the varible paragraphs 
substring(paragraphs[1], 1, 200)



# 5. Some numbers (chars, words, sentences)
library(spacyr)
spacy_install()
#Use spacy_install() if you have never used spacyr before. This will install a miniconda environment
spacy_download_langmodel('es') #This downloads the model es_core_news_sm to disk
spacy_initialize(model = "es_core_news_sm") #Loads the Spanish model fron disk
#Gets sentences from paragraphs
phrases <- spacy_tokenize(paragraphs,             #If you use quanteda you can use
                                                  #  corpus_reshape(corpus, to = "sentences"))
                                                  #Taks a while.
                                                  #Returns a list with 138 elements, each one
                                                  #        is a string vector.
                          what="sentence"         #By default remove_separators = TRUE # (removes trailing spaces)
)
v_phrases <- unlist(phrases)
numphrases <- length(v_phrases) #8,975 sentences 
sum(v_phrases=="") #1
v_phrases <- v_phrases[-which(v_phrases=="")] #8,974 sentences
#A simple histogram will do fine
hist(nchar(v_phrases),
     main = "Histogram of sentence size",
     xlab = "Sentece size (number of characters)", ylab = "Ocurrences"
)

tokens <- spacy_tokenize(paragraphs
                         #Parameters asigned by default:
                         #remove_punct = FALSE, punt symbols are tokens
                         #remove_url = FALSE,  url elements are tokens
                         #remove_numbers = FALSE,   
                         #remove_ separators = TRUE, numbers are tokens spaces are NOT tokens
                         #remove_symbols FALSE,  symbols (like $) are tokens
)#Returns a list

v_tokens <- unlist(tokens)
v_tokens[1:10]
length(v_tokens) #442,164 tokens (many repeated)

length(unique(v_tokens)) #24,130 different (unique) tokens.
#As a list
head(sort(table(v_tokens), decreasing = TRUE), n = 25)
#As a simple plot
plot(head(sort(table(v_tokens), decreasing = TRUE), n = 10), xlab = "Token",
     ylab = "Ocurrences"
)
tic <- Sys.time()
res <- lapply(v_phrases[1:100],
              spacy_parse, #This is the function to apply to every element in v_phrases
              dependency = TRUE, nounphrase = TRUE #These are the arguments of the functio 
              )
df <- res[[1]] #A data frame with the first resuls
for (i in 2:length(res)){ #Attention! The loop starts from 2
  df <- rbind(df, res[[i]]) }
Sys.time()-tic
#As this takes a while, I save the result
saveRDS(df, file="spacy_parse_Quixote.rds")
#Shows the first 20 tokens.
library(kableExtra) #Styling the kable output to show very width data frames 
kable_styling(kable(df[1:20, c(3:ncol(df))]), #The first 2 cols UNSHOWN are doc_id and sentence_id
              font_size = 7
)
library(udpipe)
model_file <- 'spanish-ancora-ud-2.5-191206.udpipe' 
if(!file.exists(model_file)){
  model <- udpipe_download_model(language = "spanish-ancora") #Another alternative: "spanish-gsd"
  udmodel_es <- udpipe_load_model(file = model$file_model) 
}else{
    udmodel_es <- udpipe_load_model(file = model_file) 
}
tic <- Sys.time()
anno <- udpipe_annotate(udmodel_es,
                        x = v_phrases[1:100],
                        parallel.cores = 10 #Check your system!! 
                        )
df <- as.data.frame(anno) 
Sys.time()-tic
## Pay attention
#anno is a list containing 3 things (last 2 where lost converting to data frame):
# 1) x: the character vector with text.
# 2) conllu: annnotation in CONLL-U format
# 3) error: A vector with the same length of x containing possible errors when annotating x
#Write the result as a coNLL file
cat(anno$conllu, file = "udpipes_es_Quixote.conllu")
#You can read this file with udpipe_read_conllu()
#Show the annotations of the first 20 tokens
#As df has 14 columns, we show them in two tables
library(kableExtra) #Styling the kable output to show very width data frames
kable_styling(kable(df[1:20, c(5:9)]), #The first 4 cols UNSHOWN are 
               #doc_id, paragraph_id, sentence_id and sentence
              font_size = 7 
)
kable_styling(kable(df[1:20, c(10:14)]), #Remaining cols 
              font_size = 7
)
spacy_finalize() #Do not forget this
sessionInfo()


write.csv(x,"job discriptions(Chinese).csv", row.names = FALSE)
