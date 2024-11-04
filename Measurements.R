library(textstem)
library(spacyr)
library(stringr)
library(quanteda.textstats)
library(quanteda)
library(readr)
library(tokenizers)
library(tidytext)
library(tibble)
library(visreg)
library(dplyr)
library(koRpus)
library(DescTools)
library(visreg)
library(moments)
library(openxlsx)

######## reading grimm
inputpath = paste(getwd(), "/data", sep = "")
df_grimm = read.xlsx(paste(inputpath, "/Corpus_grimm_with_text.xlsx", sep=""))

text = c()
i = 1
for (i in 1:length(df_grimm$text)){
  text[i] = df_grimm$text[i]
}

grimm_corpus = corpus(text)
summary(grimm_corpus)

docvars(grimm_corpus, 'title') = df_grimm$title
docvars(grimm_corpus, 'author') = 'Grimm'

df_grimm = summary(grimm_corpus, n=Inf)

##### reading Andersen
df_hca = read.xlsx(paste(inputpath, "/Corpus_HCA.xlsx", sep=""))

text = c()
i = 1
for (i in 1:length(df_hca$text)){
  text[i] = df_hca$text[i]
}

hca_corpus = corpus(text)
summary(hca_corpus)

docvars(hca_corpus, 'title') = df_hca$title
docvars(hca_corpus, 'author') = 'HC Andersen'

df_hca = summary(hca_corpus, n=Inf)

### descriptive analysis of the corpuses

length(df_hca$Text) #142
length(df_grimm$Text) #209

sum_hca = sum(df_hca$Tokens)
sum_grimm = sum(df_grimm$Tokens)

sum(df_hca$Tokens)        
sum(df_grimm$Tokens)   

#difference in size of corpuses -> normalized TTR required

### generating complexity measurements:
#readability
read_fk = function(corpus){
   i = 1
   fk = c()
   for (i in 1:length(corpus)){
     fk[i] = textstat_readability(corpus[i], measure="Flesch.Kincaid")[,2]
   }
   corpus$flesch_kincaid = fk
   return(corpus)
} 


hca_corpus = read_fk(hca_corpus)
grimm_corpus = read_fk(grimm_corpus)

#Lexical diversity
#as the stories all have different length, a normalized type token ratio is used. MSTTR has shown it self suitable
# independent of text length and is easy to interpret
#https://core.ac.uk/download/pdf/82620241.pdf
MSTTR = function(corpus){
  i = 0
  lr = c()
  for (i in 1:length(corpus)){
    tokens = tokens(corpus[i])
    lr[i] = textstat_lexdiv(tokens, measure = "MSTTR")[,2]
    
  }
  corpus$MSTTR = lr
  return(corpus)
  
}

hca_corpus = MSTTR(hca_corpus)
grimm_corpus = MSTTR(grimm_corpus)

df_hca = summary(hca_corpus, n=Inf)
df_grimm = summary(grimm_corpus, n=Inf)

#### Tagging the corpus
spacy_download_langmodel(lang_models = "en_core_web_sm", force = TRUE)
spacy_initialize("en_core_web_sm")

hca_tagged = spacy_parse(hca_corpus,
                       lemma = TRUE,
                       pos = TRUE, 
                       tag = TRUE, 
                       entity = TRUE, 
                       dependency = TRUE)

grimm_tagged = spacy_parse(grimm_corpus,
                         lemma = TRUE,
                         pos = TRUE, 
                         tag = TRUE, 
                         entity = TRUE, 
                         dependency = TRUE)


spacy_finalize()

## clean corpus
remove_pos = c("PUNCT", "NUM", "SYM", "SPACE")

hca_tagged_reduced = hca_tagged[-which(hca_tagged$pos %in% remove_pos),]
grimm_tagged_reduced = grimm_tagged[-which(grimm_tagged$pos %in% remove_pos),]

## Hapax
rich_hpx <- function(x) {
  table <- table(x$lemma)
  hapax <- table[which(table == 1)]
  hpx_pc <- length(hapax)/length(table)*100
  return(hpx_pc)
}

hapax = c()
i = 0
for (i in 1:length(df_hca$Text)){
  text = paste('text', i,sep = "")
  hca_text = hca_tagged_reduced[which(hca_tagged_reduced$doc_id == text),]
  hapax = c(hapax, rich_hpx(hca_text))
}

df_hca$hapax = hapax

hapax = c()
i = 0
for (i in 1:length(df_grimm$Text)){
  text = paste('text', i,sep = "")
  grimm_text = grimm_tagged_reduced[which(grimm_tagged_reduced$doc_id == text),]
  hapax = c(hapax, rich_hpx(grimm_text))
}
df_grimm$hapax = hapax


#### Lexical density
lex_dens <- function(x) {
  all_pos <- unique(x$pos)
  lex_words <- c("NOUN", "ADJ", "PROPN", "ADV", "VERB")
  grm_words <- all_pos[-which(all_pos %in% lex_words)]
  
  cont_words <- x[which(x$pos %in% lex_words),]
  cont_pc <- nrow(cont_words)/nrow(x)
  grm_words <- x[which(x$pos %in% grm_words),]
  grm_pc <- nrow(grm_words)/nrow(x)
  
  return(c(cont_pc, grm_pc))
}

content_pc = c()
grm_pc = c()
i = 0
for (i in 1:length(df_hca$Text)){
  text = paste('text', i,sep = "")
  hca_text = hca_tagged_reduced[which(hca_tagged_reduced$doc_id == text),]
  content = lex_dens(hca_text)[1]
  grm = lex_dens(hca_text)[2]
  content_pc = c(content_pc, content)
  grm_pc = c(grm_pc, grm)
}

df_hca$lex_D = content_pc
df_hca$grm_D = grm_pc

content_pc = c()
grm_pc = c()
i = 0
for (i in 1:length(df_grimm$Text)){
  text = paste('text', i,sep = "")
  grimm_text = grimm_tagged_reduced[which(grimm_tagged_reduced$doc_id == text),]
  content = lex_dens(grimm_text)[1]
  grm = lex_dens(grimm_text)[2]
  content_pc = c(content_pc, content)
  grm_pc = c(grm_pc, grm)
}

df_grimm$lex_D = content_pc
df_grimm$grm_D = grm_pc


### Syntactic complexity

ficht_c <- function(x) {
  
  sub_corpus <- x
  n_verbs <- nrow(x[which(x$pos == "VERB"),])
  x$doc_sent_id <- paste(x$doc_id, x$sentence_id, sep = "")
  n_sentences <- length(unique(x$doc_sent_id))
  n_words <- nrow(x)
  c <- (n_verbs/n_sentences)*(n_words/n_sentences)
  
  return(c)
}

ficht = c()
i = 0
for (i in 1:length(df_hca$Text)){
  text = paste('text', i,sep = "")
  hca_text = hca_tagged_reduced[which(hca_tagged_reduced$doc_id == text),]
  measure = ficht_c(hca_text)
  ficht = c(ficht, measure)
}
df_hca$ficht_c = ficht

ficht = c()
i = 0
for (i in 1:length(df_grimm$Text)){
  text = paste('text', i,sep = "")
  grimm_text = grimm_tagged_reduced[which(grimm_tagged_reduced$doc_id == text),]
  measure = ficht_c(grimm_text)
  ficht = c(ficht, measure)
}
df_grimm$ficht_c = ficht


#### sentiment analysis
sents = read.table(paste(inputpath, "/NRC-VAD-Lexicon.txt", sep = ""), sep = "\t", quote = "", stringsAsFactors = FALSE, header = FALSE)
colnames(sents) = c("word", 'valence', 'arousal', 'dominance')

## remove stop words and unify token by lowercase
stop_eng = stopwords()
hca_tagged_reduced = hca_tagged_reduced[-which(hca_tagged_reduced$lemma %in% stop_eng),]
grimm_tagged_reduced = grimm_tagged_reduced[-which(grimm_tagged_reduced$lemma %in% stop_eng),]
grimm_tagged_reduced$token = tolower(grimm_tagged_reduced$token)
hca_tagged_reduced$token = tolower(hca_tagged_reduced$token)

#merge with sent-values
hca_sent = merge(hca_tagged_reduced, sents, by.x = "token", by.y = "word")
grimm_sent = merge(grimm_tagged_reduced, sents, by.x = "token", by.y = "word")

avg_valence = c()
avg_arousal = c()
avg_dominance = c()
i = 0
for (i in 1:length(df_hca$Text)){
  text = paste('text', i,sep = "")
  hca_sent_text = hca_sent[which(hca_sent$doc_id == text),]
  avg_valence = c(avg_valence, sum(hca_sent_text$valence)/length(hca_sent_text$valence))
  avg_arousal = c(avg_arousal, sum(hca_sent_text$arousal)/length(hca_sent_text$arousal))
  avg_dominance = c(avg_dominance, sum(hca_sent_text$dominance)/length(hca_sent_text$dominance))
}

df_hca$valence = avg_valence
df_hca$arousal = avg_arousal
df_hca$dominance = avg_dominance

avg_valence = c()
avg_arousal = c()
avg_dominance = c()
i = 0
for (i in 1:length(df_grimm$Text)){
  text = paste('text', i,sep = "")
  grimm_sent_text = grimm_sent[which(grimm_sent$doc_id == text),]
  avg_valence = c(avg_valence, sum(grimm_sent_text$valence)/length(grimm_sent_text$valence))
  avg_arousal = c(avg_arousal, sum(grimm_sent_text$arousal)/length(grimm_sent_text$arousal))
  avg_dominance = c(avg_dominance, sum(grimm_sent_text$dominance)/length(grimm_sent_text$dominance))
}
df_grimm$valence = avg_valence
df_grimm$arousal = avg_arousal
df_grimm$dominance = avg_dominance


#export data
outputpath = paste(getwd(), "/output", sep = "")
write.xlsx(df_hca, paste(outputpath, "/hca_measures.xlsx", sep=""), rowNames = FALSE)
write.xlsx(df_grimm, paste(outputpath, "/grimm_measures.xlsx", sep=""), rowNames = FALSE)

sents_nums = subset(sents, select = -c(Word))
cm1 = cor(sents_nums)
corrplot(cm1)


VAD_lexicon = read.table(paste(inputpath, "/NRC-VAD-Lexicon.txt", sep = ""), sep = "\t", quote = "", stringsAsFactors = FALSE, header = FALSE)
View(VAD_lexicon)
colnames(VAD_lexicon) = c("word", "valence", "arousal", 'dominance')
vad_nums = subset(VAD_lexicon, select = -c(word))
cm2 = cor(vad_nums)
corrplot(cm2)
