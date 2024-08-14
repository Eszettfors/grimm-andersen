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
df_grimm = read_excel("Documents/Programming/R/Research Corpora/Project/data/Corpus_grimm_with_text.xlsx")


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

df_hca = read_excel("Documents/Programming/R/Research Corpora/Project/data/Corpus_HCA.xlsx")

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

par(mfrow = c(1,1))

sum_hca <- sum(df_hca$Tokens)
sum_grimm <- sum(df_grimm$Tokens)

sum(df_hca$Tokens)        
sum(df_grimm$Tokens)   

#grimm corpus is quite larger than the andersen corpus/more texts -> normalized TTR required


## Checking distribution

par(mfrow = c(1,2))

hist_hca = hist(df_hca$Tokens, col = 'lightblue', main = 'Distribution of tokens in Andersen stories', xlab = 'Tokens')

#left skew, tendency towards uniform distribution

hist_grimm = hist(df_grimm$Tokens, col = 'pink', main = 'Distribution of tokens in Grimm stories', xlab = 'Tokens')

#heavy left skew falling -> similar to poisson distribution

box_hca = boxplot(df_hca$Tokens, col = 'lightblue', main = 'Boxplot of tokens in Andersen stories', ylab = 'Tokens')
print(box_hca)

box_grimm = boxplot(df_grimm$Tokens, col = 'pink', main = 'Boxplot of tokens in Grimm stories', ylab = 'Tokens')
print(box_grimm)
#existing outliers -> check and remove

box_grimm$out
#df_grimm[df_grimm$Tokens >= min(box_grimm$out),]
#no especially important works -> okay to remove
#df_grimm = df_grimm[df_grimm$Tokens < min(box_grimm$out),]

Desc(df_hca$Tokens, main='Descriptive plots of tokens in Andersen') #mean = 2743, 95CI = 2426 - 3056; median = 2023, skew = 0.95

Desc(df_grimm$Tokens, main='Descriptive plots of tokens in Grimm')#mean = 1484 95CI = 1338 - 1630; median = 1261; skew = 0.82
dev.off(dev.list()["RStudioGD"]) #clears plot window

#As transforming the data would effect interpretablility negatively, non parametric test of significance
# is used: Mann-Whitney U test

test = wilcox.test(df_grimm$Tokens, df_hca$Tokens)
print(test) #p>0.001 -> highly significant difference in average token length

# Conclusion: The stories of HC Andersen are on average longer than those of the Grimm brothers

### generating complexity measurement:

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
#as the stories all have different length, a normalized type token ration is used. MSTTR has shown it self suitable
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

##analysing Readability
Desc(df_hca$flesch_kincaid, main = 'Flesch-kincaid scores of Andersen texts') # mean 8.996, 95 CI = 8.643-9.151; skew = 0.34
shapiro.test(df_hca$flesch_kincaid)# -> not normally distributed p>0.2

Desc(df_grimm$flesch_kincaid, main = 'Flesch-kincaid scores of Grimm texts') # mean 7.595, 95 CI = 7.63-9.151; skew = -0.07
shapiro.test(df_grimm$flesch_kincaid) #normal distribution: p<0.01

#Transforming hca readability. 
squared_hca = df_hca$flesch_kincaid^2
shapiro.test(squared_hca) #p < 0.01
skewness(squared_hca)#skew = 0.77

root_hca = sqrt(df_hca$flesch_kincaid)
shapiro.test(root_hca) #p = 0.74 -> not normal
skewness(root_hca) #skew = 0.12
#squaring gives normal distribution

#comparing difference in mean
squared_grimm = (df_grimm$flesch_kincaid)^2
shapiro.test(squared_grimm) #normal distribution

test = t.test(squared_hca, squared_grimm)
print(test) #p>0.001 Highly significant

#conclusion:The grimm stories are on average at the readability level of a 7-8 grader,
#while the HC Andersen stories on average at the readability level of a 8-9,
#meaning that the stories of HC Anderson are more difficult to read than the Grimm ones


##Analysing lexical diversity
Desc(df_hca$MSTTR, main = 'MSTTR for Andersen texts') # mean 0.690, 95 CI = 0.689-0.694; skew = -0.81
shapiro.test(df_hca$MSTTR)# -> normally distributed -> p<0.001

Desc(df_grimm$MSTTR, main = 'MSTTR for Grimm texts') # mean 0.669, 95 CI = 0.662-0.675; skew = -3.83
shapiro.test(df_grimm$MSTTR) #normal distribution: p<0.001


test = t.test(df_hca$MSTTR, df_grimm$MSTTR) #p<0.001

CohenD(df_hca$MSTTR, df_grimm$MSTTR)#D = 0.58 -> Medium Effect

#conclusion: There is a significant difference in lexical diversity, with the stories of HCA being more diverse. 
#The size of the effect is medium.



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
sents = read.table('~/Documents/Programming/R/Research Corpora/Project/data/UK_sentiment.txt', sep = "\t", quote = "", stringsAsFactors = FALSE, header = TRUE)
sents = sents[,-1]

## remove stop words
stop_eng <- stopwords()
hca_tagged_reduced<- hca_tagged_reduced[-which(hca_tagged_reduced$lemma %in% stop_eng),]
grimm_tagged_reduced<- grimm_tagged_reduced[-which(grimm_tagged_reduced$lemma %in% stop_eng),]

#merge with sent-values
hca_sent = merge(hca_tagged_reduced, sents, by.x = "lemma", by.y = "Word")
grimm_sent = merge(grimm_tagged_reduced, sents, by.x = "lemma", by.y = "Word")




avg_valence = c()
avg_arousal = c()
avg_dominance = c()
i = 0
for (i in 1:length(df_hca$Text)){
  text = paste('text', i,sep = "")
  hca_sent_text = hca_sent[which(hca_sent$doc_id == text),]
  avg_valence = c(avg_valence, sum(hca_sent_text$Valence)/length(hca_sent_text$Valence))
  avg_arousal = c(avg_arousal, sum(hca_sent_text$Arousal)/length(hca_sent_text$Arousal))
  avg_dominance = c(avg_dominance, sum(hca_sent_text$Dominance)/length(hca_sent_text$Dominance))
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
  avg_valence = c(avg_valence, sum(grimm_sent_text$Valence)/length(grimm_sent_text$Valence))
  avg_arousal = c(avg_arousal, sum(grimm_sent_text$Arousal)/length(grimm_sent_text$Arousal))
  avg_dominance = c(avg_dominance, sum(grimm_sent_text$Dominance)/length(grimm_sent_text$Dominance))
}
df_grimm$valence = avg_valence
df_grimm$arousal = avg_arousal
df_grimm$dominance = avg_dominance
View(df_grimm)
View(df_hca)
#export data

write.xlsx(df_hca, "~/Documents/Programming/R/Research Corpora/Project/output/hca_measures.xlsx", rowNames = FALSE)
write.xlsx(df_grimm, "~/Documents/Programming/R/Research Corpora/Project/output/grimm_measures.xlsx", rowNames = FALSE)


Desc(df_hca$valence)
Desc(df_grimm$valence)

shapiro.test((df_hca$valence))
shapiro.test(df_grimm$valence)
wilcox.test(df_hca$valence, df_grimm$valence)

CohenD(df_hca$valence, df_grimm$valence)
wilcox.test(df_hca$valence)

Desc(df_hca$dominance)
Desc(df_grimm$dominance)

wilcox.test(df_hca$valence, df_grimm$valence)

shapiro.test(df_hca$dominance)
shapiro.test(df_grimm$dominance)

wilcox.test(df_hca$dominance, df_grimm$valence)
CohenD(df_hca$dominance, df_grimm$dominance)


