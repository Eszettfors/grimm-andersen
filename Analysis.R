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
library(ggplot2)
library(effsize)
library(gt)

inputpath = paste(getwd(), "/output", sep = "")
df_hca = read.xlsx(paste(inputpath, "/hca_measures.xlsx", sep=""))
df_grimm = read.xlsx(paste(inputpath, "/grimm_measures.xlsx", sep=""))
df_bind = rbind(df_hca, df_grimm)

#data frame to hold averages
df_average = data.frame(row.names = c('Mean/Median*', 'mean/median*', 'p-value', "Cohen's D/Cliff's delta*"))
df_average$author = c('HC Anderson', 'Grimm Brothers')
df_average$author[3:4] = NA


### length of texts
Desc(df_hca$Tokens, main='Descriptive plots of tokens in Andersen') #mean = 2743, 95CI = 2426 - 3056; median = 2023, skew = 0.95
Desc(df_grimm$Tokens, main='Descriptive plots of tokens in Grimm')#mean = 1484 95CI = 1338 - 1630; median = 1261; skew = 0.82

shapiro.test(df_hca$Tokens) #normality can be rejected
shapiro.test(log(df_hca$Tokens)) #more normal, but rejectable on the .01 lvl -> parametric test required
wilcox.test(df_hca$Tokens, df_grimm$Tokens) #p>0.001 -> Significant difference
df_average$length = c(round(median(df_hca$Tokens), 2), round(median(df_grimm$Tokens),2), 'p<0.01', NA)

bar <- df_bind %>% group_by(author) %>% summarise(m = median(Tokens),
                                       CI = MedianCI(Tokens)[3]-MedianCI(Tokens)[2])
ggplot(bar, aes(author,m,fill=author)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=m-CI/2, ymax=m+CI/2), width=.2,
                position=position_dodge(.9))+
                  ylab('Average number of tokens per text')

### Readability

Desc(df_hca$flesch_kincaid, main='Descriptive plots of Flesch-Kincaid score in Andersen')
Desc(df_grimm$flesch_kincaid, main='Descriptive plots of Flesch_Kincaid in Grimm')

shapiro.test(df_hca$flesch_kincaid) # normality can't be rejected, and visual inspection suggests normality
shapiro.test(df_grimm$flesch_kincaid) # normality can be rejected at alpha = 0.01, but visual inspection looks okay

t.test(df_hca$flesch_kincaid, df_grimm$flesch_kincaid)# p<0.001
CD = CohenD(df_hca$flesch_kincaid, df_grimm$flesch_kincaid)

df_average$flesch_kincaid = c(round(mean(df_hca$flesch_kincaid), 2), round(mean(df_grimm$flesch_kincaid),2), 'p<0.001', paste(round(CD,2), 'Medium', sep = ', '))


bar <- df_bind %>% group_by(author) %>% summarise(m = mean(flesch_kincaid),
                                                  CI = MeanCI(flesch_kincaid)[3]-MeanCI(flesch_kincaid)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Average Flesch-Kincaid Score (mean)')


### Lexical diversity

Desc(df_hca$MSTTR, main='Descriptive plots of mean segment type token ratio in Andersen')
Desc(df_grimm$MSTTR, main='Descriptive plots of mean segment type token ratio in Grimm')

shapiro.test(df_hca$MSTTR) # normality can be rejected, but visual inspection looks fairly okay
shapiro.test(df_grimm$MSTTR) # normality can be rejected at alpha = 0.99, but visual inspection looks okay


t.test(df_hca$MSTTR, df_grimm$MSTTR) #p<0.001
CD = CohenD(df_hca$MSTTR, df_grimm$MSTTR)

df_average$MSTTR = c(round(mean(df_hca$MSTTR), 2), round(mean(df_grimm$MSTTR),2), 'p<0.001', paste(round(CD, 2), 'Medium', sep = ', '))


bar <- df_bind %>% group_by(author) %>% summarise(m = mean(MSTTR),
                                                  CI = MeanCI(MSTTR)[3]-MeanCI(MSTTR)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Average Mean Segment Type Token Ratio (mean(')

#Conclusion: The texts of HC Anderson are on average more lexically diverse than those of grimm


### Hapax - richness of text
Desc(df_hca$hapax, main='Descriptive plots of mean hapax legomenon in Andersen')
Desc(df_grimm$hapax, main='Descriptive plots of mean segment type token ratio in Grimm')

shapiro.test(df_hca$hapax) # normality can't be rejected, but looks visually good
shapiro.test(df_grimm$hapax) # normality can't be rejected, but looks visually good
#Assuming normality

t.test(df_hca$hapax, df_grimm$hapax) #p>0.01
CD = CohenD(df_hca$hapax, df_grimm$hapax)

df_average$hapax = c(round(mean(df_hca$hapax), 2), round(mean(df_grimm$hapax),2), 'p<0.01', paste(round(CD, 2), 'Small', sep = ', '))


bar <- df_bind %>% group_by(author) %>% summarise(m = mean(hapax),
                                                  CI = MeanCI(hapax)[3]-MeanCI(hapax)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Average percent of hapax legomenon (mean)')


#Lexical diversity
Desc(df_hca$lex_D)
Desc(df_grimm$lex_D) #many outliers, right tail

shapiro.test(df_hca$lex_D) #normality can't be rejected, but looks visually okay
shapiro.test(df_grimm$lex_D) #normality can be rejected, but looks cisually okay

t.test(df_hca$lex_D, df_grimm$lex_D) # p < 0.001
CD = CohenD(df_hca$lex_D, df_grimm$lex_D) #small effect

df_average$lex_D = c(round(mean(df_hca$lex_D), 2), round(mean(df_grimm$lex_D),2), 'p<0.001', paste(round(CD, 2), 'Small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = mean(lex_D),
                                                  CI = MeanCI(lex_D)[3]-MeanCI(lex_D)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Average lexical Density (mean)')


### syntactic complexity

Desc(df_hca$ficht_c)
Desc(df_grimm$ficht_c)

shapiro.test(df_hca$ficht_c) #normality can be rejected -> transformation required
shapiro.test(df_grimm$ficht_c) # normality can be rejected -> transformation required

Desc(log(df_hca$ficht_c)) # log transform -> normality can't be rejected
Desc(log(df_grimm$ficht_c)) # log transform -> normality can be rejected

shapiro.test(log(df_hca$ficht_c)) #normality can't be rejected
shapiro.test(log(df_grimm$ficht_c)) # normality can be rejected 

#parametric test
wilcox.test(log(df_grimm$ficht_c), log(df_hca$ficht_c)) # significant at p<0.001
cd = cliff.delta(log(df_hca$ficht_c), log(df_grimm$ficht_c))

df_average$log_ficht_c = c(round(median(log(df_hca$ficht_c)), 2), round(median(log(df_grimm$ficht)),2), 'p<0.001', paste(round(CD, 2), 'Small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = median(log(ficht_c)),
                                                  CI = MedianCI(log(ficht_c))[3]-MedianCI(log(ficht_c))[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Syntactic complexity as the average logarithm of Ficht C (median)')

###analysing sentiment

##valence
Desc(df_hca$valence)
Desc(df_grimm$valence)

shapiro.test(df_hca$valence)#normality can't be rejected at alpha = 0.99 -> assuming normality
shapiro.test(df_grimm$valence) #normality can't be rejected at alpha = 0.99 -> assuming normality

t.test(df_hca$valence, df_grimm$valence) # p<0.001

CD = CohenD(df_hca$valence, df_grimm$valence) #medium effect

df_average$valence = c(round(mean(df_hca$valence), 2), round(mean(df_grimm$valence),2), 'p<0.001', paste(round(CD, 2), 'Medium', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = mean(valence),
                                                  CI = MeanCI(valence)[3]-MeanCI(valence)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Mean valence score')

## Arousal
Desc(df_hca$arousal)
Desc(df_grimm$arousal)

shapiro.test(df_hca$arousal) # can't assume normality
shapiro.test(df_grimm$arousal) # can't assume normality

#non parametric test
wilcox.test(df_hca$arousal, df_grimm$arousal) #p<0.001

#Effectsize cliffs delta
cd = cliff.delta(df_hca$arousal, df_grimm$arousal)#small effect

df_average$arousal = c(round(median(df_hca$arousal), 2), round(median(df_grimm$arousal),2), 'p<0.001', paste(round(cd$estimate, 2), 'Medium', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = median(arousal),
                                                  CI = MedianCI(arousal)[3]-MedianCI(arousal)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('average arousal score (median)')

## Dominance
Desc(df_hca$dominance)
Desc(df_grimm$dominance)

shapiro.test(df_hca$dominance) #normality can't be rejected
shapiro.test(df_grimm$dominance) #normality can be rejected

#non parametric tests

#non parametric test
wilcox.test(df_hca$dominance, df_grimm$dominance) #p<0.01

#Effectsize cliffs delta
cd = cliff.delta(df_hca$dominance, df_grimm$dominance)#small effect

df_average$dominance = c(round(median(df_hca$dominance), 2), round(median(df_grimm$dominance),2), 'p<0.001', paste(round(cd$estimate, 2), 'Small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = median(dominance),
                                                  CI = MedianCI(dominance)[3]-MedianCI(dominance)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Median dominance score')

outputpath = paste(getwd(), "/output", sep = "")
write.csv(df_average, paste(outputpath, '/average_measures_combined.csv', sep = ""), row.names = TRUE, quote = TRUE)

##create a table
df_average = df_average %>%
  rownames_to_column(var = " ")
colnames(df_average) = c(" ", "Author", "Length*", "Flesch-Kincaid", "MSTTR", "Hapax", "Lexical Density", "Ficht C (log)", "Valence", "Arousal*", "Dominance*" )

gt(df_average) %>%
  tab_header(title = "Average Scores Table") %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )



