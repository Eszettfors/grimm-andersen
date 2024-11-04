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
library(tidyr)
library(jtools)
library(MuMIn)
library(corrplot)
library(viridis)
library(broom)
library(stargazer)
library(rgl)
library(scatterplot3d)
library(plotly)


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

df_average$flesch_kincaid = c(round(mean(df_hca$flesch_kincaid), 3), round(mean(df_grimm$flesch_kincaid),3), 'p<0.001', paste(round(CD,3), 'Medium', sep = ', '))


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

df_average$MSTTR = c(round(mean(df_hca$MSTTR), 3), round(mean(df_grimm$MSTTR),3), 'p<0.001', paste(round(CD, 3), 'Medium', sep = ', '))


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

df_average$hapax = c(round(mean(df_hca$hapax), 3), round(mean(df_grimm$hapax),3), 'p<0.01', paste(round(CD, 3), 'Small', sep = ', '))


bar <- df_bind %>% group_by(author) %>% summarise(m = mean(hapax),
                                                  CI = MeanCI(hapax)[3]-MeanCI(hapax)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Average percent of hapax legomenon (mean)')

#Conclusion: HC andersen show a small but significant tendency to a more rich language with on average more hapax legomenon than Grimm

#Lexical density
Desc(df_hca$lex_D)
Desc(df_grimm$lex_D) #many outliers, right tail

shapiro.test(df_hca$lex_D) #normality can't be rejected, but looks visually okay
shapiro.test(df_grimm$lex_D) #normality can be rejected, but looks cisually okay

t.test(df_hca$lex_D, df_grimm$lex_D) # p < 0.001
CD = CohenD(df_hca$lex_D, df_grimm$lex_D) #small effect

df_average$lex_D = c(round(mean(df_hca$lex_D), 3), round(mean(df_grimm$lex_D),3), 'p<0.001', paste(round(CD, 3), 'Small', sep = ', '))

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

df_average$log_ficht_c = c(round(median(log(df_hca$ficht_c)), 3), round(median(log(df_grimm$ficht)),3), 'p<0.001', paste(round(cd$estimate, 3), 'Small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = median(log(ficht_c)),
                                                  CI = MedianCI(log(ficht_c))[3]-MedianCI(log(ficht_c))[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Syntactic complexity as the average logarithm of Ficht C (median)')


###analysing sentiment

Desc(df_hca$valence)
Desc(df_grimm$valence)

shapiro.test(df_hca$valence)#normality can't be rejected at alpha = 0.99 -> assuming normality
shapiro.test(df_grimm$valence) #normality can't be rejected at alpha = 0.99 -> assuming normality

t.test(df_hca$valence, df_grimm$valence) # p<0.001

CD = CohenD(df_hca$valence, df_grimm$valence) #large effect

df_average$valence = c(round(mean(df_hca$valence), 3), round(mean(df_grimm$valence),3), 'p<0.001', paste(round(CD, 3), 'Large', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = mean(valence),
                                                  CI = MeanCI(valence)[3]-MeanCI(valence)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Mean valence score')

# Conclusion: HC andersen stories are more positive than the grimm stories

Desc(df_hca$arousal)
Desc(df_grimm$arousal)

shapiro.test(df_hca$arousal) # assume normality
shapiro.test(df_grimm$arousal) # assume normality

#parametric test
t.test(df_hca$arousal, df_grimm$arousal) #p<0.001

#Effectsize cliffs delta
cd = CohenD(df_hca$arousal, df_grimm$arousal)#small effect

df_average$arousal = c(round(mean(df_hca$arousal), 3), round(mean(df_grimm$arousal),3), 'p<0.001', paste(round(cd, 3), 'small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = mean(arousal),
                                                  CI = MeanCI(arousal)[3]-MeanCI(arousal)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('average arousal score (median)')

Desc(df_hca$dominance)
Desc(df_grimm$dominance)

shapiro.test(df_hca$dominance) #normality can't be rejected
shapiro.test(df_grimm$dominance) #normality can be rejected

#parametric test
t.test(df_hca$dominance, df_grimm$dominance) #p<0.05

#Effectsize cliffs delta
cd = CohenD(df_hca$dominance, df_grimm$dominance)#small effect

df_average$dominance = c(round(mean(df_hca$dominance), 3), round(mean(df_grimm$dominance),3), 'p<0.05', paste(round(cd, 3), '  Small', sep = ', '))

bar <- df_bind %>% group_by(author) %>% summarise(m = mean(dominance),
                                                  CI = MeanCI(dominance)[3]-MeanCI(dominance)[2])
ggplot(bar, aes(author, m, fill = author)) +
  geom_bar(stat = "identity", color = "black", 
           position = position_dodge()) +
  geom_errorbar(aes(ymin = m - CI/2, ymax = m + CI/2), width = 0.2,
                position = position_dodge(0.9)) +
  ylab('Median dominance score')

outputpath = paste(getwd(), "/output", sep = "")
write.csv(df_average, paste(outputpath, '/average_measures_combined.csv', sep = ""), row.names = TRUE, quote = TRUE)



#### visuals
#create a table
df_average = df_average %>%
  rownames_to_column(var = " ")
colnames(df_average) = c(" ", "Author", "Length*", "Flesch-Kincaid", "MSTTR", "Hapax", "Lexical Density", "Ficht C (log)*", "Valence", "Arousal", "Dominance" )

gt(df_average) %>%
  tab_header(title = "Average Scores Table") %>%
  tab_style(
    style = list(
      cell_fill(color = "lightblue"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels()
  )

avrgs = df_bind %>% group_by(author) %>% summarise(
  length = median(Tokens)/10000,
  flesch_kincaid = mean(flesch_kincaid)/10, 
  MSTTR = mean(MSTTR),
  hapax = mean(hapax)/100,
  lex_D = mean(lex_D),
  log_ficht_c = median(log(ficht_c))/10,
  valence = mean(valence),
  arousal = mean(arousal),
  dominance = mean(dominance)
)

# barplots
avrg_longer = avrgs %>% pivot_longer(cols = c('length','flesch_kincaid', 'MSTTR', 'hapax', 'lex_D','log_ficht_c', 'valence', 'arousal', 'dominance'), names_to = "measure", values_to = 'averages') 
avrg_longer$measure = factor(avrg_longer$measure, levels = c('length','flesch_kincaid', 'MSTTR', 'hapax', 'lex_D','log_ficht_c', 'valence', 'arousal', 'dominance'))

avrg_plot = ggplot(data = avrg_longer,
                   map = aes(y = averages,x = measure, fill = author, color = author)) + geom_bar(position = 'dodge', stat = 'identity')
avrg_plot + labs(title = 'Comparison of average measurements between authors rescaled', y = 'averages', x = 'measures') + scale_x_discrete(labels = c('Length*', 'Flesch Kincaid','MSTTR' ,'Hapax', 'Lexical Density',"Log Fichtner's C*", 'Valence', 'Arousal', 'Dominance')) + theme_apa()


#boxplots
sent_long = df_bind %>% pivot_longer(cols = c('valence', 'arousal', 'dominance'), names_to = 'sent_measures', values_to = 'sent_values')
sent_long$sent_measures = factor(sent_long$sent_measures, levels = c('valence','arousal', 'dominance'))

sent_box = ggplot(data = sent_long,
                  map = aes(x = sent_measures, 
                            y = sent_values, fill = author, color = author)) + geom_boxplot(position = 'dodge', color = 'black')
sent_box + labs(title = 'Comparison of Distribution of Sentiment Values by Author', y = 'Sentiment Values', x = 'Sentiment Measurements') + theme_apa()

num_cols = sapply(df_bind, is.numeric)
df_nums = subset(df_bind, select = num_cols)
df_nums = subset(df_nums, select = -c(Types, Sentences, grm_D))
test = scale(df_nums$valence)[,1]

for (i in 1:length(colnames(df_nums))){
  df_nums[,i] = scale(df_nums[,i])[,1]
}

cols_to_pivot = colnames(df_nums)
df_nums$author = df_bind$author

nums_long = df_nums %>% pivot_longer(cols = all_of(cols_to_pivot), names_to = "measures", values_to = "values")
nums_long$measures = factor(nums_long$measures, level = c("Tokens", "flesch_kincaid","MSTTR", "hapax","lex_D", "ficht_c", "valence", "arousal", "dominance"))

nums_box = ggplot(data = nums_long,
                  map = aes(x = measures, y = values, fill = author, color = author)) + geom_boxplot(position = 'dodge', color = "black")
nums_box + labs(title = 'Distribution of z-transformed Measures by Author as Boxplots', y = 'Values', x = 'Measures') + scale_x_discrete(labels = c('Length', 'Flesch Kincaid','MSTTR' ,'Hapax', 'Lexical Density',"Fichtner's C", 'Valence', 'Arousal', 'Dominance')) + theme_apa()



# scatterplots

plot_ly(df_bind, 
        x = ~valence, 
        y = ~arousal, 
        z = ~dominance,
        color = ~author,  # Color points by author
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5),
        text = ~paste("Valence:", valence, 
                      "<br>Arousal:", arousal, 
                      "<br>Dominance:", dominance, 
                      "<br>Author:", author, 
                      "<br>Title:", df_bind$title),  # Custom hover text
        hoverinfo = "text")  # Display hover text only



#### modelling

model = subset(df_bind, select = -c(Text, title, Types, Tokens, Sentences, grm_D))
model$author = as.factor(model$author)
attach(model)

nums_cols = sapply(model, is.numeric)
z_model = model
z_model = model[, nums_cols] = scale(z_model[, nums_cols])
z_model = as.data.frame(z_model[,])


cm = cor(model[,nums_cols])
corrplot(cm)
cor.mtest(cm)
cor.test(flesch_kincaid, ficht_c)

z_model$author = author
author_mdl = glm(data = z_model, author ~ ., family = binomial(link = "logit") )
summary(author_mdl)
plot_coefs(author_mdl) + labs(title = "Coefficients for Infering Authorship, full model") + theme_apa()
stargazer(author_mdl, out = "aut_mdl_output.tex", type = "latex")

z_model_2 = subset(z_model, select = -c(ficht_c))
author_mdl_2 = glm(data = z_model_2, author ~ ., family = binomial(link = "logit") )
summary(author_mdl_2)

z_model_3 = subset(z_model, select = -c(flesch_kincaid))
author_mdl_3 = glm(data = z_model_3, author ~ ., family = binomial(link = "logit") )
summary(author_mdl_3)

author_mdl_4 = glm(data = z_model, author ~ . + arousal:dominance + valence:dominance + valence:arousal, family = binomial(link = "logit") )
summary(author_mdl_4)
plot_coefs(author_mdl_4) + labs(title = "Coefficients for Infering Authorship") + theme_apa()


options(na.action = 'na.fail')
dd = dredge(author_mdl, rank = "AICc")
confset = subset(dd, cumsum(dd$weight) <= .95)
average_model = model.avg(confset)
confset
coef(confset)[-1]
sw(dd)
vars = names(sw(dd))
rvi = as.vector(sw(dd))

barplot(rvi, col = "darkmagenta", names.arg = vars, ylab = "relative variable importance", main = "Relative Variable Importance for Valence")
df_rvi = as.data.frame(rvi)
df_rvi$vars = vars
df_rvi$vars = factor(vars, levels = vars)
ggplot(data = df_rvi,
       map = aes(y = rvi, x = vars)) + geom_bar(stat = 'identity', col = 'darkmagenta', fill = 'darkmagenta') + labs(title = "Relative Variable Importance for Inferring Authorship", x = 'Variables', y = 'Relative Variable Importance') + theme_apa()


stargazer(author_mdl, author_mdl_2, author_mdl_3,title = "comparison of base models", column.labels = c("Full model",  "Without Fichtner's C", "without Flesch Kincaid"),  out = "glm_output.tex", type = "latex")



sent_mod = glm(data = z_model, author ~ valence * arousal * dominance ,family = binomial(link = "logit"))
summary(sent_mod)
