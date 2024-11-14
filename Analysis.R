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
library(htmlwidgets)

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
  flesch_kincaid = median(flesch_kincaid)/10, 
  MSTTR = median(MSTTR),
  hapax = median(hapax)/100,
  lex_D = median(lex_D),
  ficht_c = median(ficht_c)/100,
  valence = median(valence),
  arousal = median(arousal),
  dominance = median(dominance)
)

# barplots
avrg_longer = avrgs %>% pivot_longer(cols = c('length','flesch_kincaid', 'MSTTR', 'hapax', 'lex_D','ficht_c', 'valence', 'arousal', 'dominance'), names_to = "measure", values_to = 'averages') 
avrg_longer$measure = factor(avrg_longer$measure, levels = c('length','flesch_kincaid', 'MSTTR', 'hapax', 'lex_D','ficht_c', 'valence', 'arousal', 'dominance'))

avrg_plot = ggplot(data = avrg_longer,
                   map = aes(y = averages,x = measure, fill = author, color = author)) + geom_bar(position = 'dodge', stat = 'identity')
avrg_plot + labs(title = 'Comparison of average measurements between authors rescaled', y = 'averages', x = 'measures') + scale_x_discrete(labels = c('Length', 'Flesch Kincaid','MSTTR' ,'Hapax', 'Lexical Density',"Fichtner's C", 'Valence', 'Arousal', 'Dominance')) + theme_apa()


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

nums_box
#heavy outlier in grimm MSTTR -> domestic servant ,  repeats the sentences over and over
# remove from analysis
df_bind = df_bind %>% filter(title != "Domestic Servants")


# scatterplots
grimm_stories = read.xlsx("data/Corpus_grimm_with_text.xlsx")
andersen_stories = read.xlsx("data/Corpus_HCA.xlsx")

grimm_stories$text = gsub("\n", " ", grimm_stories$text)
andersen_stories$text = gsub("\n", " ", andersen_stories$text)

stories_bind = rbind(grimm_stories, andersen_stories)
df_plot = merge(df_bind, stories_bind, by = "title")

fig = plot_ly(df_plot, 
        x = ~valence, 
        y = ~arousal, 
        z = ~dominance,
        color = ~author,  # Color points by author
        type = "scatter3d", 
        mode = "markers",
        marker = list(size = 5),
        text = ~paste("Valence:", round(valence,3), 
                      "<br>Arousal:", round(arousal,3), 
                      "<br>Dominance:", round(dominance,3), 
                      "<br>Author:", author, 
                      "<br>Title:", df_plot$title),  # Custom hover text
        customdata = df_plot$text,
        hoverinfo = "text")  # Display hover text only

fig <- fig %>% htmlwidgets::onRender("
  function(el, x) {
  
    var modal = document.createElement('div');
    modal.style.display = 'none';
    modal.style.position = 'fixed';
    modal.style.zIndex = '1000';
    modal.style.left = '50%';
    modal.style.top = '50%';
    modal.style.transform = 'translate(-50%, -50%)';
    modal.style.backgroundColor = '#fff';
    modal.style.padding = '20px';
    modal.style.border = '1px solid #888';
    modal.style.borderRadius = '8px';
    modal.style.boxShadow = '0px 0px 10px rgba(0, 0, 0, 0.2)';

    var closeButton = document.createElement('button');
    closeButton.innerText = 'Close';
    closeButton.style.marginTop = '10px';
    closeButton.onclick = function() {
      modal.style.display = 'none';
    };

    var content = document.createElement('p');
    var content = document.createElement('div');  // Changed from <p> to <div> to allow scrolling
    content.style.maxHeight = '300px';  // Set max height to make it scrollable
    content.style.overflowY = 'auto';   // Enable vertical scrolling if content exceeds max height
    content.style.marginBottom = '10px'; // Add space between text and close button
    
    
    modal.appendChild(content);
    modal.appendChild(closeButton);
    document.body.appendChild(modal);

    el.on('plotly_click', function(data) {
      var storyText = data.points[0].customdata;
      content.innerText = storyText;
      modal.style.display = 'block';
    });
  }
")

saveWidget(fig, "3d_scatterplot.html", selfcontained = TRUE)



#### modelling
#inferential modelling

model = subset(df_bind, select = -c(Text, title, Types, Sentences, grm_D))


model$author = as.factor(model$author)
levels(model$author) = c("HC Andersen", "Grimm")
colnames(model)[1] = "length"
model = model %>% 
attach(model)

nums_cols = sapply(model, is.numeric)
z_model = model
z_model = model[, nums_cols] = scale(z_model[, nums_cols])
z_model = as.data.frame(z_model[,])


cm = cor(model[,nums_cols])
corrplot(cm)
cor.mtest(cm)
cor.test(flesch_kincaid, ficht_c)# high correlation -> check VIF

z_model$author = author
author_mdl = glm(data = z_model, author ~ ., family = binomial(link = "logit") )
summary(author_mdl)#AIC = 134.66
plot_coefs(author_mdl) + labs(title = "Coefficients for Infering Authorship, full model") + theme_apa()
stargazer(author_mdl, out = "aut_mdl_output.tex", type = "latex") 
VIF(author_mdl) #flesch kincaid and ficht_C have high VIF

z_model_2 = subset(z_model, select = -c(ficht_c))
author_mdl_2 = glm(data = z_model_2, author ~ ., family = binomial(link = "logit") )
summary(author_mdl_2) #AIC = 290
plot_coefs(author_mdl_2)
VIF(author_mdl_2) 

z_model_3 = subset(z_model, select = -c(flesch_kincaid))
author_mdl_3 = glm(data = z_model_3, author ~ ., family = binomial(link = "logit") )
summary(author_mdl_3) #AIC 288.19
plot_coefs(author_mdl_3)+ labs(title = "Coefficients for Infering Authorship, third model") + theme_apa()
VIF(author_mdl_3)
#removing flesch-kincaid C has better AIC -> chosen to keep

#adding interaction terms for sentiment
author_mdl_4 = glm(data = z_model_3, author ~ . + arousal:dominance + valence:dominance + valence:arousal, family = binomial(link = "logit") )
summary(author_mdl_4) #285.91
plot_coefs(author_mdl_4) + labs(title = "Coefficients for Infering Authorship, Best model") + 
  scale_y_discrete(labels = c('Length','MSTTR' ,'Hapax', 'Lexical Density',"Fichtner's C", 'Valence', 'Arousal', 'Dominance', 'Arousal:Dominance', 'Valence:Dominance', 'Valence:Arousal')) + 
                     theme_apa()


#plotting best model
sum = summary(author_mdl_4)
coefs = as.data.frame(sum$coefficients)
coefs$Estimate
colnames(coefs) = c("Estimate", "Error", "z", "p")
coefs = coefs %>% mutate(OR = exp(Estimate), significance = case_when(p < 0.05 ~"significant", p > 0.05 ~ "not significant"))
coefs = coefs[-1, ]
coefs$variable = rownames(coefs)
ggplot(data = coefs,
       map = aes(y = OR, x = variable, fill = significance, color = significance)) + geom_bar(stat = 'identity') + 
  labs(title = "Relative increase in odds of HC anderson being author per standard deviation increase in sentiment and complexity") + geom_hline(yintercept = 1, linetype = "dashed") + scale_x_discrete(labels = c('Arousal', 'Arousal:Dominance','Dominance',"Fichtner's C", 'Hapax',"Length", 'Lexical Density','MSTTR', 'Valence', 'Valence:Arousal', 'Valence:Dominance')) +theme_apa() +
  scale_fill_manual(values = c("significant" = "darkgreen", "not significant" = "gray")) + 
  scale_color_manual(values = c("significant" = "darkgreen", "not significant" = "darkgray")) 
#
VAD_mdl = glm(data = z_model, author ~ valence * dominance * arousal, family = binomial(link = "logit") )
summary(VAD_mdl)
plot_coefs(VAD_mdl) + labs(title = "Coefficients for Infering Authorship, sentiment") + theme_apa()
sum = summary(VAD_mdl)
vad_coefs = as.data.frame(sum$coefficients)
vad_coefs$Estimate
colnames(vad_coefs) = c("Estimate", "Error", "z", "p")
vad_coefs = vad_coefs %>% mutate(OR = exp(Estimate), significance = case_when(p < 0.05 ~"significant", p > 0.05 ~ "not significant"))
vad_coefs = vad_coefs[-1, ]
vad_coefs$variable = rownames(vad_coefs)
ggplot(data = vad_coefs,
       map = aes(y = OR, x = variable, fill = significance, color = significance)) + geom_bar(stat = 'identity') + 
       labs(title = "Relative increase in odds of HC anderson being author per standard deviation increase in sentiment") + geom_hline(yintercept = 1, linetype = "dashed") + theme_apa() 





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

stargazer(author_mdl, author_mdl_2, author_mdl_3, author_mdl_4,title = "comparison of models", column.labels = c("Full Model",  "Without Fichtner's C", "Without Flesch Kincaid", "Best Model", "VAD Model"),  out = "glm_output.tex", type = "latex")



# if there truly is a difference between the language used, then the language should be able to be used to predict if the author of a text is Andersen or the Grimm brothers.
# predictive modelling

library(caret)
df_bind
data = subset(df_bind, select = -c(Text, Types, Sentences, title, grm_D))
head(data)
data$author = as.factor(data$author)
levels(data$author) = c("HC Andersen", "Grimm")


set.seed(42)
inTrain = createDataPartition(
  y = data$author,
  p = 0.75,
  list = FALSE
)
training = data[inTrain, ]
test = data[-inTrain,]


###fullmod
fullmod = train(
  author ~.,
  data = training,
  method = "glm",
  family = binomial()
)
summary(fullmod)


fullmodpred_train = predict(fullmod, training)
fullmodpred_test = predict(fullmod, test)

cm_train = confusionMatrix(fullmodpred_train, training$author)
print(cm_train) #acc = 0.92

cm_test = confusionMatrix(fullmodpred_test, test$author)
print(cm_test) # acc = 0.91


#linguistic model

ling = subset(data, select = -c(valence, arousal, dominance))
  
inTrain = createDataPartition(
  y = ling$author,
  p = 0.75,
  list = FALSE
)
training = ling[inTrain, ]
test = ling[-inTrain,]

lingmod = train(
  author ~.,
  data = ling,
  method = "glm",
  family = binomial()
)
summary(lingmod) #aic = 141

lingmodpred_train = predict(lingmod, training)
lingmodpred_test = predict(lingmod, test)

cm_train = confusionMatrix(lingmodpred_train, training$author)
print(cm_train) #acc = 0.93

cm_test = confusionMatrix(lingmodpred_test, test$author)
print(cm_test) # acc = 0.89


#vadmod
vad = subset(data, select = c(author, valence, arousal, dominance))

inTrain = createDataPartition(
  y = vad$author,
  p = 0.75,
  list = FALSE
)
training = vad[inTrain, ]
test = vad[-inTrain,]

vadmod = train(
  author ~ valence * arousal * dominance,
  data = vad,
  method = "glm",
  family = binomial()
)
summary(vadmod) #aic = 415

vadmodpred_train = predict(vadmod, training)
vadmodpred_test = predict(vadmod, test)

cm_train = confusionMatrix(vadmodpred_train, training$author)
print(cm_train) #acc = 0.71

cm_test = confusionMatrix(vadmodpred_test, test$author)
print(cm_test) # acc = 0.70


#### linguistic complexity is a better predictor of authorship than sentiment
