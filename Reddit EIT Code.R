#Contents:
#Section 1: Packages
#Section 2: Data Load
#Section 3: Text Analysis
#Section 4: Hypothesis Testing
#Section 5: Figures
#Section 6: Regression Tables

###################################################################################
#Section 1: Packages
library(dplyr)
library(plyr)
library(tm)
library(ggplot2)
library(reshape2)
library(wordcloud)
library(RWeka)
library(NLP)
library(gdata)
library(gsubfn)
library(nlme)
library(latticeExtra)
library(ggplot2)
library(qwraps2)
library(stargazer)
library(rlang)
library(gridExtra)
library(jtools)
library(tidyverse)
library(Hmisc)

###################################################################################
#Section 2: Load necessary files
setwd('C:\\Users\\19789\\Documents\\Journal Submissions\\Ch. 3')
#Loads the raw text data for Section 3.
premeasure <- readLines(file('premeasure_final.txt'), encoding = 'utf-8')#premeasurement corpus
postmeasure <- readLines(file('postmeasure_final.txt'), encoding = 'utf-8')#postmeasurement corpus, only posts made outside of far-right subreddits
arpostmeasure <- readLines(file('frpostmeasure_final.txt'), encoding = 'utf-8')#postmeasurement corpus, only posts made in far-right subreddits

#Loads unigram and bigram dictionaries for text analysis in Section 3.
frdict_bi <- readLines(file('FR Bigram Dictionary.txt', encoding = 'utf-8'))
frdict_uni <- readLines(file('FR Unigram Dictionary.txt', encoding = 'utf-8'))

#Loads the completed dataset with posting and text analysis results for Section 4 and 5.
data <- read.csv("T_D Reddit Data_14_17_Anonymized.csv", header = T, stringsAsFactors = F)#Loads the completed dataset for section 4-5

###################################################################################
#Section 3: Text Analysis (Term analysis results already displayed in complete dataset.)

#Premeasurement Corpus Analysis
supercount1 <- c()#Vector recording the total bi- and unigram counts for each user's monthly posts.
matches1 <- c()#Vector containing all words found in the corpus that contained a dictionary word.
gramsperuser1 <- list() #List containing per-user vectors of dictionary terms found.
dictmatch1 <- c() #Dictionary words that were matched.

for(i in 1:530){   
  print(i)
  if(premeasure[i]=='[[]]'){
    supercount1[i] <- NA
    i = i+1   }
  else{
    dictbucket <- c()
    countbucket = 0
    someCleanText <- premeasure[i] 
    aCorpus <- VCorpus(VectorSource(someCleanText)) 
    aCorpus = tm_map(aCorpus, removePunctuation)
    aCorpus = tm_map(aCorpus, content_transformer(tolower))
    aCorpus = tm_map(aCorpus, removeWords, c(stopwords("english")))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
    aTDM <- TermDocumentMatrix(aCorpus, control = list(tokenize = BigramTokenizer,
                                                       removePunctuation = TRUE,
                                                       stopwords = c("and", "the"),
                                                       stemming = FALSE))
    bigrams <- rep(row.names(as.matrix(aTDM)), aTDM$v)
    aCorpus_uni <- VCorpus(VectorSource(someCleanText)) 
    aCorpus_uni = tm_map(aCorpus_uni, content_transformer(tolower))
    aCorpus_uni = tm_map(aCorpus_uni, removeWords, c(stopwords("english")))
    aCorpus_uni = tm_map(aCorpus_uni, removePunctuation)
    UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1)) #Change n here
    aTDM_uni <- TermDocumentMatrix(aCorpus_uni, control = list(tokenize = UnigramTokenizer,
                                                               removePunctuation = TRUE,
                                                               stopwords = c("and", "the"),
                                                               stemming = FALSE))
    unigrams <- rep(row.names(as.matrix(aTDM_uni)), aTDM_uni$v)
    unigrams <- unigrams[grepl("http", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("www", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("imgur", unigrams) == FALSE ]
    for(biterm in frdict_bi) {
      matchtest_bi <- grep(biterm, bigrams, value = TRUE, ignore.case = TRUE)
      # print(c(term, length(matchtest_bi)))
      if(length(matchtest_bi) > 0){
        matches1 <- c(matches1,matchtest_bi)
        dictbucket <- c(dictbucket, biterm)
        countbucket = countbucket + length(matchtest_bi)    }    }
    for(uniterm in frdict_uni) {
      # print(c("searching for", term))
      matchtest_uni <- grep(uniterm, unigrams, value = TRUE, ignore.case = TRUE)
      # print(matchtest_uni)
      # print(c(term, length(matchtest_uni)))
      if(length(matchtest_uni) > 0){
        matches1 <- c(matches1,matchtest_uni)
        dictbucket <- c(dictbucket, uniterm)
        countbucket = countbucket + length(matchtest_uni)    }}
    if(countbucket == 0){
      dictbucket <- c(dictbucket, "NA")    }
    supercount1 <- c(supercount1, countbucket)
    dictmatch1 <- c(dictmatch1, dictbucket)
    gramsperuser1[[i]] <- dictbucket}}

#Postmeasurement corpus, posts made outside of far-right communities.
supercount2 <- c()
matches2 <- c()
gramsperuser2 <- list() 
dictmatch2 <- c()

for(i in 1:530){  
  print(i)
  if(postmeasure[i]=='[[]]'){
    supercount2[i] <- NA
    i = i+1  }
  else{
    dictbucket <- c()
    countbucket = 0
    someCleanText <- postmeasure[i] 
    aCorpus <- VCorpus(VectorSource(someCleanText)) 
    aCorpus = tm_map(aCorpus, removePunctuation)
    aCorpus = tm_map(aCorpus, content_transformer(tolower))
    aCorpus = tm_map(aCorpus, removeWords, c(stopwords("english")))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
    aTDM <- TermDocumentMatrix(aCorpus, control = list(tokenize = BigramTokenizer,
                                                       removePunctuation = TRUE,
                                                       stopwords = c("and", "the"),
                                                       stemming = FALSE))
    bigrams <- rep(row.names(as.matrix(aTDM)), aTDM$v)
    aCorpus_uni <- VCorpus(VectorSource(someCleanText)) 
    aCorpus_uni = tm_map(aCorpus_uni, content_transformer(tolower))
    aCorpus_uni = tm_map(aCorpus_uni, removeWords, c(stopwords("english")))
    aCorpus_uni = tm_map(aCorpus_uni, removePunctuation)
    UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1)) #Change n here
    aTDM_uni <- TermDocumentMatrix(aCorpus_uni, control = list(tokenize = UnigramTokenizer,
                                                               removePunctuation = TRUE,
                                                               stopwords = c("and", "the"),
                                                               stemming = FALSE))
    unigrams <- rep(row.names(as.matrix(aTDM_uni)), aTDM_uni$v)
    unigrams <- unigrams[grepl("http", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("www", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("imgur", unigrams) == FALSE ]
    for(biterm in frdict_bi) {
      matchtest_bi <- grep(biterm, bigrams, value = TRUE, ignore.case = TRUE)
      # print(c(term, length(matchtest_bi)))
      if(length(matchtest_bi) > 0){
        matches2 <- c(matches2,matchtest_bi)
        dictbucket <- c(dictbucket, biterm)
        countbucket = countbucket + length(matchtest_bi)      }    }
    for(uniterm in frdict_uni) {
      # print(c("searching for", term))
      matchtest_uni <- grep(uniterm, unigrams, value = TRUE, ignore.case = TRUE)
      # print(matchtest_uni)
      # print(c(term, length(matchtest_uni)))
      if(length(matchtest_uni) > 0){
        matches2 <- c(matches2,matchtest_uni)
        dictbucket <- c(dictbucket, uniterm)
        countbucket = countbucket + length(matchtest_uni)    }}
    if(countbucket == 0){
      dictbucket <- c(dictbucket, "NA")    }
    supercount2 <- c(supercount2, countbucket)
    dictmatch2 <- c(dictmatch2, dictbucket)
    gramsperuser2[[i]] <- dictbucket}}

#Postmeasurement corpus, posts made in far-right communities.
supercount3 <- c()
matches3 <- c()
gramsperuser3 <- list() 
dictmatch3 <- c()
for(i in 1:530){   
  print(i)
  if(arpostmeasure[i]=='[[]]'){
    supercount3[i] <- NA
    i = i+1  }
  else{
    dictbucket <- c()
    countbucket = 0
    someCleanText <- arpostmeasure[i] # plug in desired text
    
    aCorpus <- VCorpus(VectorSource(someCleanText)) 
    aCorpus = tm_map(aCorpus, removePunctuation)
    aCorpus = tm_map(aCorpus, content_transformer(tolower))
    aCorpus = tm_map(aCorpus, removeWords, c(stopwords("english")))
    BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
    aTDM <- TermDocumentMatrix(aCorpus, control = list(tokenize = BigramTokenizer,
                                                       removePunctuation = TRUE,
                                                       stopwords = c("and", "the"),
                                                       stemming = FALSE))
    bigrams <- rep(row.names(as.matrix(aTDM)), aTDM$v)
    
    aCorpus_uni <- VCorpus(VectorSource(someCleanText)) 
    aCorpus_uni = tm_map(aCorpus_uni, content_transformer(tolower))
    aCorpus_uni = tm_map(aCorpus_uni, removeWords, c(stopwords("english")))
    aCorpus_uni = tm_map(aCorpus_uni, removePunctuation)
    UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1)) #Change n here
    aTDM_uni <- TermDocumentMatrix(aCorpus_uni, control = list(tokenize = UnigramTokenizer,
                                                               removePunctuation = TRUE,
                                                               stopwords = c("and", "the"),
                                                               stemming = FALSE))
    unigrams <- rep(row.names(as.matrix(aTDM_uni)), aTDM_uni$v)
    unigrams <- unigrams[grepl("http", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("www", unigrams) == FALSE ]
    unigrams <- unigrams[grepl("imgur", unigrams) == FALSE ]
    for(biterm in frdict_bi) {
      matchtest_bi <- grep(biterm, bigrams, value = TRUE, ignore.case = TRUE)
      # print(c(term, length(matchtest_bi)))
      if(length(matchtest_bi) > 0){
        matches3 <- c(matches3,matchtest_bi)
        dictbucket <- c(dictbucket, biterm)
        countbucket = countbucket + length(matchtest_bi)      }    }
    for(uniterm in frdict_uni) {
      # print(c("searching for", term))
      matchtest_uni <- grep(uniterm, unigrams, value = TRUE, ignore.case = TRUE)
      # print(matchtest_uni)
      # print(c(term, length(matchtest_uni)))
      if(length(matchtest_uni) > 0){
        matches3 <- c(matches3,matchtest_uni)
        dictbucket <- c(dictbucket, uniterm)
        countbucket = countbucket + length(matchtest_uni)    }}
    if(countbucket == 0){
      dictbucket <- c(dictbucket, "NA")    }
    supercount3 <- c(supercount3, countbucket)
    dictmatch3 <- c(dictmatch3, dictbucket)
    gramsperuser3[[i]] <- dictbucket}}

data$precount <- supercount1 #Writes term analysis results (gram counts) to main dataset
gramsperuser1#Use this for overlap analysis.
dictmatch3#Use this for Top 25 Terms figure.
# write.csv(dictmatch, "Premeasure Dictionary Terms Found2.csv")

###################################################################################
#Hypothesis Testing

#H1
#T-Test
postsum <- data$netpostcount
presum <- data$precount
t.test(postsum - presum, alternative = 'greater')

#Overlap analysis
data$overlapterms <- NA
for(i in 1:530){
  # print(i)
  x <- c(gramsperuser2[[i]], gramsperuser3[[i]])
  overlaptest <- intersect(x, gramsperuser1[[i]])
  if(length(overlaptest) != 0){
    data$overlapterms[i] <- overlaptest  }}
data$overlapterms[data$overlapterms == 'NA'] <- NA
data$overlapterms

#H2
#Change in term use over time by engagement score (expressed as count) 
r1 <- lm(data$netdiff ~ data$newarscores + data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r1)

#Change in term use over time by engagement score (expressed as rate per 100 posts) 
r2 <- lm(data$diff_in_rate ~ data$newarscores+ data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r2)

#H3a
#Effect of engagement on internal term use (as count)
r3 <- lm(data$arpostcount ~ data$newarscores + data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r3)

#Effect of engagement on internal term use (as rate per 100 posts)
data$rate_int <- data$arpostcount/data$arsubcount_t3
r4 <- lm(data$rate_int*100 ~ data$newarscores + data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r4)

#H3b
#Effect of engagement on external term use (as count)
r5 <- lm(data$postcount ~ data$newarscores + data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r5)

#Effect of engagement on external term use (as rate per 100 posts)

data$rate_ext <- data$postcount/(data$t3_postcount-data$arsubcount_t3)
r6 <- lm(data$rate_ext*100 ~ data$newarscores + data$prec_cons + data$prec_rac + 
           data$prec_vio + data$prec_sex + data$prec_off + data$prec_gam)
summary(r6)

###################################################################################
#Figures

#Fig. 1, "Top 25 Far-Right Terms"
bplt <- data.frame(table(c(dictmatch1, dictmatch2, dictmatch3))) #Load these objects using Sec. 3 code.
bplt2 <- bplt[order(-bplt$Freq),][1:25,]
v1<- ggplot(bplt2, aes(x=reorder(Var1, -Freq), y = Freq))+ geom_bar(stat = "identity") + 
  ggtitle('Top 25 FR Terms') +
  xlab('')+
  ylab('')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
v1

#Fig. 2, "Far-Right Term Use, Postmeasure"
cats <- c('Internal', 'External')
nums <- c(sum(data$arpostcount, na.rm=TRUE),sum(data$postcount, na.rm=TRUE))
brplt <- data.frame(cats, nums)
v2a <- ggplot(brplt, aes(x=cats, y = nums))+ geom_bar(stat = "identity") + 
  ggtitle('Far-Right Term Use, Postmeasure') +
  xlab('')+
  ylab('Count')
totalrate_int<-sum(data$arpostcount, na.rm=TRUE)/sum(data$arsubcount_t3)
totalrate_ext <- sum(data$postcount, na.rm=TRUE)/sum(data$t3_postcount)
catsb <- c('Internal', 'External')
numsb <- c(totalrate_int*100,totalrate_ext*100)
brpltb <- data.frame(catsb, numsb)
v2b <- ggplot(brpltb, aes(x=catsb, y = numsb))+ geom_bar(stat = "identity") + 
  ggtitle('') +
  xlab('')+
  ylab('Rate per 100 Posts')
grid.arrange(v2a, v2b)

#Fig. 3, "Mean Increase in Far-Right Term Use"
samplegroups <- c(rep('Premeasure', 530), rep('Postmeasure', 530))
diffs <- data.frame(postsum - presum)
v3 <- ggplot(diffs, aes(x = postsum...presum)) +
  geom_density(color="darkblue", fill="lightblue") +
  geom_vline(aes(xintercept=mean(postsum - presum, na.rm = T)),
             color="blue", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=0),
             color="grey", linetype="dashed", size=1) +
  ggtitle('Mean Increase in Far-Right Term Use')+
  xlab('Postmeasure - Premeasure') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(-20,20) +
  ylim(0,0.20) +
  geom_text(x=-13, y=.17, label= expression('One-sided t-test:')) +
  geom_text(x=-16.2, y=.15, label= expression(paste(mu, ' = 3.44')))+
  geom_text(x=-10.2, y=.13, label= expression('t(530) = 11.324; p < .01'))
v3

#Fig. 4, "Instances of Within-User Term Overlap"
bplt3 <- data.frame(table(data$overlapterms))
bplt4 <- bplt3[bplt3$Freq > 1,]
v4 <- ggplot(bplt4, aes(x=reorder(Var1, -Freq), y = Freq))+ geom_bar(stat = "identity") + 
  ggtitle('Instances of Within-User Term Overlap') +
  xlab('')+
  ylab('')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
v4

#Fig. 5, "Effects of Engagement on External Term Use"
v5 <- ggplot(data, aes(x=newarscores, y=rate_ext)) +
  geom_ribbon(stat = 'smooth', method = 'lm', se = TRUE, alpha = 0.2, fill = 'dodgerblue')+
  geom_line(stat="smooth",method="lm") +
  labs(title = "Effects of Engagement on External Term Use", x = 'Engagement Score', y = 'Term Rate per 100 Posts')+
  theme(plot.title = element_text(hjust = 0.3))
v5

###################################################################################
#Regression Tables

#Table 2
stargazer(r1, r2, title = 'Change in Term Use between Pre- and Postmeasurement',
          dep.var.labels = c('Change in Term Count','Change in Term Rate'), 
          column.sep.width = "0pt", covariate.labels = c("Engagement Score", 'Conspiracy Precursor',
                                                         'Racist Precursor', 'Violent Precursor',
                                                         'Sexist Precursor', 'Offensive Precursor',
                                                         'Gamer Precursor'))

#Table 3
stargazer(r3, r4, title = 'Effect of Engagement Score on Internal Term Use at Postmeasurement',
          dep.var.labels = c('Term Count','Term Rate per 100 posts'), 
          column.sep.width = "0pt", covariate.labels = c("Engagement Score", 'Conspiracy Precursor',
                                                         'Racist Precursor', 'Violent Precursor',
                                                         'Sexist Precursor', 'Offensive Precursor',
                                                         'Gamer Precursor'))

#Table 4
stargazer(r5, r6, title = 'Effect of Engagement Score on External Term Use at Postmeasurement',
          dep.var.labels = c('Term Count','Term Rate per 100 posts'), 
          column.sep.width = "0pt", covariate.labels = c("Engagement Score", 'Conspiracy Precursor',
                                                         'Racist Precursor', 'Violent Precursor',
                                                         'Sexist Precursor', 'Offensive Precursor',
                                                         'Gamer Precursor'))
