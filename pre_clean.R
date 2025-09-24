# preprocess title and abstract in US major dailies
# https://htmlpreview.github.io/?https://github.com/wesslen/NCStateSenateFacebook/blob/master/code/STM-ncsenate-facebook-part1.html

#rm(list = ls())

##Load required packages
if (!requireNamespace("pacman")) install.packages('pacman')
library(pacman)
packages<-c("tidyverse","lubridate","glue","stm","quanteda","tm","dplyr",
            "haven","here", "stringr","magrittr")
p_load(packages,character.only = TRUE)

#load data
news1 <- read.csv("/Users/lin/Documents/projects/USnews/trackpaper/fulltext_cleaned.csv")
#news <- read.csv("/gpfs/home/haolin1/FUN/US_fulltext.csv")
#write.csv(new_df,"/gpfs/home/haolin1/FUN/fulltext_cleaned.csv" )
colnames(news1)
names(news1) <- gsub(" ", "_", names(news1))
names(news1) <- gsub("_/_", "_", names(news1))
colnames(news1)

# create a subset dataset
news <- subset (news1, Publication.year == 2022)
head(news)

#check number of abstract
sum(is.na(news$Abstract))
#7084

# generate publisher
new_df <- news1 %>%
  mutate(newspaper = case_when(
    str_detect(Publication.info,pattern = "Wall") ~ "wall_street_journal",
    str_detect(Publication.info,pattern = "Washington Post") ~ "washtington_post",
    str_detect(Publication.info,pattern = "York Times") ~ "new_york_times",
    str_detect(Publication.info, pattern = "Angeles Times") ~"los_angeles_times",
    str_detect(Publication.info, pattern = "Chicago Tribune") ~"Chicago_Tribune",
    TRUE ~ NA_character_
  ))

# full text
new_df <- transform(new_df, Full.text = as.character(Full.text),
                    Publication.year = as.numeric(Publication.year),
                    Publication.date = as.character(Publication.date)
                    )


low[100, ]
# this give me the chance to look deeper at the dataset, which helps me pay more 
# attention on "section", turn out that lots of articles are not "newspaper"
table(new_df$Source.type)
new_df$Source.type <- gsub(" ", "", new_df$Source.type)

#             AudioorVideoWork Blog,Podcast,orWebsite 
#8                     51                   4520 
#Magazine              Newspaper 
#184                   3597 

# keep those from newspaper sections
allnews <- new_df%>%
  filter(Source.type=="Newspaper")
table(allnews$topic_asian)
#0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19 
#628 915 444 285 230 124 113  82  75  61  50  54  54  52  41  16  33  35  22  23 
#20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  37  38  39  40 
#22  15  19  21  21  17  12  11   8   5  12   6   6   2   7   4   7   6   5   5 
#41  42  43  44  45  46  47  48  49  50  52  53  54  57  58  65  70  71  73  79 
#7   2   3   2   3   6   3   2   3   3   1   1   3   1   1   1   1   2   2   1 
#123 
#1 

lownews <- allnews%>%
  filter(topic_asian<4)
lownews[100, ]
lownews[1000, ]
# this is where I found that my dataset is not complete, even for the part that 
# I downloaded by myself

#########################
# how to solve the issue of 
# lack of dataset
#########################

#  Publication.year = as.numeric(year)
typeof(new_df$Full.text)
typeof(new_df$Publication.year)

# publication year
# use publication year and publication date to complete the value of publication year
new_df$Publication.year  <- gsub(" ", "", new_df$Publication.year)
typeof(new_df$Publication.year)
typeof(new_df$Location)
head(new_df$Publication.year)
new_df$Publication.date <- gsub(" ", "", new_df$Publication.date)

new_df <- new_df %>%
  mutate(year=substr(new_df$Publication.date, nchar(new_df$Publication.date) - 4 + 1, nchar(new_df$Publication.date))) #n_last=4

#check missing value
which(is.na(new_df$Publication.year)) 
#[1] 4556 4586 4638 4729 5052 7175
new_df[4556, 12] # check the value of Publication.date (#12) for that specific row
new_df[4586, 12]
sum(is.na(new_df$year))

new_df <- new_df %>%
  mutate(Publication.year = coalesce(Publication.year,year))

#check
new_df[4586, 12]
new_df[4586, 11]
new_df[5052, 12]
new_df[5052, 11]


# publication date with format like "Oct21,2019"
typeof(new_df$Publication.date)
new_df <- transform(new_df, Publication.date = as.Date(Publication.date, format="%b%d,%Y"))
which(is.na(new_df$Publication.date)) 
# [1] 1580 2368 4410 4476 4477 5082 6188 8338
#check
new_df[4586, 12]
new_df[5052, 12]

#check number of news for each news media
#number <-new_df %>% 
#  group_by(Publication.date, newspaper) %>% 
#  summarise(number = n())
#print(number)
                    
number <-new_df %>% 
  group_by(Publication.year, newspaper) %>% 
  summarise(number = n()) 

p3<- ggplot(number) + 
  aes(x = Publication.year, y = number, color = newspaper) + 
  geom_line() 

p3<-p3 + ggtitle("Yearly trends of news articles on Asian Americans") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Year") + ylab("Number")

ggsave("/Users/lin/Documents/projects/USnews/trackpaper/yeartrends.eps", limitsize = FALSE)

# by year & publisher
number1 <-new_df %>% 
  group_by(Publication.year, newspaper) %>% 
  summarise(number = n())

number1 %>% 
  subset(Publication.year!="1984" & Publication.year!="1983" & Publication.year!="1982" & Publication.year!="1981" & Publication.year!="1980"  )%>% 
  ggplot(aes(x=Publication.year,y=number,group=newspaper,color=newspaper)) +
  geom_point(size=1)+
  geom_line(size=.5)+
  scale_x_continuous(breaks = c(2017,2018,2019,2020,2021,2022))+
  labs(y="# of news articles\n",x="\nYear",color="",
       title = "Number of news among each news media",
       caption = "Data Source: ProQuest")

ggsave("/gpfs/home/haolin1/FUN/Number_of_news.eps",dpi = 300,width = 100,height = 79,units = "mm")

number2 <-new_df %>% 
  group_by(newspaper) %>% 
  summarise(number = n())

number3 <-new_df %>% 
  group_by(newspaper,Publication.year) %>% 
  range(Publication.year)

# find how many events / news have happend
# let's assume big events/news twice a year
peak <- findpeaks(number, npeaks=10, threshold=5, sortstr=TRUE, minpeakheight=5)

# check if the full text is complete
new_df <- new_df %>%
  mutate(nword=ntoken(Full.text))
range(new_df$nword, na.rm=TRUE) # 3 9786
table(new_df$nword)

lownews <- new_df2%>%
  filter(nword<50)
lownews[100, ]
table(lownews$nword)
lownews[lownews$nword == 27,]
lownews[lownews$nword == 32,]
lownews[lownews$nword == 41,]
lownews[lownews$nword == 44,]
lownews[lownews$nword == 47,]
new_df[109, "columnName"] <- value

# now every word of full text is included in the file
# for those only with 3 words, they are old newspapers, only PdF versions are available
# it is "Not available."

new_df %>% new_df
  mutate(alltext = case_when(Full.text == "Not avaiable" ~ 0, #condition 1
                         Mother == 0 & Father == 1 ~ 1, #condition 2
                         is.na(Mother) & Father == 1 ~ NA_real_, #condition 3
                         TRUE ~ 99))

new_df[, "ProQuest.document.ID"==421031273] 
new_df[new_df$ProQuest.document.ID == 421031273, ]
# 2932

# check if abstract is available
new_df2 <- new_df2 %>%
  mutate(nabstract=ntoken(Abstract))
sum(is.na(new_df2$Abstract))
new_df2[new_df2$Abstract == "N\A",]

# now I think I've found the reason why some news articles are not full: the 
# blank line in the full text

colnames(new_df)
head(new_df)

# prepare corpus
mycorpus <-corpus(new_df$Full.text)
docvars(mycorpus, "publisher") <- new_df$newspaper
docvars(mycorpus, "id") <- new_df$X1
docvars(mycorpus, "year") <- new_df$Publication_year

#sdata <- read.table(file = "/gpfs/home/haolin1/FUN/stpwords_en.txt", header = TRUE)
head(sdata)
tail(sdata)


stopwords <- as.character(sdata$word)
stopwords <- c(stopwords, stopwords())

dfm <- dfm(mycorpus,
           ignoredFeatures = c(stopwords("english"), stpwords),
           ngrams=c(1,2))


vdfm <- dfm_trim(dfm, 
                 min_termfreq = 2, min_docfreq = 1,
                 termfreq_type = c("count"),
                 docfreq_type = c("count"))

#
library(RColorBrewer)
par(mfrow = c(1,2),mar = c(0, 2, 0, 0 ))
plot(vdfm,  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
     random.order = F, rot.per=0.1, max.words=250, main = "Raw Counts",
     pdf(file = "/Users/lin/Desktop/USnews/news2022_rawcounts.pdf"))
plot(tfidf(vdfm),  scale=c(3.5, .75), colors=brewer.pal(8, "Dark2"), 
     random.order = F, rot.per=0.1, max.words=250, main = "TF-IDF")


# structural topic model
processed <- textProcessor(new_df$Full.text,
                           metadata=new_df,
                           customstopwords=stopwords,
                           lowercase=TRUE,
                           removestopwords=TRUE,
                           removenumbers=TRUE,
                           removepunctuation=TRUE,
                           stem=FALSE)

remove<-plotRemoved(processed$documents, lower.thresh=seq(1,200, by=20))
print(remove)


out <- prepDocuments(processed$documents,
                     processed$vocab,
                     processed$meta,
                     lower.thresh = 3,upper.thresh=100000)
#Removing 54276 of 79719 terms (75477 of 1106182 tokens) due to frequency 
#Your corpus now has 4178 documents, 25443 terms and 1030705 tokens.> 

#lower.thresh = 5
#Removing 60351 of 79719 terms (102271 of 1106182 tokens) due to frequency 
#Your corpus now has 4178 documents, 19368 terms and 1003911 tokens.> 

# Determining Optimal Number of Topics
K<-c(10,20,30,40,50,75,100)

#kresult <- searchK(out$documents, out$vocab, K, prevalence=~publisher + s(year), data = out$meta, init.type = "Spectral")
#kresult <- searchK(out$documents, out$vocab, K, prevalence=~publisher, data = out$meta, init.type = "Spectral")
kresult <- searchK(out$documents, out$vocab, K, prevalence=~factor(newspaper) + s(Publication.year), data = out$meta, init.type = "Spectral")

#number of observations in content covariate (4178) prevalence covariate (4177) and documents (4178) are not all equal.
which(is.na(new_df$Publication.year)) # 395
new_df[395, "Publication.year"] <- 2001


print(kresult$results)
options(repr.plot.width=6, repr.plot.height=6)
plot(kresult)

# print(kresult$results)
#K   exclus    semcoh   heldout residual     bound    lbound em.its
#1  10 9.328977 -54.00883 -8.418759 4.053093 -11865244 -11865228     49
#2  20 9.635426 -60.59994 -8.334804 3.291008 -11629065 -11629023    124
#3  30 9.692846 -67.15289 -8.319334 2.886235 -11482342 -11482267    137
#4  40 9.689255 -75.96731 -8.337110 2.610601 -11379989 -11379879     73
#5  50 9.695089 -75.89483 -8.344393 2.410525 -11286938 -11286789     71
#6  75 9.737946 -81.77044 -8.382225 2.115685 -11105481 -11105229     57
#7 100 9.756112 -85.05926 -8.450764 1.949534 -10950407 -10950044     52



# stm
# I tried 30 and 50
k <- 50

stmFit <- stm(out$documents, 
              out$vocab, K = k, 
              prevalence=~factor(newspaper) + s(Publication.year),
              max.em.its = 200, 
              data = out$meta, 
              init.type = "Spectral", seed = 300)

# save the model
save(stmFit, file = "/gpfs/home/haolin1/FUN/stmFit_full_50.RData")
save(out, file = "/gpfs/home/haolin1/FUN/out_full_50.RData")

# summary the topics and their keywords
pdf("/gpfs/home/haolin1/FUN/topic40years_sum50topic.pdf")
plot(stmFit, type = "summary", xlim = c(0,.20), n = 10, #labeltype = "frex",
     main = "Newspaper's 50 Topics Over the Last 40 Years", text.cex = 0.6)
dev.off() 


#dev.new(width = 550, height = 1330, unit = "px")
#plot(stmFit, type = "labels", n = 10, 
#     labeltype = c("prob", "frex", "lift", "score"),
#     main = "Keywords of Topics Over the Last Five Years",
#     text.cex = 0.4)
#dev.off() 
labelTopics <- labelTopics(stmFit, n = 10)
capture.output(labelTopics, file = "/gpfs/home/haolin1/FUN/topic40_words.txt")

# assign names to topics
topic <- data.frame(
  topicnames = c("Broadway Musicals",
                 "Art & Race",
                 "Family",
                 "Immigration Policy",
                 "the Death of George Floyd",
                 "2022WinterOlympic",
                 "Nomadland",
                 "Not Sure-Talk",
                 "President Inauguration",
                 "Census Population",
                 "Asian Food",
                 "Sports",
                 "Gender Issue",
                 "Art",
                 "Shang-chi related",
                 "Literary",
                 "Trump-Biden",
                 "Affirmative Action",
                 "Covid-19",
                 "Education",
                 "Trump-China",
                 "California Election",
                 "Not Sure-Kpop race",
                 "Chinatown",
                 "Law",
                 "President Election",
                 "Online Media",
                 "Not Sure-Yoga",
                 "College Admission",
                 "Race",              
                 "Filmmaking related",
                 "Biden Governemnt",
                 "Asian Immigrants",
                 "illinois related",
                 "Atlanta Shooting",
                 "Andrew Yang",
                 "Diversity in Companies",
                 "Economy",
                 "Hate Crime",
                 "Fashion Designer"),
  TopicNumber = 1:k,
  TopicProportions = colMeans(stmFit$theta))

rank = order(unlist(Result1$means))
topic <- topic[rank,]

jpeg("/gpfs/home/haolin1/FUN/wordsforeachtopic.jpg")
par(mfrow = c(1,1),mar = c(2, 2, 2, 2))
for (i in 1:k){
  plot(stmFit, type = "labels", n = 15, 
       topics = i, main = paste0(topic$topicnames[i]," - Raw Probabilities"),
       width = 55)
  plot(stmFit, type = "labels", n = 15, 
       topics = i, main = paste0(topic$topicnames[i]," - FREX"), 
       labeltype = "frex", width = 55)
}
dev.off()


#We can also plot the exclusivity and semantic coherence, which are two metrics 
#that measure the “interpretability” of each topic. Higher semantic coherence 
#indicate topics that have more consistent words (more interpretable) while 
#exclusivity measures how exclusive the words are to the topic relative to other 
#topics (e.g. low values mean topics that are vague and share a lot of words with 
#other topics while high values indicate words that are very unique/exclusive 
#to the topic).
jpeg("/gpfs/home/haolin1/FUN/Interpretability.jpg")
par(mfrow = c(1,1),mar = c(2, 2, 2, 2))
topicQuality(stmFit,
             documents = out$documents, 
             main = "Topic Interpretability: Exclusivity and Semantic Coherence")
dev.off()

# Estimating the Effect of Covariates - Publishers

prep <- estimateEffect(1:k ~ factor(newspaper) + s(Publication.year), stmFit, meta = out$meta, uncertainty = "Global")

# order based on Expected Topic Proportion
pdf("/gpfs/home/haolin1/FUN/topic_difference_nyt_ct.pdf")
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))
rank = order(unlist(Result$means))
topic <- topic[rank,]
Result1 <- plot(prep, "newspaper", method = "difference", 
               cov.value1 = "new_york_times", cov.value2 = "Chicago_Tribune", 
               topics = topic$TopicNumber,
               verbose.labels = F, 
               ylab = "Expected Difference in Topic Probability among NYT and CT \n (with 95% Confidence Intervals)", 
               labeltype = "custom",
               xlab = "More Likely Chicago_Tribune                         Not Significant                          More Likely new_york_times",
               custom.labels  = topic$topicnames, 
               main = "Effect of Publishers (NYT and CT) on Topic Prevelance for Newspaper Coverage",
               xlim = c(-0.08,0.08))
dev.off()


pdf("/gpfs/home/haolin1/FUN/topic_difference_nyt_wsj.pdf")
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))
plot(prep, "newspaper", method = "difference", 
                cov.value1 = "new_york_times", cov.value2 = "wall_street_journal", 
                topics = topic$TopicNumber,
                verbose.labels = F, 
                ylab = "Expected Difference in Topic Probability among NYT and WSJ \n (with 95% Confidence Intervals)", 
                labeltype = "custom",
                xlab = "More Likely wall_street_journal                         Not Significant                          More Likely new_york_times",
                custom.labels  = topic$topicnames, 
                main = "Effect of Publishers (nyt & wsj) on Topic Prevelance for Newspaper Coverage",
                xlim = c(-0.08,0.08))
dev.off()



pdf("/gpfs/home/haolin1/FUN/topic_difference_ct_wsj.pdf")
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))
plot(prep, "newspaper", method = "difference", 
     cov.value1 = "Chicago_Tribune", cov.value2 = "wall_street_journal", 
     topics = topic$TopicNumber,
     verbose.labels = F,  
     ylab = "Expected Difference in Topic Probability among CT and WSJ \n (with 95% Confidence Intervals)", 
     labeltype = "custom",
     xlab = "More Likely wall_street_journal                         Not Significant                          More Likely Chicago_Tribune",
     custom.labels  = topic$topicnames, 
     main = "Effect of Publishers (ct & wsj) on Topic Prevelance for Newspaper Coverage",
     xlim = c(-0.08,0.08))
dev.off()


pdf("/gpfs/home/haolin1/FUN/topic_difference_lat_wsj.pdf")
par(mfrow = c(1,1),mar = c(6, 6, 4, 4))
plot(prep, "newspaper", method = "difference", 
     cov.value1 = "los_angeles_times", cov.value2 = "wall_street_journal", 
     topics = topic$TopicNumber,
     verbose.labels = F,  
     ylab = "Expected Difference in Topic Probability among LAT and WSJ \n (with 95% Confidence Intervals)", 
     labeltype = "custom",
     xlab = "More Likely wall_street_journal                         Not Significant                          More Likely los_angeles_times",
     custom.labels  = topic$topicnames, 
     main = "Effect of Publishers (LAT & WSJ) on Topic Prevelance for Newspaper Coverage",
     xlim = c(-0.08,0.08))
dev.off()


# how the prevalence of topics change over time
pdf("/gpfs/home/haolin1/FUN/topic_time.pdf")
par(mfrow = c(2,2),mar = c(4,4,2,2))
for (i in 1:k){
  plot(prep, "Publication.year", method = "continuous", topics = rank[i], rank[i], 
       main = paste0(topic$topicnames[i],": Topic ",rank[i]),
       printlegend = FALSE, ylab = "Exp. Topic Prob", 
       xlab = "Time from 2017 to 2022", ylim = c(-0.02,0.2),
       moderator = "newspaper", moderator.value = "new_york_times",  linecol = "blue",
       ci.level = 0
  )
  plot(prep, "Publication.year", method = "continuous", topics = rank[i], rank[i], 
       main = paste0(topic$topicnames[i],": Topic ",rank[i]),
       printlegend = FALSE, ylab = "Exp. Topic Prob", 
       xlab = "Time from 2017 to 2022", ylim = c(-0.02,0.2),
       moderator = "newspaper", moderator.value = "wall_street_journal",  linecol = "red", add = "T", 
       ci.level = 0
  )
  plot(prep, "Publication.year", method = "continuous", topics = rank[i], rank[i], 
       main = paste0(topic$topicnames[i],": Topic ",rank[i]),
       printlegend = FALSE, ylab = "Exp. Topic Prob", 
       xlab = "Time from 2017 to 2022", ylim = c(-0.02,0.2),
       moderator = "newspaper", moderator.value = "washtington_post",  linecol = "grey", add = "T", 
       ci.level = 0
  )
  plot(prep, "Publication.year", method = "continuous", topics = rank[i], rank[i], 
       main = paste0(topic$topicnames[i],": Topic ",rank[i]),
       printlegend = FALSE, ylab = "Exp. Topic Prob", 
       xlab = "Time from 2017 to 2022", ylim = c(-0.02,0.2),
       moderator = "newspaper", moderator.value = "los_angeles_times",  linecol = "khaki", add = "T", 
       ci.level = 0
  )
  plot(prep, "Publication.year", method = "continuous", topics = rank[i], rank[i], 
       main = paste0(topic$topicnames[i],": Topic ",rank[i]),
       printlegend = FALSE, ylab = "Exp. Topic Prob", 
       xlab = "Time from 2017 to 2022", ylim = c(-0.02,0.2),
       moderator = "newspaper", moderator.value = "Chicago_Tribune",  linecol = "lightpink", add = "T", 
       ci.level = 0
  )
  legend(-1, 1.9, c("new_york_times", "wall_street_journal","washtington_post","los_angeles_times","Chicago_Tribune"), lwd = 2, col = c("blue", "red"))
}
dev.off()


# deal with the error "the doc size and the text size do not match"
load("out.RData")
load("stmFit.RData")
docs<-processed$documents
out <- prepDocuments(docs,
                     processed$vocab,
                     processed$meta,
                     lower.thresh = 5,upper.thresh=100000)
# matchtext <- new_df$Full.text[-out$docs.removed, ] 
matchtext <- new_df$Full.text[-c(36,  142,  200,  215,  216,  661,  756,  762, 1552, 2118, 2356, 2359, 2857, 3068, 3518,5684, 5917, 6066, 6132, 6207, 6396, 6944, 7339, 7743, 7779, 8017, 8160, 8175, 8206, 8207)]

# inspect the documents
pdf("/gpfs/home/haolin1/FUN/thought1.pdf")
par(mfrow = c(2,2),mar = c(4,4,2,2))
thought <- findThoughts(stmFit, texts=matchtext, topics=c(18), n=3)
print(thought)
plot(thought)
dev.off()

# There are so many noises in the dataset
# such as new_df[8122, ], which only mention a professor of Asian American Studies

# R environment
install.packages("renv")
library("renv")
renv::init()
renv::snapshot()

# docker
# rocker


