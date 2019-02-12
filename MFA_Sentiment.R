library(rJava)
library(xlsxjars)
library(xlsx)
library(dplyr)
library(tidyr)
library(qwraps2)
library(ggplot2)
library(reshape2)
library(zoo)
library(stargazer)
library(jiebaRD)
library(jiebaR)
#library(quanteda)

#### Section, setwd
setwd("/Users/ming-minyang/Dropbox/1_PHD/1_Dissertation01/Chapters/3_chapter_Korea/Mofa/")

#getSheets
WJBRoutinePress <- read.xlsx("MOFAQ&A2019_2018_2.xlsx", sheetName="Sheet1")

#rename using dyplr
WJBRoutinePress$hkey <- NULL
WJBRoutinePress$NA. <- NULL
WJBRoutinePress$title <- NULL
WJBRoutinePress$author <- NULL
#WJBRoutinePress$NA..1 <- NULL
WJBRoutinePress <- rename(WJBRoutinePress, date = speech_date, question = ask)

WJBRoutinePress$date <- gsub("-","",WJBRoutinePress$date)

#Take a look of the data
str(WJBRoutinePress)

##### Section 2, check for key words
#Search for keywor
toMatch1 <- c("菲方","菲律宾","南海仲裁") #
WJBRoutinePress$keep_Philippines <- ifelse(grepl(paste(toMatch1,collapse="|"), WJBRoutinePress$question), 1,0)
Philippines <- filter(WJBRoutinePress, keep_Philippines > 0)
Philippines$keep_Philippines <- NULL
str(Philippines)
Philippines$NA..1 <- NULL

#segemnt, stop words, just for first comment
engine1 = worker(type  = "mix", stop_word ="stopwords.txt", 
                 user = "/Users/ming-minyang/Dropbox/1_PHD/1_Dissertation01/Chapters/sentiment A/china_diplo_sent.txt")

#Find the dirtory of Lib: .Library
#Add new 分詞 like diplomatic words
#Here I use "China_diplo_sent.txt" as additional dictionary for 分詞
#Another way to do is: new_user_word(engine1,"宠物小精灵","分詞")
#Check https://qinwenfeng.com/doc/jiebaR_v0_8/dict.html for details 
words_total_1 <- data.frame(sentiment = 0,
                            date = 0,
                            stringsAsFactors = F)



##### Section 3, get emotion
# the function: "getEmotionalType" is in other R data
# Remember to run “emotion_program.R” first
# Remember: segment exists in both quanteda & JiebaR, so dont use both packages in same time
Philippines$answer <- toString(Philippines$answer)
str(Philippines)

for (i in 1:nrow(Philippines)){
  words <-toString(Philippines$answer[i])
  words <- segment(words, engine1)
  EmotionRank <-getEmotionalType(words,positive,negative)
  words_total <- data.frame(sentiment = sum(EmotionRank),
                            date = Philippines$date[i],
                            stringsAsFactors = F)
  words_total_1 <- rbind(words_total_1,words_total)}

#Bind table
str(Philippines)
Philippines2<- words_total_1
Philippines2 <- Philippines2[-1,]
Philippines3 <- bind_cols(Philippines, Philippines2)
Philippines3$date1 <- NULL
Philippines3$question <- NULL

Philippines3$sentiment2 <- ifelse(Philippines3$sentiment2>=1,1,
                                  ifelse(Philippines3$sentiment==0,0,-1))
write.table(Philippines3, file = "Philippines3.csv",row.names=FALSE, na="",col.names=FALSE, sep=",")
write.csv(Philippines3, file = "Philippines.csv",row.names=FALSE, na="")

#(Philippines3$sentiment >)
#JBRoutinePress$keep_Philippines <- ifelse(grepl(paste(toMatch1,collapse="|"), WJBRoutinePress$question), 1,0)

##### Sectioin 4, Plot 
#Clean data
#Average the sentiment scores in same date
words_total_1$date <- as.Date(words_total_1$date, format= "%Y%m%d")
words_total_2 <- words_total_1 %>% group_by(date) %>% summarise(avg = mean(sentiment))
#words_total_2 = words_total_2[-1,] #delete first row
words_total_2 <- rename(words_total_2,sentiment = avg)
##add year variable
words_total_2$year <- substr(words_total_2$date, 1, 4)


#Plot
P <- ggplot(words_total_2, aes(date, sentiment)) + 
  geom_bar(aes(fill = year), stat = "identity") +  #colour="black") +
  theme(axis.text.x = element_text(angle=70, vjust=0.5)) +
  #scale_color_brewer() : https://ggplot2.tidyverse.org/reference/scale_brewer.html
  #scale_fill_grey(start = .8, end = 0)  + 
  xlab("Date") +
  ylab("Sentiment against Philippines") 
P
P + scale_x_date() + 
  geom_vline(aes(xintercept = as.Date("2012-04-10")), linetype="dashed", color = "black") +
  geom_vline(aes(xintercept = as.Date("2016-07-12")), linetype="dashed", color = "black") +
  geom_text(aes(as.Date("2012-04-10"), label="Scarborough Shoal standoff\n", y=0), colour="black", angle=90, text=element_text(size=0.5)) +
  geom_text(aes(as.Date("2016-07-12"), label="The Hague court dismissed China's 9-dash claim\n", y=0), colour="black", angle=90, text=element_text(size=0.5))



