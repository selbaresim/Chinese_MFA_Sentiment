#Before running this program, have to run your own dictionary and stopwords in your directory
#dictionary name should be neg.txt(neagtive  dictionary) and pos.txt(positive dictionary)
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
#library(jiebaR)
library(jiebaRD)

#Emotion 程式a
getEmotionalType <- function(x,pwords,nwords){
  emotionType <-numeric(0)  
  xLen <-length(x)  
  emotionType[1:xLen]<- 0  
  index <- 1  
  while(index <=xLen){  
    yLen <-length(x[[index]])  
    index2 <- 1  
    while(index2<= yLen){  
      if(length(pwords[pwords==x[[index]][index2]]) >= 1){  
        emotionType[index] <- emotionType[index] + 1  
      }else if(length(nwords[nwords==x[[index]][index2]]) >= 1){  
        emotionType[index] <- emotionType[index] - 1  
      }  
      index2<- index2 + 1  
    }  
    #获取进度  
    if(index%%100==0){  
      print(round(index/xLen,3))  
    }        
    index <-index +1  
  }  
  emotionType}  
#run dictionary, positive ane negative 
#Have to set up your own pos & neg dictionary
#Remember to make sure neg.txt and pos.txt are in same directory
#setwd("/Users/ming-minyang/Dropbox/1_PHD/1_Dissertation01/Data_dissertation/PPD/sentiment A/diplo_words/")
library(readtext)
#negative <- read.table("/Users/ming-minyang/Dropbox/1_PHD/1_Dissertation01/Data_dissertation/PPD/sentiment A/diplo_words/neg.txt")
#positive <- read.table("/Users/ming-minyang/Dropbox/1_PHD/1_Dissertation01/Data_dissertation/PPD/sentiment A/diplo_words/pos.txt")
negative_ntu <- read.table("/Users/ming-minyang/Desktop/Github/D/ntusd-negative.txt")
positive_ntu <- read.table("/Users/ming-minyang/Desktop/Github/D/ntusd-positive.txt")

