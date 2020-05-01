##copy and paste to R script to use
##_____________________________________________________________________________
install.packages("dplyr", "dplyr", "tidyr", "tidytext", "readxl", "topicmodels", "ggplot2", "stringr", "textdata", "reshape2") ##install all required packages

##load required libraries
library(dplyr)  ##dataset processing
library(tidytext)  ##Data cleaning
library(stringr)  ##text clearning
library(readxl)  ##reading xlsx
library(tidyr) ##Data cleaning
library(ggplot2)  ##Plot
library(topicmodels) ##LDA
library(textdata) ##install textdata for dictionaries
# library(reshape2)

##_____________________________________________________________________________
##read data
# setwd("D:/Work/Teaching/Guest lecture/Text analysis workshop") ##replace the working dirctory with the place you place data file

data<-read_excel("Facebook_Posts_EricFang.xlsx") ##replace Facebook_Posts.xlsx with the data file name

##_____________________________________________________________________________
##process data
data=mutate(data,id=row_number()) ## append id as row_number to the dataframe
message=data$message ##keep only message content
message<-tolower(message)  ##upper cases to lower cases
message<-str_remove_all(message,regex("http[s]?://[a-zA-Z0-9.?/&=:#-]*")) ##remove url
message=tibble(message) ##convert text into tibble format
message=mutate(message,id=data$id) ##add id number for identification

subdata=data[ , c(1,2,15:24)]

##_____________________________________________________________________________
##tokenize text
tidy_mess <- message %>%
  unnest_tokens(word, message) %>% 
  anti_join(stop_words) ##remove stop_words

##_____________________________________________________________________________
##attitude analysis
#using afinn lexicon
afinnscore <- tidy_mess %>%
  inner_join(get_sentiments("afinn")) %>% ##using dictionary "afinn" by score
  group_by(id) %>%
  summarize(afinnscore=sum(value))%>%
  ungroup()

bingscore <- tidy_mess %>%
  inner_join(get_sentiments("bing")) %>% ##using dictionary "bing" by positive/negative
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(bingscore = positive - negative) ##sentiment score=positive-negative %>%

nrcscore=tidy_mess %>% 
  inner_join(get_sentiments("nrc")) %>% ##using dictionary "nrc" by category
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0)

loughranscore=tidy_mess %>% 
  inner_join(get_sentiments("loughran")) %>% ##using dictionary "loughran" by category
  count(id, sentiment) %>%
  spread(sentiment, n, fill = 0)

totaln=tidy_mess %>%
  group_by(id) %>%
  count(id)%>%
  ungroup()

# ggplot(afinnscore,aes(id,sentiment,fill=id))+geom_col(show.legend = F)#plot
# ggplot(bingscore,aes(id,bingscore,fill=id))+geom_col(show.legend = F)#plot sentiment
# ggplot(nrcscore,aes(id,anger,fill=id))+geom_col(show.legend = F)#plot anger
# data_post<-merge(data,loughranscore,by="id",all=T)
# ggplot(totaln,aes(id,n,fill=id))+geom_col(show.legend = F)  #plot number of words

## append score data to the subdata
data_post <- merge(subdata, message, by="id", all=T)
data_post <- merge(data_post, afinnscore, by="id", all=T)
data_post <- merge(data_post, bingscore[ , c('id', 'bingscore')], by="id", all=T)
data_post <- merge(data_post, nrcscore, by="id", all=T)
data_post <- merge(data_post, loughranscore, by="id", all=T)
data_post <- merge(data_post, totaln, by="id", all=T)

write.csv(data_post,file="complete_resutls.csv") ##write results into csv file, replace results.csv with the desired file name


##_____________________________________________________________________________
##topic modelling --- LDA
#creating data format

data_split = split(data_post, data_post$company_id)
top_terms_data = data.frame()
for (item in data_split) {
  company_message = item$message
  company_message = tibble(company_message)
  company_message = mutate(company_message, id=item$id)

  company_tidy_mess <- company_message %>%
    unnest_tokens(word, company_message) %>% 
    anti_join(stop_words)
  
  word_counts <- company_tidy_mess %>%
    anti_join(stop_words) %>%
    count(id, word) %>%
    ungroup()
  
  mess_dtm <- word_counts %>%
    cast_dtm(id, word, n)
  
  #model with 4 latent categories, k=4, replace 4 with any number you like
  mess_lda <- LDA(mess_dtm, k = 2, control = list(seed = 1234))

  #read the results
  mess_topics <- tidy(mess_lda, matrix = "gamma") %>%
    spread(topic,gamma, fill = 0)  ##adjust the format
  mess_topics$document<-as.numeric(mess_topics$document)
  mess_topics=mess_topics[order(as.numeric(mess_topics$document)),]## adjust the sequence
  names(mess_topics)[1]=c("id") ##rename 
  # lda_data = rbind(lda_data, mess_topics)
  
  top_terms <- tidy(mess_lda, matrix = "beta") %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms['company_urlname'] = item$company_urlname[1]
  top_terms['company_id'] = item$company_id[1]
  top_terms_data = rbind(top_terms_data, top_terms)
  
  ' # Draw topic graph
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()  ##plot the sequence
  '
}

write.csv(top_terms_data,file="lda_results.csv")
