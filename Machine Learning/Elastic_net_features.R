#text features to input into elastic net models 

################################################################
# Twitter Transdiagnostic Study Analysis 

library(dplyr)
library(data.table)
library(Hmisc)
library(patchwork)
library(ggplot2)
library(corrplot)
library(extrafont)
loadfonts(device = "win")

setwd('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')

# standard threshold for removal should be 3 SDs from the mean on any variable
# function to remove outliers for a single row
remove_outliers <- function(x, na.rm = TRUE, ...) {
  s <- sd(x)
  m <- mean(x)
  y <- x
  y[x > (m + 3*s)] <- NA
  y[x < (m - 3*s)] <- NA
  y
}
# function to apply to all rows
remove_all_outliers <- function(d){
  d[] <- lapply(d, function(x) if (is.numeric(x))
    remove_outliers(x) else x)
  d
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#mean successive squared difference 
mssd <- function (x) 
{
  x <- na.omit(diff(x))
  as.vector((mean(x^2)))
}

#time adjusted mean successive squared difference 
time.adj.mssd <- function (x,y) 
{
  #text feature values
  x2 <- na.omit(diff(x))
  
  #time values 
  y2 <- na.omit(diff(y))
  
  sum(as.vector(x2^2)/y2)*(median(y2)/(length(x)-1))
}

################################################################

#Read in participant data with questionnaires
participants <- read.csv("Participant_Data/Participants_12.08.csv", stringsAsFactor=FALSE)
#remove participants who failed the attention check
participants <- participants %>% filter(OCI_6 == 1 | is.na(OCI_6))

#data from tweets, retweets, likes
text_features <- read.csv("text_features/all_tweets/text_features.csv", stringsAsFactors = FALSE)

#data from only tweets
#text_features <- read.csv("text_features/tweet/text_features.csv", stringsAsFactors = FALSE)

#data from only retweets
#text_features <- read.csv("text_features/retweet/text_features.csv", stringsAsFactors = FALSE)

#data from only likes
#text_features <- read.csv("text_features/like/text_features.csv", stringsAsFactors = FALSE)


text_features <- text_features %>% select(-c(Date,recruitment_type,arousal,dominance))
text_features$Day <- as.numeric(text_features$Day)
#filter out participants without at least 5 days of Tweets and at least
#50% of tweets in English 
participants <- participants %>% filter(Id %in% text_features$Id)

#filter out participants by percentile of data 
#participants <- participants %>% 
#  filter((participants$tweet + participants$retweet + participants$like) >=
#           quantile((participants$tweet + participants$retweet + participants$like),0.75) &
#           (participants$tweet + participants$retweet + participants$like) <
#           quantile((participants$tweet + participants$retweet + participants$like),1))

mean(participants$tweet + participants$retweet + participants$like)

#creating variables for 1st pp, 2nd pp and 3rd pp:
text_features$pro1 = (text_features$i + text_features$we)/2
text_features$pro2 = text_features$you
text_features$pro3 = (text_features$shehe + text_features$they)/2

text_features_mssd = text_features_time.mssd <- data.frame(matrix(vector(),1006 , 90,
                    dimnames=list(c(), colnames(text_features)[2:91])),stringsAsFactors=F)

for(i in 1:length(participants$Id)){
  
  print(i)
  id <- participants$Id[i]
  df1 <- text_features %>% filter(Id == id)
  df1 <- df1[complete.cases(df1), ]
  
  text_features_mssd[i,1] = text_features_time.mssd[i,1] <- id
  #mssd 
  text_features_mssd[i,2:90] <- apply(df1[,3:91],2,FUN = function(x) mssd(x))
  }


text_features <- text_features %>% select(-Day)

#average txt features over the past year 
text_features_mean <- aggregate(. ~ Id , data = text_features, FUN = "mean")
text_features_sd <- aggregate(. ~ Id , data = text_features, FUN = "sd")

#weights from Rouault 
weights <- read.csv("Participant_Data/weights_Rouault.csv", stringsAsFactor=FALSE)
################################################################

#not enough participants in genders not male or female, add together to create 3rd category
participants$Gender[which(participants$Gender > 2)] <- 3
participants$Gender <- as.factor(participants$Gender)
participants$Education <- as.factor(participants$Education)
participants$Employment <- as.factor(participants$Employment)

#total number of tweets 
participants$total_tweets <- (participants$tweet + participants$retweet + participants$like)
#differnece in percentage of tweets sent during the day - night 
participants$tweet_time <- (participants$day_tweets/participants$total_tweets) - (participants$night_tweets/participants$total_tweets)

#######################################
#reverse code scales

# depression
sds_flippers = c("SDS_2","SDS_5","SDS_6","SDS_11","SDS_12","SDS_14","SDS_16","SDS_17","SDS_18","SDS_20")
participants[,sds_flippers] = 5 - participants[ ,sds_flippers]


# schizotypy
smss_flippers = c("SSMS.IntAn_3", "SSMS.IntAn_4", "SSMS.IntAn_5", "SSMS.IntAn_7", "SSMS.IntAn_8", "SSMS.ImpNon_1", "SSMS.ImpNon_4", "SSMS.ImpNon_6")
participants[,smss_flippers] = 1 - participants[ ,smss_flippers]

# impulsivity
bis_flippers = c("BIS_9", "BIS_20", "BIS_30", "BIS_1","BIS_7","BIS_8","BIS_12","BIS_13","BIS_10","BIS_15","BIS_29")
participants[,bis_flippers] = 4 - participants[ ,bis_flippers]

# apathy
aes_flippers = c("AES_1","AES_2","AES_3","AES_4","AES_5","AES_7","AES_8","AES_9","AES_12","AES_13","AES_14","AES_15","AES_16","AES_17","AES_18")
participants[,aes_flippers] = 5 - participants[ ,aes_flippers]

# EAT
#rescale EAT from 0-5 to 0-3 after reverse coding 
eat_flippers = c("EAT_26")
participants[,eat_flippers] = 5 - participants[ ,eat_flippers]
participants[ , grepl( "EAT" , names( participants))] <- participants[ , grepl( "EAT" , names( participants))] - 2
participants[ , grepl( "EAT" , names( participants))] <- apply(participants[ , grepl( "EAT",names( participants))], MARGIN=2, function(x) {ifelse(x<=0, 0,x)})

# STAI
stai_flippers = c("STAI_T_1","STAI_T_3", "STAI_T_6", "STAI_T_7", "STAI_T_10", "STAI_T_13", "STAI_T_14", "STAI_T_16", "STAI_T_19")
participants[,stai_flippers] = 5 - participants[ ,stai_flippers]



# the OCI items need to be recoded. Item 6 is a catch question that we use in clickworker to assess attention. SO then we need to realign them to the correct responses.
setnames(participants, old=c("OCI_1","OCI_2","OCI_3","OCI_4","OCI_5","OCI_6","OCI_7","OCI_8","OCI_9","OCI_10",
                             "OCI_11","OCI_12",  "OCI_13","OCI_14","OCI_15","OCI_16","OCI_17","OCI_18","OCI_19"),
         new=c("OCI_1","OCI_2","OCI_3","OCI_4","OCI_5","trap_question","OCI_6","OCI_7","OCI_8","OCI_9","OCI_10",
               "OCI_11","OCI_12",  "OCI_13","OCI_14","OCI_15","OCI_16","OCI_17","OCI_18"))

# getting sum scores
participants$OCI_total = rowSums(sapply(participants[ , grepl( "OCI" , names( participants ) ) ], as.numeric))
participants$SDS_total = rowSums(sapply(participants[ , grepl( "SDS" , names( participants ) ) ], as.numeric))
participants$SSMS_total = rowSums(sapply(participants[ , grepl( "SSMS" , names( participants ) ) ], as.numeric))
participants$BIS_total = rowSums(sapply(participants[ , grepl( "BIS" , names( participants ) ) ], as.numeric))
participants$LSAS_total = rowSums(sapply(participants[ , grepl( "LSAS" , names( participants ) ) ], as.numeric))
participants$EAT_total = rowSums(sapply(participants[ , grepl( "EAT" , names( participants ) ) ], as.numeric))
participants$AUDIT_total = rowSums(sapply(participants[ , grepl( "AUDIT" , names( participants ) ) ], as.numeric))
participants$AES_total = rowSums(sapply(participants[ , grepl( "AES" , names( participants ) ) ], as.numeric))
participants$STAI_total = rowSums(sapply(participants[ , grepl( "STAI" , names( participants ) ) ], as.numeric))


############################################################################################################################################
# constructing trans-diagnostic dimensions
############################################################################################################################################
# must make sure that the order is consistent across the weights and the data frame from subjects
weights_order = weights$X

# note that the naming of individual items differs slightly; so we need to rename
setnames(participants, old=c('AUDIT_1', 'AUDIT_2', 'AUDIT_C_1', 'AUDIT_C_2', 'AUDIT_C_3', 'AUDIT_C_4', 'AUDIT_C_5', 'AUDIT_C_6', 'AUDIT_9_10_1', 'AUDIT_9_10_2'), new=c("AUDIT_1", "AUDIT_2","AUDIT_3","AUDIT_4", "AUDIT_5","AUDIT_6", "AUDIT_7", "AUDIT_8" ,"AUDIT_9","AUDIT_10"))

setnames(participants, old=c("STAI_T_1", "STAI_T_2","STAI_T_3","STAI_T_4","STAI_T_5","STAI_T_6","STAI_T_7","STAI_T_8","STAI_T_9","STAI_T_10","STAI_T_11","STAI_T_12","STAI_T_13","STAI_T_14","STAI_T_15","STAI_T_16","STAI_T_17","STAI_T_18","STAI_T_19","STAI_T_20"),  new=c("STAI_1", "STAI_2","STAI_3","STAI_4","STAI_5","STAI_6","STAI_7","STAI_8","STAI_9","STAI_10","STAI_11","STAI_12","STAI_13","STAI_14","STAI_15","STAI_16","STAI_17","STAI_18","STAI_19","STAI_20"))

setnames(participants, old=c("SSMS.UnEx_1","SSMS.UnEx_2","SSMS.UnEx_3","SSMS.UnEx_4","SSMS.UnEx_5","SSMS.UnEx_6","SSMS.UnEx_7","SSMS.UnEx_8",
                             "SSMS.UnEx_9","SSMS.UnEx_10","SSMS.UnEx_11","SSMS.UnEx_12","SSMS.CogDis_1","SSMS.CogDis_2","SSMS.CogDis_3","SSMS.CogDis_4",
                             "SSMS.CogDis_5","SSMS.CogDis_6","SSMS.CogDis_7","SSMS.CogDis_8","SSMS.CogDis_9","SSMS.CogDis_10","SSMS.CogDis_11","SSMS.IntAn_1",
                             "SSMS.IntAn_2","SSMS.IntAn_3","SSMS.IntAn_4","SSMS.IntAn_5","SSMS.IntAn_6","SSMS.IntAn_7","SSMS.IntAn_8","SSMS.IntAn_9","SSMS.IntAn_10",
                             "SSMS.ImpNon_1","SSMS.ImpNon_2","SSMS.ImpNon_3","SSMS.ImpNon_4","SSMS.ImpNon_5","SSMS.ImpNon_6","SSMS.ImpNon_7","SSMS.ImpNon_8","SSMS.ImpNon_9",
                             "SSMS.ImpNon_10"), new=c("SCZ_1"  ,  "SCZ_2"   , "SCZ_3"   , "SCZ_4"   , "SCZ_5"   , "SCZ_6" , "SCZ_7"  ,  "SCZ_8" ,   "SCZ_9" ,   "SCZ_10"  , 
                                                      "SCZ_11"  , "SCZ_12","SCZ_13" ,  "SCZ_14"  , "SCZ_15"  , "SCZ_16"  , "SCZ_17" ,  "SCZ_18","SCZ_19" ,  "SCZ_20"  , 
                                                      "SCZ_21"  , "SCZ_22"  , "SCZ_23"  , "SCZ_24", "SCZ_25" ,  "SCZ_26" ,  "SCZ_27" ,  "SCZ_28"  , "SCZ_29" ,  "SCZ_30",
                                                      "SCZ_31" ,  "SCZ_32" ,  "SCZ_33" ,  "SCZ_34"  , "SCZ_35" ,  "SCZ_36", "SCZ_37" ,  "SCZ_38" ,  "SCZ_39" ,  "SCZ_40" , 
                                                      "SCZ_41" ,  "SCZ_42", "SCZ_43"))

# get a dataframe that just has the columns from weights.csv file, and in that same order:
individ_items = participants[array(weights$X)]

# convert the raw individual items scores into their weighted version, depending on which of the 3 factors we select to weight them by: AD anxious depression < CIT Compulsive behaviour and intrusive thought < SW social withdrawal
AD = individ_items*t(weights[c("AD")])[col(individ_items)]
CIT = individ_items*t(weights[c("CIT")])[col(individ_items)]
SW = individ_items*t(weights[c("SW")])[col(individ_items)]

# now we just need to sum up these re-weighted items to get the factor score
AD$AD_score = rowSums(AD)
CIT$CIT_score = rowSums(CIT)
SW$SW_score = rowSums(SW)

#z-score trans-diagnostic factor scores 
AD$AD_score = as.numeric(scale(AD$AD_score))
CIT$CIT_score = as.numeric(scale(CIT$CIT_score))
SW$SW_score = as.numeric(scale(SW$SW_score))
############################################################################################################################################
participants = cbind(participants, AD[c("AD_score")],CIT[c("CIT_score")], SW[c("SW_score")])

#threshold <- text_features %>% group_by(Id) %>% summarise(WC_sum = sum(WC,na.rm = T))
#threshold <- threshold[order(threshold$WC_sum,decreasing = T),]

#threshold <- text_features %>% group_by(Id) %>% summarise(WC_sum = sum(WC,na.rm = T)) %>% filter(WC_sum >= 200)


df_mean <- text_features_mean %>% inner_join(participants, by=c("Id"))
df_sd <- text_features_sd %>% inner_join(participants, by=c("Id"))
df_mssd <- text_features_mssd %>% inner_join(participants, by=c("Id"))


#threshold <- threshold %>% filter(Id %in% df_mean$Id)
#threshold <- threshold[1:476,]

#df_mean <- df_mean %>% filter(Id %in% threshold$Id)


#LIWC text features, Age, total scores 
features_ml <- df_mean[,c(2:87,112,113,379:387)]
features_ml_td <- df_mean[,c(2:87,112,113,388:390)]

correlation_mat  <- cor(as.matrix(features_ml[,1:86]), as.matrix(features_ml[,1:86]), use="complete.obs")
corrplot(correlation_mat,is.corr = F,tl.cex = 2,cl.ratio = 0.25,tl.srt = 45,cl.align.text = "c",
         cl.cex = 2,method = "square", tl.pos='n')


SW_resid <- resid(glm(SW_score ~ AD_score + CIT_score, data = features_ml_td))
AD_resid <- resid(glm(AD_score ~ SW_score + CIT_score, data = features_ml_td))
CIT_resid <- resid(glm(CIT_score ~ AD_score + SW_score, data = features_ml_td))

features_ml_td_resid <- data.frame(features_ml_td[,1:88],AD_resid,CIT_resid,SW_resid)


write.csv(features_ml,file = 'ElasticNet/mean_features_top204.csv',row.names = FALSE)

write.csv(features_ml_td,file = 'ElasticNet/mean_features_td_rouault.csv',row.names = FALSE)
write.csv(features_ml_td_resid,file = 'ElasticNet/mean_features_td_resid_rouault.csv',row.names = FALSE)


#write.csv(features_sd_ml,file = 'ElasticNet/sd_features.csv',row.names = FALSE)
#write.csv(features_mssd_ml,file = 'ElasticNet/mssd_features.csv',row.names = FALSE)



