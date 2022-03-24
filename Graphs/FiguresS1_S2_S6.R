#Supplementary Figures 1-3

################################################################

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

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
#ggplot themes
TD_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.title = element_blank(),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 36),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


TD_theme.noxaxis= theme(
  panel.background = element_blank(),
  text = element_text(size = 42, family = "Arial"),
  legend.position = "none",
  axis.title.x = element_text(size = 52),
  axis.title.y = element_text(size = 52),
  axis.text = element_text(size = 50),
  axis.text.x = element_blank(),
  axis.text.y = element_text(colour = "black",size = 52 ),
  plot.title = element_text(lineheight=.8, face="bold", size = 40),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


TD_theme.nolegend = theme(
  panel.background = element_blank(),
  text = element_text(size = 42, family = "Arial"),
  legend.position = "none",
  axis.title.x = element_text(size = 52),
  axis.title.y = element_text(size = 52),
  axis.text = element_text(size = 50),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 52),
  axis.text.y = element_text(colour = "black",size = 52 ),
  plot.title = element_text(lineheight=.8, face="bold", size = 40),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
################################################################

#Read in participant data with questionnaires
participants <- read.csv("Participant_Data/Participants_12.08.csv", stringsAsFactor=FALSE)
#remove participants who failed the attention check
participants <- participants %>% filter(OCI_6 == 1 | is.na(OCI_6))

#data from tweets, retweets, likes
text_features <- read.csv("text_features/all_tweets/text_features.csv", stringsAsFactors = FALSE)
text_features <- text_features %>% select(-c(Date,recruitment_type,arousal,dominance))
text_features$Day <- as.numeric(text_features$Day)
#filter out participants without at least 5 days of Tweets and at least
#50% of tweets in English 
participants <- participants %>% filter(Id %in% text_features$Id)

#creating variables for 1st pp, 2nd pp and 3rd pp:
text_features$pro1 = (text_features$i + text_features$we)/2
text_features$pro2 = text_features$you
text_features$pro3 = (text_features$shehe + text_features$they)/2

text_features_acf = text_features_mssd = text_features_time.mssd <- data.frame(matrix(vector(),1006 , 90,
                                                                                      dimnames=list(c(), colnames(text_features)[2:91])),stringsAsFactors=F)

for(i in 1:length(participants$Id)){
  
  print(i)
  id <- participants$Id[i]
  df1 <- text_features %>% filter(Id == id)
  df1 <- df1[complete.cases(df1), ]
  
  text_features_mssd[i,1] = text_features_time.mssd[i,1] <- id
  #mssd 
  text_features_mssd[i,2:90] <- apply(df1[,3:91],2,FUN = function(x) mssd(x))
  #time adjusted mssd
  text_features_time.mssd[i,2:90] <- apply(df1[,3:91],2,FUN = function(x,y) time.adj.mssd(x,df1$Day))
  #lag-1 autocorrelation 
  #text_features_acf[i, 2:90] <- t(apply(df1[,3:91],2,FUN = function(x) if(sd(x) > 0){acf(x,lag.max = 1)$acf[2]} else{NA}))
}


text_features <- text_features %>% select(-Day)

#average txt features over the past year 
text_features_mean <- aggregate(. ~ Id , data = text_features, FUN = "mean")
text_features_sd <- aggregate(. ~ Id , data = text_features, FUN = "sd")


#weights are from the eLife paper and will be used to construct transdiagnostic scores for each subject
weights <- read.csv("Participant_Data/weights.csv", stringsAsFactor=FALSE)

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
#Figure S1
#histograms of 9 scales

#schizotypy, depression, general anxiety, social anxiety, impulsivity, ocd, apathy, eating disorders, alcohol abuse
cbpalette <-c("#E2D200","black","#08306B","#46ACC8","#0B775E","#DD8D29","#046C9A","#8C510A","#C51B7D")

sds.plt <- ggplot(data = participants, aes(x = SDS_total))+ 
  geom_histogram(color= "black",bins=15, fill = "white") +
  xlab("Depression (SDS)") + TD_theme

ssms.plt <- ggplot(data = participants, aes(x = SSMS_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#E2D200") +
  xlab("Schziotypy (SSMS)") + TD_theme

ocd.plt <- ggplot(data = participants, aes(x = OCI_total))+
  geom_histogram(color= "black",bins=15, fill = "#DD8D29") +
  xlab("OCD (OCI-R)") + TD_theme

eat.plt <- ggplot(data = participants, aes(x = EAT_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#8C510A") +
  xlab("Eating Disorders (EAT-26)") + TD_theme

bis.plt <- ggplot(data = participants, aes(x = BIS_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#0B775E") +
  xlab("Impulsivity (BIS-11)") + TD_theme

audit.plt <- ggplot(data = participants, aes(x = AUDIT_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#C51B7D") +
  xlab("Alcoholism (AUDIT)") + TD_theme

aes.plt <- ggplot(data = participants, aes(x = AES_total))+
  geom_histogram(color= "black",bins=15, fill = "#046C9A") +
  xlab("Apathy (AES)") + TD_theme

lsas.plt <- ggplot(data = participants, aes(x = LSAS_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#46ACC8") +
  xlab("Social Anxiety (LSAS)") + TD_theme

stai.plt <- ggplot(data = participants, aes(x = STAI_total))+ 
  geom_histogram(color= "black",bins=15, fill = "#08306B") +
  xlab("Generalised Anxiety (STAI)") + TD_theme

scales.combined <- (sds.plt + ssms.plt + ocd.plt) / (eat.plt + bis.plt + audit.plt) / (aes.plt + lsas.plt + stai.plt)

scales.combined
#save combined scale histograms 
tiff("Figures/FigureS1.tiff", units="cm", width=80, height=30, res=600)
scales.combined
dev.off()

############################################################################################################################################
# constructing trans-diagnostic dimensions
############################################################################################################################################
# must make sure that the order is consistent across the weights and the data frame from subjects
weights_order = weights$X

# note that the naming of individual items differs slightly; so we need to rename
setnames(participants, old=c('AUDIT_1', 'AUDIT_2', 'AUDIT_C_1', 'AUDIT_C_2', 'AUDIT_C_3', 'AUDIT_C_4', 'AUDIT_C_5', 'AUDIT_C_6', 'AUDIT_9_10_1', 'AUDIT_9_10_2'), new=c("AUDIT_1", "AUDIT_2","AUDIT_3","AUDIT_4", "AUDIT_5","AUDIT_6", "AUDIT_7", "AUDIT_8" ,"AUDIT_9","AUDIT_10"))

setnames(participants, old=c("STAI_T_1", "STAI_T_2","STAI_T_3","STAI_T_4","STAI_T_5","STAI_T_6","STAI_T_7","STAI_T_8","STAI_T_9","STAI_T_10","STAI_T_11","STAI_T_12","STAI_T_13","STAI_T_14","STAI_T_15","STAI_T_16","STAI_T_17","STAI_T_18","STAI_T_19","STAI_T_20"),  new=c("STAI_1", "STAI_2","STAI_3","STAI_4","STAI_5","STAI_6","STAI_7","STAI_8","STAI_9","STAI_10","STAI_11","STAI_12","STAI_13","STAI_14","STAI_15","STAI_16","STAI_17","STAI_18","STAI_19","STAI_20"))

# Caoimhe can you please check this is correct. I am basing this on a big assumption, which is that the order of items in the datafile UnEx then CogDis etc is the same as in the original questionnaire. I imagine they are not, so need to check that the OLD names correspond accurately to the NEW names we want (which just go from 1-43 straight from the order in the original questionnaire)
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

df_mean <- text_features_mean %>% inner_join(participants, by=c("Id"))
df_sd <- text_features_sd %>% inner_join(participants, by=c("Id"))
df_mssd <- text_features_mssd %>% inner_join(participants, by=c("Id"))
df_time.mssd <- text_features_time.mssd %>% inner_join(participants, by=c("Id"))

features_ml <- df_mean[,c(2:87,112,379:387)]
features_ml[,1:86] <- remove_all_outliers(features_ml[1:86])


#z-score all LIWC text features, Age, followers, and followees and the 9 questionnaire sum totals 
df_mean[,c(2:90,112,368,369,370:387)] <- scale(df_mean[,c(2:90,112,368,369,370:387)])

# running the latter function and creating new dataframes with text_features/metadata that have outliers removed.
mean.norm = remove_all_outliers(df_mean[c(2:90,368:369,370:378)])

# link that back up with the self report data you want. add in the rest yourself
mean.norm2 = cbind(mean.norm, df_mean[c("Id", "Age", "Gender","AD_score", "CIT_score", "SW_score", "SSMS_total", "SDS_total", "OCI_total", "EAT_total","BIS_total",
                                        "LSAS_total","AUDIT_total","AES_total","STAI_total")])


#---------------------------------------------------------

#Figure S2
#Association between scales and age/gender


mod.sds <- (glm(SDS_total ~ Age + Gender,data = mean.norm2))
mod.ssms <- (glm(SSMS_total ~ Age + Gender,data = mean.norm2))
mod.oci <- (glm(OCI_total ~ Age + Gender,data = mean.norm2))
mod.eat <- (glm(EAT_total ~ Age + Gender,data = mean.norm2))
mod.bis <- (glm(BIS_total ~ Age + Gender,data = mean.norm2))
mod.audit <- (glm(AUDIT_total ~ Age + Gender,data = mean.norm2))
mod.aes <- (glm(AES_total ~ Age + Gender,data = mean.norm2))
mod.lsas <- (glm(LSAS_total ~ Age + Gender,data = mean.norm2))
mod.stai <- (glm(STAI_total ~ Age + Gender,data = mean.norm2))

betas_age <- c(coefficients(summary(mod.sds))[2],coefficients(summary(mod.ssms))[2],coefficients(summary(mod.oci))[2],
               coefficients(summary(mod.eat))[2],coefficients(summary(mod.bis))[2],coefficients(summary(mod.audit))[2],
               coefficients(summary(mod.aes))[2],coefficients(summary(mod.lsas))[2],coefficients(summary(mod.stai))[2])


betas_female <- c(coefficients(summary(mod.sds))[3],coefficients(summary(mod.ssms))[3],coefficients(summary(mod.oci))[3],
                  coefficients(summary(mod.eat))[3],coefficients(summary(mod.bis))[3],coefficients(summary(mod.audit))[3],
                  coefficients(summary(mod.aes))[3],coefficients(summary(mod.lsas))[3],coefficients(summary(mod.stai))[3])


lwr.ci.age <- c(confint(mod.sds,parm = "Age")[1],confint(mod.ssms,parm = "Age")[1],
                confint(mod.oci,parm = "Age")[1],confint(mod.eat,parm = "Age")[1],
                confint(mod.bis,parm = "Age")[1],confint(mod.audit,parm = "Age")[1],
                confint(mod.aes,parm = "Age")[1],confint(mod.lsas,parm = "Age")[1],
                confint(mod.stai,parm = "Age")[1])

lwr.ci.female <- c(confint(mod.sds,parm = "Gender2")[1],confint(mod.ssms,parm = "Gender2")[1],
                   confint(mod.oci,parm = "Gender2")[1],confint(mod.eat,parm = "Gender2")[1],
                   confint(mod.bis,parm = "Gender2")[1],confint(mod.audit,parm = "Gender2")[1],
                   confint(mod.aes,parm = "Gender2")[1],confint(mod.lsas,parm = "Gender2")[1],
                   confint(mod.stai,parm = "Gender2")[1])


upr.ci.age <- c(confint(mod.sds,parm = "Age")[2],confint(mod.ssms,parm = "Age")[2],
                confint(mod.oci,parm = "Age")[2],confint(mod.eat,parm = "Age")[2],
                confint(mod.bis,parm = "Age")[2],confint(mod.audit,parm = "Age")[2],
                confint(mod.aes,parm = "Age")[2],confint(mod.lsas,parm = "Age")[2],
                confint(mod.stai,parm = "Age")[2])

upr.ci.female <- c(confint(mod.sds,parm = "Gender2")[2],confint(mod.ssms,parm = "Gender2")[2],
                   confint(mod.oci,parm = "Gender2")[2],confint(mod.eat,parm = "Gender2")[2],
                   confint(mod.bis,parm = "Gender2")[2],confint(mod.audit,parm = "Gender2")[2],
                   confint(mod.aes,parm = "Gender2")[2],confint(mod.lsas,parm = "Gender2")[2],
                   confint(mod.stai,parm = "Gender2")[2])


zscore_ci = data.frame(matrix(vector(),9 , 7,
                              dimnames=list(c(), c("Beta_Age","Beta_Gender","lwr.ci.age","upr.ci.age",
                                                   "lwr.ci.gender","upr.ci.gender","Scale"))),
                       stringsAsFactors=F)


zscore_ci$Beta_Age <- betas_age;zscore_ci$Beta_Gender <- betas_female
zscore_ci$lwr.ci.age <- lwr.ci.age; zscore_ci$upr.ci.age <- upr.ci.age
zscore_ci$lwr.ci.gender <- lwr.ci.female; zscore_ci$upr.ci.gender <- upr.ci.female

zscore_ci$Scale <- c("Depression","Schizotypy","Obsessive\nCompulsive","Eating\nDisorders","Impulsivity",
                     "Alcohol\nAbuse","Apathy","Social\nAnxiety","Generalised\nAnxiety")

zscore_ci$Scale <- factor(zscore_ci$Scale, levels = c("Depression","Apathy","Alcohol\nAbuse",
                                                      "Impulsivity","Eating\nDisorders","Social\nAnxiety",
                                                      "Obsessive\nCompulsive","Schizotypy","Generalised\nAnxiety"))

cbpalette <- c("white","#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8","#DD8D29","#E2D200","#08306B")
cbpalette_outline <- c("black","#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8","#DD8D29","#E2D200","#08306B")


zscore_ci$Scale <- factor(zscore_ci$Scale, levels = zscore_ci$Scale[order(zscore_ci$Scale,decreasing = F)])

age.plot <- ggplot(data = zscore_ci,aes(x=Scale, y=Beta_Age,fill = Scale)) +
  TD_theme.noxaxis  + geom_bar(stat="identity", aes(color=Scale),size = 1.2,width=0.8) + xlab("") + scale_fill_manual(values=cbpalette) +
  scale_color_manual(values=cbpalette_outline) + 
  ylab(expression("Age Beta (95% CI)")) + geom_errorbar(aes(ymin=lwr.ci.age, ymax=upr.ci.age), width=.25,size=0.9) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size=1.5)


gender.plot <- ggplot(data = zscore_ci,aes(x=Scale, y=Beta_Gender,fill = Scale)) +
  TD_theme.nolegend  + geom_bar(stat="identity", aes(color=Scale),size = 1.2,width=0.8) + xlab("\n Psychiatric Symptoms") + scale_fill_manual(values=cbpalette) +
  scale_color_manual(values=cbpalette_outline) + 
  ylab(expression("Gender Beta (95% CI)")) + geom_errorbar(aes(ymin=lwr.ci.gender, ymax=upr.ci.gender), width=.25,size=0.9) +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "black", size=1.5)

age.plot / gender.plot +  plot_annotation(tag_levels = 'A')

tiff("Figures/FigureS2.tiff", units="cm", width=100, height=80, res=600)
age.plot / gender.plot +  plot_annotation(tag_levels = 'A')
dev.off()


####################################################################

#Figure S3
#Correlation between scales 

correlation.scales <- mean.norm2[,c(102,107:115)]
colnames(correlation.scales) <- c("Age","SSMS","SDS","OCI","EAT","BIS","LSAS","AUDIT","AES","STAI")
colnames(correlation.scales) <- c("Age","Schizotypy","Depression","Obsessive Compulsive","Eating Disorders",
                                  "Impulsivity","Social Anxiety","Alcohol Abuse","Apathy",
                                  "Generalised Anxiety")
correlation.scales2  <- cor(as.matrix(correlation.scales), as.matrix(correlation.scales), use="complete.obs")

corrplot(correlation.scales2,is.corr = F,tl.cex = 2,cl.ratio = 0.25,tl.srt = 45,cl.align.text = "c",
         cl.cex = 2,method = "square", type = "upper")


correlation.mat <- cor(as.matrix(mean.norm2[,107:115]), as.matrix(mean.norm2[ , 1:89]), use="complete.obs")

#correlation between 9 psychiatric scales and all LIWC text features 
tiff("Figures/FigureS3.tiff", units="cm", width=25, height=25, res=600)
corrplot(correlation.scales2,is.corr = F,tl.cex = 1.5,cl.ratio = 0.25,tl.srt = 45,cl.align.text = "c",
         cl.cex = 1.5,method = "square", type = "upper")
dev.off()
