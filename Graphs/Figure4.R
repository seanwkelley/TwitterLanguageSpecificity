#-----------------------------------------------------------
#Kelley et al., 2022

#Figure 4

#-----------------------------------------------------------



library(dplyr)
library(reshape2)
library(ggplot2)
library(patchwork)

# standard threshold for removal should be 3 SDs from the mean on any variable
# function to remove outliers for a single row
remove_outliers <- function(x, na.rm = TRUE, ...) {
  s <- sd(x,na.rm = T)
  m <- mean(x,na.rm = T)
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


#Read in participant data with questionnaires
setwd('C:/Users/User/OneDrive - TCDUD.onmicrosoft.com/Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')

participants <- read.csv('participant_mean.csv',stringsAsFactors = FALSE)
participants <- participants[,c(1,379:387)]

tweets <- read.csv("Text_Features/all_tweets/VADER_ANEW_LIWC_complete.csv", stringsAsFactor=FALSE)
colnames(tweets)[4] <- "Id"

toMatch <- c("manange", "diagnos*", "handle", "deal")

tweets <- tweets %>% dplyr::filter(Id %in% participants$Id)
tweets$depression_reference <- grepl("depress*",tweets$Tweets)
tweets$suicide_reference <- grepl("suicide",tweets$Tweets)


depression_reference <- (tweets %>% group_by(Id) %>% 
    summarise(depression_reference = sum(depression_reference)))
              
t1 <- merge(participants,depression_reference,by = 'Id')  
t1$SDS_total <- ifelse(t1$SDS_total >= 50, 1, 0)
t1$depression_reference <- ifelse(t1$depression_reference >= 1, 1, 0) 

            
tweets <- tweets[tweets$depression_reference == FALSE,]
text_features_mean <- aggregate(. ~ Id , data = tweets[,c(4,9:94)], FUN = "mean")

text_features_mean <- merge(text_features_mean,participants, by = "Id")
tweet_features_mean <- merge(text_features_mean,depression_reference,by = 'Id')

tweet_features_mean$depression_reference <- ifelse(tweet_features_mean$depression_reference >= 1,1,0)
tweet_features_mean <- tweet_features_mean %>% dplyr::select(-Id)

write.csv(tweet_features_mean,file = 'mean_features_reviewer_exclude.csv',row.names = F)

#-----------------------------------------------------------------------------
review_theme = theme(
  panel.background = element_blank(),
  legend.position = "none",
  plot.margin=unit(c(1,1,1,1),"cm"),
  text = element_text(size = 48, family = "Arial"),
  axis.title.x = element_text(size = 48),
  axis.title.y = element_text(size = 48),
  axis.text = element_text(size = 48),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 48),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



hist_theme <- theme(text = element_text(size=36),
                    legend.position = "none",
                    axis.text.x = element_text(colour = "black"),
                    axis.text.y = element_text(colour = "black"),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    plot.margin=unit(c(1,1,1,1),"cm")) 

participants <- read.csv('participant_mean.csv',stringsAsFactors = FALSE)
df1 <- read.csv('ElasticNet/SDS_residuals.csv',stringsAsFactors = F)


participants <- participants[,c(1,368:377)]
df_resid <- merge(df1,participants, by = 'Id')

df_resid$residuals <- df_resid$SDS_total - df_resid$sds_test
df_resid[,100:109] <- scale(df_resid[,100:109])
df_resid[,2:110] <- remove_all_outliers(df_resid[2:110])


summary(glm(residuals ~ WC, data = df_resid))
summary(glm(residuals ~ total_tweets, data = df_resid))
summary(glm(residuals ~ volume, data = df_resid))
summary(glm(residuals ~ reply, data = df_resid))
summary(glm(residuals ~ followers, data = df_resid))
summary(glm(residuals ~ followees, data = df_resid))


g1 <- ggplot(df_resid,aes(x= WC,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Word Count") + ylab("Depression Residuals") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme 


g2 <- ggplot(df_resid,aes(x= total_tweets,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Number of Tweets") + ylab("") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme


g3 <- ggplot(df_resid,aes(x= volume,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Tweet Volume") + ylab("") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme


g4 <- ggplot(df_resid,aes(x= reply,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Number of Replies") + ylab("Depression Residuals") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme 


g5 <- ggplot(df_resid,aes(x= followers,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Followers") + ylab("") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme + scale_x_continuous(breaks=seq(0,2,1))


g6 <- ggplot(df_resid,aes(x= followees,y = residuals)) + geom_point(alpha = 0.8,color = "#46ACC8",size = 12) + 
  xlab("Followees") + ylab("") + geom_smooth(method = "lm",size = 3,se=FALSE,color = "black") + 
  review_theme 

df_resid %>% ggplot(aes(x=residuals)) +
  geom_histogram(color="black", alpha=0.8, position = 'identity',bins = 10) +
  labs(fill="") + xlab("Depression Residuals") +  
  hist_theme 


png('Figures/residual_regression.png',width = 60,height = 40, units = 'cm', res = 600)

g1 + g2 + g3 + g4 + g5 + g6 + plot_layout(ncol = 3)

dev.off()


png('Figures/residual_histogram.png',width = 20,height = 20, units = 'cm', res = 600)

df_resid %>% ggplot(aes(x=residuals)) +
  geom_histogram(color="black", alpha=0.8, position = 'identity',bins = 10) +
  labs(fill="") + xlab("Depression Residuals") +  
  hist_theme 


dev.off()



#--------------------------------------------------------------------------------


TD_theme.nolegend = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,1,0,0),"cm"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 36),
  axis.text.y = element_text(colour = "black",size = 46),
  plot.title = element_text(lineheight=.8, face="bold", size = 36),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


TD_theme.nolegend2 = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,1,0,0),"cm"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 36),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 36),
  axis.text.y = element_text(colour = "black",size = 36),
  plot.title = element_text(lineheight=.8, face="bold", size = 36),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


TD_theme.noy = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,1,0,0),"cm"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_blank(),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 36),
  axis.text.y = element_text(colour = "black",size = 36),
  plot.title = element_text(lineheight=.8, face="bold", size = 36),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



roc_dep <- read.csv("ElasticNet/roc_dep.csv", stringsAsFactor=FALSE)
colnames(roc_dep) <- c('spec','sens','threshold')

roc_dep <- (aggregate( .~ threshold, data = roc_dep, mean))

m1 <- lm(sens ~ splines::bs(spec, 3), data = roc_dep)

roc_dep_fit <- (cbind(roc_dep$spec,predict(m1)))
roc_dep_fit <- roc_dep_fit[order(roc_dep_fit[,1]),]
roc_dep_fit <- as.data.frame(roc_dep_fit[!duplicated(roc_dep_fit[,1]),])
colnames(roc_dep_fit) <- c('spec','sens')

ggplot(data = roc_dep_fit, aes(x = spec, y = sens)) + geom_line(size = 2.25) + 
  geom_abline(intercept = 0,size = 1,linetype = 'dashed') +
  TD_theme.nolegend + xlab('1 - Specificity') + ylab('Sensitivity')

#-----
roc_word <- read.csv("ElasticNet/roc_word.csv", stringsAsFactor=FALSE)
colnames(roc_word) <- c('spec','sens','threshold')

roc_word <- (aggregate( .~ threshold, data = roc_word, mean))

m1 <- lm(sens ~ splines::bs(spec, 7), data = roc_word)

roc_word_fit <- (cbind(roc_word$spec,predict(m1)))
roc_word_fit <- roc_word_fit[order(roc_word_fit[,1]),]
roc_word_fit <- as.data.frame(roc_word_fit[!duplicated(roc_word_fit[,1]),])
colnames(roc_word_fit) <- c('spec','sens')


roc_dep_fit <- melt(roc_dep_fit[,c("spec", "sens")], id="spec")
roc_word_fit <- melt(roc_word_fit[,c("spec", "sens")], id="spec")

roc_dep_fit[,'type'] <- 'self_report'
roc_word_fit[,'type'] <- 'posts'

data <- rbind(roc_dep_fit,roc_word_fit)

png('Figures/roc_plot.png',width = 20,height = 20, units = 'cm', res = 600)

ggplot(data = data, aes(x = spec, y = value,color = type )) + geom_line(size = 2.25) + 
  geom_abline(intercept = 0,size = 1,linetype = 'dashed') + scale_color_manual(values = c('#E8384F','black')) + 
  TD_theme.nolegend2 + xlab('1 - Specificity') + ylab('Sensitivity')

dev.off()
#----------------------------------------------------------------

ft_depression <- read.csv("ElasticNet/featureimportance_depression_rf.csv", stringsAsFactor=FALSE)
ft_word <- read.csv("ElasticNet/featureimportance_word_rf.csv", stringsAsFactor=FALSE)

ft_depression <- as.data.frame(apply(ft_depression,MARGIN = 2, FUN = mean))
ft_word <- as.data.frame(apply(ft_word,MARGIN = 2, FUN = mean))

colnames(ft_depression) = colnames(ft_word)  = 'value'



ft_depression$feature <- row.names(ft_depression)
ft_depression$feature[which(ft_depression$feature == "WC")] <- "word count"
ft_depression$feature[which(ft_depression$feature == "function.")] <- "function"
ft_depression$feature <- tolower(ft_depression$feature)

ft_depression <- ft_depression[order(ft_depression$value,decreasing = T),]
ft_depression <- ft_depression[1:10,]
ft_depression$value <- ft_depression$value/max(ft_depression$value)


ft_word$feature <- row.names(ft_word)
ft_word$feature[which(ft_word$feature == "WC")] <- "word count"
ft_word$feature[which(ft_word$feature == "function.")] <- "function"
ft_word$feature <- tolower(ft_word$feature)

ft_word <- ft_word[order(ft_word$value,decreasing = T),]
ft_word <- ft_word[1:10,]
ft_word$value <- ft_word$value/max(ft_word$value)


p1 <- ggplot(data = ft_depression, aes(x = reorder(feature, value), y = value)) +
  geom_bar(stat="identity" ,aes(y = value),fill = 'black',
           size=1.2, position = "dodge2",width=0.8) + ylab("Feature Importance") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.nolegend


p2 <- ggplot(data = ft_word, aes(x = reorder(feature, value), y = value)) +
  geom_bar(stat="identity" ,aes(y = value),fill = '#E8384F',
           size=1.2, position = "dodge2",width=0.8) + ylab("Feature Importance") +    
  coord_flip() + xlab("") + TD_theme.nolegend




png('Figures/featureimportance_plot.png',width = 50,height = 20, units = 'cm', res = 600)

p1 + p2 

dev.off()



