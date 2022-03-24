#Hierarchical Clustering 
library(dendextend)
library(ggdendro)
library(ggplot2)
library(patchwork)
library(reshape2)


TD_theme.legend = theme(
  panel.background = element_blank(),
  legend.key=element_blank(),
  legend.justification = "top",
  legend.text=element_text(size=50),
  legend.title=element_text(size=50),
  text = element_text(size = 36, family = "Arial"),
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


TD_theme.noy = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,2,1,0),"cm"),
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


custpal <- c("#32CD32","#006400")
################################################################

setwd('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')

liwc <- read.csv('ElasticNet/mean_features_td_resid_rouault.csv')

mean_sf <- read.csv('ElasticNet/Selection_Frequency_TD_resid_rouault.csv')
mean_sf <- cbind(LIWC = colnames(liwc)[1:86], mean_sf)

betas_sf <- read.csv('ElasticNet/Selection_Frequency_Betas_TD_resid_rouault.csv')
betas_sf <- cbind(LIWC = colnames(liwc)[1:86], betas_sf)

betas_sf[,2:4] <- ifelse(betas_sf[,2:4] > 0, "(+)","(-)")


betas_sf$LIWC <- tolower(betas_sf$LIWC)
betas_sf$LIWC[which(betas_sf$LIWC == "wc")] <- "word count"


colnames(mean_sf) <- c("LIWC","Anxious Depression","Compulsivity and Instrusive Thought","Social Withdrawl")


#schizotypy, depression, general anxiety, social anxiety, impulsivity, ocd, apathy, eating disorders, alcohol abuse
cbpalette <-c("#7CFC00","#32CD32","#006400")


############################
############################
colnames(mean_sf) <- c("LIWC","AnxiousDepression","CompulsivityandInstrusiveThought","SocialWithdrawl")

mean_sf$LIWC <- tolower(mean_sf$LIWC)
mean_sf$LIWC[which(mean_sf$LIWC == "wc")] <- "word count"

ad <- (mean_sf[order(mean_sf$AnxiousDepression,decreasing = T)[1:20],])
cit <- (mean_sf[order(mean_sf$CompulsivityandInstrusiveThought,decreasing = T)[1:20],])
sw <- (mean_sf[order(mean_sf$SocialWithdrawl,decreasing = T)[1:20],])

ad_betas <- merge(ad,betas_sf,by="LIWC")
cit_betas <- merge(cit,betas_sf,by="LIWC")
sw_betas <- merge(sw,betas_sf,by="LIWC")

#top_20 <- as.data.frame(table(c(as.character(ad$LIWC),as.character(cit$LIWC),as.character(sw$LIWC))))

top_20 <- as.data.frame(table(c(paste(ad_betas$LIWC,ad_betas$AD_resid),paste(cit_betas$LIWC,cit_betas$CIT_resid),
                                paste(sw_betas$LIWC,sw_betas$SW_resid))))
colnames(top_20) <- c("LIWC","Freq")

top_20$LIWC <- tolower(top_20$LIWC)
top_20$LIWC[which(top_20$LIWC == "wc")] <- "word count"

ad <- merge(ad,betas_sf[,c("LIWC","AD_resid")],by="LIWC")
ad$LIWC <- paste(ad$LIWC,ad$AD_resid)
ad <- ad[,-5]
ad <- merge(ad,top_20,by="LIWC")
ad$Freq <- (as.factor(ad$Freq))
ad$Freq <- factor(ad$Freq, levels=rev(levels(ad$Freq)))

cit <- merge(cit,betas_sf[,c("LIWC","CIT_resid")],by="LIWC")
cit$LIWC <- paste(cit$LIWC,cit$CIT_resid)
cit <- cit[,-5]
cit <- merge(cit,top_20,by="LIWC")
cit$Freq <- (as.factor(cit$Freq))
cit$Freq <- factor(cit$Freq, levels=rev(levels(cit$Freq)))


sw <- merge(sw,betas_sf[,c("LIWC","SW_resid")],by="LIWC")
sw$LIWC <- paste(sw$LIWC,sw$SW_resid)
sw <- sw[,-5]
sw <- merge(sw,top_20,by="LIWC")
sw$Freq <- (as.factor(sw$Freq))
sw$Freq <- factor(sw$Freq, levels=rev(levels(sw$Freq)))



ad.plt <- ggplot(data = ad, aes(x = reorder(LIWC, AnxiousDepression), y = Depression,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = AnxiousDepression,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Anxious Depression\nResiduals") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

cit.plt <- ggplot(data = cit, aes(x = reorder(LIWC, CompulsivityandInstrusiveThought),
                                  y = CompulsivityandInstrusiveThought,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = CompulsivityandInstrusiveThought,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Compulsivity and Instrusive Thought\nResiduals") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

colnames(sw)[5] <- "Frequency"
sw.plt <- ggplot(data = sw, aes(x = reorder(LIWC, SocialWithdrawl), y = SocialWithdrawl,fill = Frequency)) +
  geom_bar(stat="identity" ,aes(y = SocialWithdrawl,fill = Frequency),
           size=1.2, position = "dodge2",width=0.8) + ylab("Social Withdrawl\nResiduals") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.legend + ylim(0,1) +  scale_fill_manual(values=custpal)

sw.plt <- sw.plt + guides(fill = guide_legend(override.aes = list(size = 20),reverse=TRUE))

p2 <- (ad.plt + cit.plt + sw.plt)


##########################################################
#Non-residuals
##########################################################
custpal <- c("#aed581","#32CD32","#006400")
################################################################

setwd('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')


liwc <- read.csv('ElasticNet/mean_features_td_rouault.csv')

mean_sf <- read.csv('ElasticNet/Selection_Frequency_TD_rouault.csv')
mean_sf <- cbind(LIWC = colnames(liwc)[1:86], mean_sf)

betas_sf <- read.csv('ElasticNet/Selection_Frequency_Betas_TD_rouault.csv')
betas_sf <- cbind(LIWC = colnames(liwc)[1:86], betas_sf)


betas_sf[,2:4] <- ifelse(betas_sf[,2:4] > 0, "(+)","(-)")


betas_sf$LIWC <- tolower(betas_sf$LIWC)
betas_sf$LIWC[which(betas_sf$LIWC == "wc")] <- "word count"


colnames(mean_sf) <- c("LIWC","Anxious Depression","Compulsivity and Instrusive Thought","Social Withdrawal")


#schizotypy, depression, general anxiety, social anxiety, impulsivity, ocd, apathy, eating disorders, alcohol abuse
cbpalette <-c("#7CFC00","#32CD32","#006400")


############################
############################
colnames(mean_sf) <- c("LIWC","AnxiousDepression","CompulsivityandInstrusiveThought","SocialWithdrawl")

mean_sf$LIWC <- tolower(mean_sf$LIWC)
mean_sf$LIWC[which(mean_sf$LIWC == "wc")] <- "word count"

ad <- (mean_sf[order(mean_sf$AnxiousDepression,decreasing = T)[1:20],])
cit <- (mean_sf[order(mean_sf$CompulsivityandInstrusiveThought,decreasing = T)[1:20],])
sw <- (mean_sf[order(mean_sf$SocialWithdrawl,decreasing = T)[1:20],])

ad_betas <- merge(ad,betas_sf,by="LIWC")
cit_betas <- merge(cit,betas_sf,by="LIWC")
sw_betas <- merge(sw,betas_sf,by="LIWC")

#top_20 <- as.data.frame(table(c(as.character(ad$LIWC),as.character(cit$LIWC),as.character(sw$LIWC))))

top_20 <- as.data.frame(table(c(paste(ad_betas$LIWC,ad_betas$AD_score),paste(cit_betas$LIWC,cit_betas$CIT_score),
                                paste(sw_betas$LIWC,sw_betas$SW_score))))
colnames(top_20) <- c("LIWC","Freq")

top_20$LIWC <- tolower(top_20$LIWC)
top_20$LIWC[which(top_20$LIWC == "wc")] <- "word count"

ad <- merge(ad,betas_sf[,c("LIWC","AD_score")],by="LIWC")
ad$LIWC <- paste(ad$LIWC,ad$AD_score)
ad <- ad[,-5]
ad <- merge(ad,top_20,by="LIWC")
ad$Freq <- (as.factor(ad$Freq))
ad$Freq <- factor(ad$Freq, levels=rev(levels(ad$Freq)))

cit <- merge(cit,betas_sf[,c("LIWC","CIT_score")],by="LIWC")
cit$LIWC <- paste(cit$LIWC,cit$CIT_score)
cit <- cit[,-5]
cit <- merge(cit,top_20,by="LIWC")
cit$Freq <- (as.factor(cit$Freq))
cit$Freq <- factor(cit$Freq, levels=rev(levels(cit$Freq)))


sw <- merge(sw,betas_sf[,c("LIWC","SW_score")],by="LIWC")
sw$LIWC <- paste(sw$LIWC,sw$SW_score)
sw <- sw[,-5]
sw <- merge(sw,top_20,by="LIWC")
sw$Freq <- (as.factor(sw$Freq))
sw$Freq <- factor(sw$Freq, levels=rev(levels(sw$Freq)))



ad.plt <- ggplot(data = ad, aes(x = reorder(LIWC, AnxiousDepression), y = Depression,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = AnxiousDepression,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Anxious Depression") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

cit.plt <- ggplot(data = cit, aes(x = reorder(LIWC, CompulsivityandInstrusiveThought),
                                  y = CompulsivityandInstrusiveThought,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = CompulsivityandInstrusiveThought,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Compulsivity and Instrusive Thought") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

colnames(sw)[5] <- "Frequency"
sw.plt <- ggplot(data = sw, aes(x = reorder(LIWC, SocialWithdrawl), y = SocialWithdrawl,fill = Frequency)) +
  geom_bar(stat="identity" ,aes(y = SocialWithdrawl,fill = Frequency),
           size=1.2, position = "dodge2",width=0.8) + ylab("Social Withdrawal") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.legend + ylim(0,1) +  scale_fill_manual(values=custpal)

sw.plt <- sw.plt + guides(fill = guide_legend(override.aes = list(size = 20),reverse=TRUE))

p1 <- (ad.plt + cit.plt + sw.plt)


p1/p2

png("Figures/SF_TD_resid_nonresid_rouault.png", units="cm", width=95, height=50, res=600)

p1/p2

dev.off()
