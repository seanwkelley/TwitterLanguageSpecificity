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


custpal <- c("#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B")
################################################################

setwd('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')

mean_sf <- read.csv('ElasticNet/Selection_Frequency.csv')

betas_sf <- read.csv('ElasticNet/Selection_Frequency_Betas.csv')
betas_sf[,2:10] <- ifelse(betas_sf[,2:10] > 0, "(+)","(-)")
colnames(betas_sf)[1] <- "LIWC"

betas_sf$LIWC <- tolower(betas_sf$LIWC)
betas_sf$LIWC[which(betas_sf$LIWC == "wc")] <- "word count"


colnames(mean_sf) <- c("LIWC","Depression","Obsessive\nCompulsive", "Schizotypy","Impulsivity",
                       "Social\nAnxiety","Eating\nDisorders","Alcohol\nAbuse","Apathy","Generalised\nAnxiety")

mean_clusters <- hclust(dist(t(mean_sf[,2:10])))


#schizotypy, depression, general anxiety, social anxiety, impulsivity, ocd, apathy, eating disorders, alcohol abuse
cbpalette <-c("#E2D200","black","#08306B","#46ACC8","#0B775E","#DD8D29","#046C9A","#8C510A","#C51B7D")


#mean dendrogram 
dend_mean <- t(mean_sf[,2:10]) %>% # data
  dist %>% # calculate a distance matrix, 
  hclust(method = "ward.D2") %>% # Hierarchical clustering 
  as.dendrogram %>% set("labels_col", value = cbpalette, k=9) %>% 
  set("branches_k_color", 
      value = cbpalette, k = 9) %>%
  set("branches_lwd", 2) %>%
  set("labels_cex", 2) 

mean_gg <- as.ggdend(dend_mean)



#ggplots of hierarchical clustering

p1 <- ggplot(mean_gg,horiz = TRUE,theme = theme( axis.title.x = element_text(size = 36),
                                        axis.text.y = element_blank(),
                                       axis.title.y = element_blank(),
                                       panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(),
                                       panel.background = element_blank(), 
                                       axis.line = element_line(colour = "black"),
                                       axis.text.x = element_text(size = 36,colour = "black"))) +
                                       ylab("Height")


############################
############################
colnames(mean_sf) <- c("LIWC","Depression","ObsessiveCompulsive", "Schizotypy","Impulsivity",
  "SocialAnxiety","EatingDisorders","AlcoholAbuse","Apathy","GeneralisedAnxiety")

mean_sf$LIWC <- tolower(mean_sf$LIWC)
mean_sf$LIWC[which(mean_sf$LIWC == "wc")] <- "word count"

sds <- (mean_sf[order(mean_sf$Depression,decreasing = T)[1:20],])
oci <- (mean_sf[order(mean_sf$ObsessiveCompulsive,decreasing = T)[1:20],])
ssms <- (mean_sf[order(mean_sf$Schizotypy,decreasing = T)[1:20],])
bis <- (mean_sf[order(mean_sf$Impulsivity,decreasing = T)[1:20],])
lsas <- (mean_sf[order(mean_sf$SocialAnxiety,decreasing = T)[1:20],])
eat <- (mean_sf[order(mean_sf$EatingDisorders,decreasing = T)[1:20],])
audit <- (mean_sf[order(mean_sf$AlcoholAbuse,decreasing = T)[1:20],])
aes <- (mean_sf[order(mean_sf$Apathy,decreasing = T)[1:20],])
stai <- (mean_sf[order(mean_sf$GeneralisedAnxiety,decreasing = T)[1:20],])

top_20 <- as.data.frame(table(c(as.character(sds$LIWC),as.character(oci$LIWC),as.character(ssms$LIWC),
  as.character(bis$LIWC),as.character(lsas$LIWC),as.character(eat$LIWC),
  as.character(audit$LIWC),as.character(aes$LIWC),as.character(stai$LIWC))))
colnames(top_20) <- c("LIWC","Freq")

top_20$LIWC <- tolower(top_20$LIWC)
top_20$LIWC[which(top_20$LIWC == "wc")] <- "word count"



sds <- merge(sds,top_20,by="LIWC")
sds$Freq <- (as.factor(sds$Freq))
sds$Freq <- factor(sds$Freq, levels=rev(levels(sds$Freq)))
temp <- merge(sds,betas_sf[,c("LIWC","SDS")],by="LIWC")
sds$LIWC <-  paste(temp$LIWC,temp$SDS)

oci <- merge(oci,top_20,by="LIWC")
oci$Freq <- (as.factor(oci$Freq))
oci$Freq <- factor(oci$Freq, levels=rev(levels(oci$Freq)))
temp <- merge(oci,betas_sf[,c("LIWC","OCI")],by="LIWC")
oci$LIWC <-  paste(temp$LIWC,temp$OCI)

ssms <- merge(ssms,top_20,by="LIWC")
ssms$Freq <- (as.factor(ssms$Freq))
ssms$Freq <- factor(ssms$Freq, levels=rev(levels(ssms$Freq)))
temp <- merge(ssms,betas_sf[,c("LIWC","SSMS")],by="LIWC")
ssms$LIWC <-  paste(temp$LIWC,temp$SSMS)

bis <- merge(bis,top_20,by="LIWC")
bis$Freq <- (as.factor(bis$Freq))
bis$Freq <- factor(bis$Freq, levels=rev(levels(bis$Freq)))
temp <- merge(bis,betas_sf[,c("LIWC","BIS")],by="LIWC")
bis$LIWC <-  paste(temp$LIWC,temp$BIS)

lsas <- merge(lsas,top_20,by="LIWC")
lsas$Freq <- (as.factor(lsas$Freq))
lsas$Freq <- factor(lsas$Freq, levels=rev(levels(lsas$Freq)))
temp <- merge(lsas,betas_sf[,c("LIWC","LSAS")],by="LIWC")
lsas$LIWC <-  paste(temp$LIWC,temp$LSAS)

eat <- merge(eat,top_20,by="LIWC")
eat$Freq <- (as.factor(eat$Freq))
eat$Freq <- factor(eat$Freq, levels=rev(levels(eat$Freq)))
temp <- merge(eat,betas_sf[,c("LIWC","EAT")],by="LIWC")
eat$LIWC <-  paste(temp$LIWC,temp$EAT)

audit <- merge(audit,top_20,by="LIWC")
audit$Freq <- (as.factor(audit$Freq))
audit$Freq <- factor(audit$Freq, levels=rev(levels(audit$Freq)))
temp <- merge(audit,betas_sf[,c("LIWC","AUDIT")],by="LIWC")
audit$LIWC <-  paste(temp$LIWC,temp$AUDIT)

aes <- merge(aes,top_20,by="LIWC")
aes$Freq <- (as.factor(aes$Freq))
aes$Freq <- factor(aes$Freq, levels=rev(levels(aes$Freq)))
temp <- merge(aes,betas_sf[,c("LIWC","AES")],by="LIWC")
aes$LIWC <-  paste(temp$LIWC,temp$AES)

stai <- merge(stai,top_20,by="LIWC")
stai$Freq <- (as.factor(stai$Freq))
stai$Freq <- factor(stai$Freq, levels=rev(levels(stai$Freq)))
temp <- merge(stai,betas_sf[,c("LIWC","STAI")],by="LIWC")
stai$LIWC <-  paste(temp$LIWC,temp$STAI)


sds.plt <- ggplot(data = sds, aes(x = reorder(LIWC, Depression), y = Depression,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = Depression,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Depression") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

oci.plt <- ggplot(data = oci, aes(x = reorder(LIWC, ObsessiveCompulsive), y = ObsessiveCompulsive,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = ObsessiveCompulsive,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Obsessive Compulsive") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

colnames(ssms)[11] <- "Frequency"
#ssms$Frequency <- factor(ssms$Frequency, levels = c("1", "2", "3","4","5","6","7"))
ssms.plt <- ggplot(data = ssms, aes(x = reorder(LIWC, Schizotypy), y = Schizotypy,fill = Frequency)) +
  geom_bar(stat="identity" ,aes(y = Schizotypy,fill = Frequency),
           size=1.2, position = "dodge2",width=0.8) + ylab("Schizotypy") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.legend + ylim(0,1) +  scale_fill_manual(values=custpal)

ssms.plt <- ssms.plt + guides(fill = guide_legend(override.aes = list(size = 20),reverse=TRUE))


bis.plt <- ggplot(data = bis, aes(x = reorder(LIWC, Impulsivity), y = Impulsivity,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = Impulsivity,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Impulsivity") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

lsas.plt <- ggplot(data = lsas, aes(x = reorder(LIWC, SocialAnxiety), y = SocialAnxiety,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = SocialAnxiety,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Social Anxiety") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)


eat.plt <- ggplot(data = eat, aes(x = reorder(LIWC, EatingDisorders), y = EatingDisorders,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = EatingDisorders,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Eating Disorders") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)


audit.plt <- ggplot(data = audit, aes(x = reorder(LIWC, AlcoholAbuse), y = AlcoholAbuse,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = AlcoholAbuse,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Alcohol Abuse") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

aes.plt <- ggplot(data = aes, aes(x = reorder(LIWC, Apathy), y = Apathy,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = Apathy,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Apathy") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

stai.plt <- ggplot(data = stai, aes(x = reorder(LIWC, GeneralisedAnxiety), y = GeneralisedAnxiety,fill = Freq)) +
  geom_bar(stat="identity" ,aes(y = GeneralisedAnxiety,fill = Freq),
           size=1.2, position = "dodge2",width=0.8) + ylab("Generalised Anxiety") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.noy + ylim(0,1) +  scale_fill_manual(values=custpal)

p2 <- (sds.plt + oci.plt + ssms.plt)/(bis.plt + lsas.plt + eat.plt) / (audit.plt + aes.plt + stai.plt)
##########################################################

en <- read.csv('ElasticNet/tf_only_all.csv')

en <- melt(en); colnames(en) <- c("scale","r2")

en$scale <- c("Depression","Obsessive Compulsive","Schizotypy","Impulsivity","Social Anxiety",
              "Eating Disorders","Alcohol Abuse","Apathy","Generalised Anxiety")


en$scale <- factor(en$scale, levels = c("Depression","Apathy","Alcohol Abuse","Impulsivity", "Eating Disorders","Social Anxiety",
                                        "Obsessive Compulsive","Schizotypy","Generalised Anxiety"))



cbpalette <- c("white","#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8","#DD8D29","#E2D200","#08306B")

p3 <- ggplot(data = en, aes(y = r2, x = scale,
                      fill = scale)) +
  geom_bar(stat="identity" ,aes(y = r2, color = scale),
           size=1.2, position = "dodge2",width=0.8) +
  scale_fill_manual(values=cbpalette) + scale_color_manual(values=c("black","#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8","#DD8D29","#E2D200","#08306B")) + 
  xlab("") + TD_theme.nolegend +  ylab(expression(R^2)) + scale_x_discrete(limits = rev(levels(en$scale))) + coord_flip()


tiff("Figures/SF_alltest.tiff", units="cm", width=90, height=90, res=600)

p2

dev.off()


tiff("Figures/SF_dend.tiff", units="cm", width=80, height=30, res=600)
(p3 + p1) + plot_layout(ncol=2,widths=c(1,1.5)) 
dev.off()


p4 <- (p3 + p1) + plot_layout(ncol=2,widths=c(1,1.5))



tiff("Figures/SF_all2.tiff", units="cm", width=90, height=90, res=600)

p2 / p4

dev.off()


#####################################################
