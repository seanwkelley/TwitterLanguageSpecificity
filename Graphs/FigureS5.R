#Supplementary Figure S5

library(ggplot2)
library(patchwork)
library(reshape2)
library(igraph)
library(corrplot)
library(lattice)
library(dplyr)

TD_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.title = element_blank(),
  legend.text = element_text(size = 50),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 42),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(size = 46,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


TD_theme.nolegend = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  axis.title.x = element_text(size = 50),
  axis.title.y = element_text(size = 50),
  axis.text = element_text(size = 42),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 50),
  axis.text.y = element_text(size = 42,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

setwd('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')


#text features only
tf_only <- read.csv('ElasticNet/tf_only_mean_tw.csv')
tf_only_re <- read.csv('ElasticNet/tf_only_mean_re.csv')
tf_only_lk <- read.csv('ElasticNet/tf_only_mean_lk.csv')



en <- as.data.frame(cbind(t(tf_only),t(tf_only_re),t(tf_only_lk)))
colnames(en) <- c("Tweets","Retweets","Likes")


#####################################################################################
#####################################################################################

en$scale <- c("Depression","Obsessive\nCompulsive","Schizotypy","Impulsivity","Social\nAnxiety",
              "Eating\nDisorders","Alcohol\nAbuse","Apathy","Generalised\nAnxiety")

en <- melt(en); colnames(en) <- c("scale","condition","r2")

cbpalette <- c("#B40F20","#046C9A","#C51B7D","#0B775E")


en$scale <- factor(en$scale, levels = c("Depression","Alcohol\nAbuse","Apathy","Eating\nDisorders","Generalised\nAnxiety",
                                        "Impulsivity","Obsessive\nCompulsive","Schizotypy","Social\nAnxiety"))

p1 <- ggplot(data = en, aes(y = r2, x = scale,
                            fill = condition)) +
  geom_bar(stat="identity" ,aes(y = r2, color = condition),
           size=1.2, position = "dodge2",width=0.8) +
  geom_hline(yintercept=0,size=1,linetype = "solid") +  
  scale_fill_manual(values=cbpalette) + scale_color_manual(values=c("#B40F20","#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8","#DD8D29","#E2D200","#08306B")) + 
  xlab("") + ylab("t statistic") + TD_theme +  ylab(expression(R^2))




p1 <- p1 + guides(color = guide_legend(override.aes = list(size=15)))

p1

tiff("Figures/Elastic_Net_tweetcond.tiff", units="cm", width=90, height=30, res=600)
p1
dev.off()

######################################################################################################

#text features only
tf_only <- read.csv('ElasticNet/tf_only_0p.csv')
tf_only_10 <- read.csv('ElasticNet/tf_only_10p.csv')
tf_only_25 <- read.csv('ElasticNet/tf_only_25p.csv')
tf_only_50 <- read.csv('ElasticNet/tf_only_50p.csv')
tf_only_75 <- read.csv('ElasticNet/tf_only_75p.csv')

en <- as.data.frame(cbind(t(tf_only),t(tf_only_10),t(tf_only_25),t(tf_only_50),t(tf_only_75)))
colnames(en) <- c("0%","10%","25%","50%","75%")

en$scale <- c("Depression","Obsessive\nCompulsive","Schizotypy","Impulsivity","Social\nAnxiety",
              "Eating\nDisorders","Alcohol\nAbuse","Apathy","Generalised\nAnxiety")



en <- melt(en); colnames(en) <- c("scale","condition","r2")

en <- en %>% filter(scale == 'Depression')

en <- read.csv('ElasticNet/tf_only_sum.csv')
colnames(en) <- c("condition","r2")

en$condition <- as.factor(en$condition)

cbpalette <- c("#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8")


en$scale <- factor(en$scale, levels = c("Depression","Alcohol\nAbuse","Apathy","Eating\nDisorders","Generalised\nAnxiety",
                                        "Impulsivity","Obsessive\nCompulsive","Schizotypy","Social\nAnxiety"))

p2 <- ggplot(data = en, aes(y = r2, x = condition,
                            fill = condition)) +
  geom_bar(stat="identity" ,aes(y = r2, color = condition),
           size=1.2, position = "dodge2",width=0.8) +
  geom_hline(yintercept=0,size=1,linetype = "solid") +  
  scale_fill_manual(values=cbpalette) + scale_color_manual(values=c("#046C9A","#C51B7D","#0B775E","#8C510A","#46ACC8")) + 
  xlab("\nNumber of Tweets Quartile") + ylab("t statistic") + TD_theme.nolegend +  ylab(expression(R^2))

p2


p1 + p2 + plot_layout(widths = c(2, 1)) + plot_annotation(tag_levels = 'A')



tiff("Figures/Elastic_Net_tweetnum2.tiff", units="cm", width=140, height=50, res=600)
p1 + p2 + plot_layout(widths = c(2, 1)) 
dev.off()


