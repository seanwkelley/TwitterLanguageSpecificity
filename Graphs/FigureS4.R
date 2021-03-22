# Supplementary Figure 4
library(dendextend)
library(ggdendro)
library(ggplot2)
library(patchwork)
library(reshape2)

#transdiagnostic scales - anxious depression, compulsivity and instrusive thoughts, social withdrawl

TD_theme.nolegend = theme(
  panel.background = element_blank(),
  text = element_text(size = 36, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,1,0,0),"cm"),
  axis.title.x = element_text(size = 36),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 36),
  axis.text.y = element_text(colour = "black",size = 46),
  plot.title = element_text(lineheight=.8, face="bold", size = 36),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



en <- read.csv('ElasticNet/tf_only_td.csv')

en <- melt(en); colnames(en) <- c("scale","r2")

en$scale <- c("Anxious\nDepression","Compulsivity\nIntrusive Thoughts","Social\nWithdrawal")


cbpalette <- c("#046C9A","#C51B7D","#0B775E")

td <- ggplot(data = en, aes(y = r2, x = scale,
                            fill = scale)) +
  geom_bar(stat="identity" ,aes(y = r2, color = scale),
           size=1.2, position = "dodge2",width=0.8) +
  scale_fill_manual(values=cbpalette) + scale_color_manual(values=c("#046C9A","#C51B7D","#0B775E")) + 
  xlab("") + TD_theme.nolegend +  ylab(expression(R^2)) + scale_x_discrete(limits = rev(levels(en$scale))) 

tiff("Figures/Td_r2.tiff", units="cm", width=50, height=30, res=600)
td
dev.off()
