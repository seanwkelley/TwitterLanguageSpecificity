#supplementary fig S7

library(faux)
library(dplyr)
library(patchwork)
library(ggplot2)

setwd('C:/Users/User/OneDrive - TCDUD.onmicrosoft.com/Gillan Lab Resources/Projects/Transdiagnostic_Twitter/')

a <- matrix(0, 100, 100)
diag(a) <- 1

a[100,1:99] <- c(rep(0,89),rep(0.32,10))
a[1:99,100] <- c(rep(0,89),rep(0.32,10))


#variables positively correlated with the outcome
a[90:99,90:99] <- 0.50
diag(a[90:99,90:99]) <- 1
dat <- rnorm_multi(n = 3000,
                   mu = 0,
                   sd = 1,
                   r = a,
                   varnames =  paste0(rep("var",100),seq(1,100)),
                   empirical = FALSE)

write.csv(dat,"ElasticNet/df_cor32_3000.csv",row.names = F)
####################################################
#Simulate data based on same correlation structure as Twitter text features
####################################################
a <- matrix(0, 100, 100)
diag(a) <- 1

a[100,1:99] <- c(rep(0,98),rep(0.32,1))
a[1:99,100] <- c(rep(0,98),rep(0.32,1))


#variables positively correlated with the outcome

dat <- rnorm_multi(n = 1000,
                   mu = 0,
                   sd = 1,
                   r = a,
                   varnames =  paste0(rep("var",100),seq(1,100)),
                   empirical = FALSE)

write.csv(dat,"ElasticNet/df_var1_cor32_1000.csv",row.names = F)
##################################################################
a <- matrix(0, 100, 100)
diag(a) <- 1

a[100,1:99] <- c(rep(0,79),rep(0.32,20))
a[1:99,100] <- c(rep(0,79),rep(0.32,20))


#variables positively correlated with the outcome
a[80:99,80:99] <- 0.50
diag(a[80:99,80:99]) <- 1
dat <- rnorm_multi(n = 1000,
                   mu = 0,
                   sd = 1,
                   r = a,
                   varnames =  paste0(rep("var",100),seq(1,100)),
                   empirical = FALSE)

write.csv(dat,"ElasticNet/df_var20_cor32_1000.csv",row.names = F)


#########################################################################
#########################################################################


df1 <- read.csv('ElasticNet/parameters_var1_cor32_1000_r2.csv')
df2 <- read.csv('ElasticNet/parameters_var1_cor32_3000_r2.csv')

model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g1 <- ggplot(model2_r2, aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab(bquote('R'^2))+ ylim(0,400) + 
  geom_vline(aes(xintercept = 0.025),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(1,1,0,0),"cm")) 


df1 <- read.csv('ElasticNet/parameters_var10_cor32_1000_r2.csv')
df2 <- read.csv('ElasticNet/parameters_var10_cor32_3000_r2.csv')


model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g2 <- ggplot(model2_r2, aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab(bquote('R'^2))+ ylim(0,400) + 
  geom_vline(aes(xintercept = 0.025),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 



df1 <- read.csv('ElasticNet/parameters_var20_cor32_1000_r2.csv')
df2 <- read.csv('ElasticNet/parameters_var20_cor32_3000_r2.csv')

model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g3 <- ggplot(model2_r2,aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab(bquote('R'^2))+ ylim(0,400) + 
  geom_vline(aes(xintercept = 0.025),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 


r2_plots <- g1 + g2 + g3





#########################################################################
#########################################################################
df1 <- read.csv('ElasticNet/parameters_var1_cor32_1000_mae.csv')
df2 <- read.csv('ElasticNet/parameters_var1_cor32_3000_mae.csv')

model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g1 <- model2_r2 %>%
  ggplot( aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab("Mean Absolute Error") + ylim(0,400) + 
  geom_vline(aes(xintercept = 0.8151544342273309),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 


df1 <- read.csv('ElasticNet/parameters_var10_cor32_1000_mae.csv')
df2 <- read.csv('ElasticNet/parameters_var10_cor32_3000_mae.csv')


model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g2 <- model2_r2 %>%
  ggplot( aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab("Mean Absolute Error") + ylim(0,400) + 
  geom_vline(aes(xintercept = 0.8151544342273309),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 



df1 <- read.csv('ElasticNet/parameters_var20_cor32_1000_mae.csv')
df2 <- read.csv('ElasticNet/parameters_var20_cor32_3000_mae.csv')

model2_r2 <- as.data.frame(rbind(df1,df2))
model2_r2$ss <- c(rep('1000',1000),rep('3000',1000))



g3 <- model2_r2 %>%
  ggplot( aes(x=MAE, fill=ss)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","black"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + 
  xlab("Mean Absolute Error") + ylim(0,400) + 
  geom_vline(aes(xintercept = 0.8151544342273309),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) 


mae_plots <- g1 + g2 + g3


##############################################################
##############################################################


png("Figures/FigureS7.png", units="cm", width=80, height=30, res=800)

r2_plots / mae_plots 

dev.off()






