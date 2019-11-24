# ctrl+shift+enter to run the whole script
# prefer using rstudio 
# install.packages("reshape2")
# install.packages("tidyr")
#install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("corrplot")
#install.packages("devtools")
#install_github("vqv/ggbiplot")
#install.packages("ggfortify")
#install.packages("gclus")
#install.packages("pls")
#install.packages("factoextra")
library(factoextra)
library(pls)
library(gclus)
library(ggplot2)
library(ggbiplot)
library(devtools)
library(ggfortify)
library(corrplot)
library(reshape2)
library(tidyr)
library(dplyr)
library(tidyverse)
if(!require(farff)){install.packages("farff")}
library("farff")
library(lattice)

# reading all the tables

officialboxscore<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2012-18_officialBoxScore.csv")

playerboxscore<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2012-18_playerBoxScore.csv")

teamboxscore<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2012-18_teamBoxScore.csv")

standings_table<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2012-18_standings.csv")

salary_table<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\NBA_season1718_salary.csv")

playerboxscore_1718<- read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2017-18_playerBoxScore.csv")

teamboxscore_1718<-read.csv("C:\\Users\\Dell\\Documents\\Conestoga\\mathematics for data anlytics\\Project\\2017-18_teamBoxScore.csv")


# wins for player box score team boxscore and official box score

playerboxscore$teamRslt_ply<- '0'
playerboxscore$teamRslt_ply[playerboxscore$teamRslt=='Win'] <- '1'
playerboxscore$teamRslt_ply= as.numeric(playerboxscore$teamRslt_ply)

teamboxscore$teamRslt_tm<- '0'
teamboxscore$teamRslt_tm[teamboxscore$teamRslt=='Win'] <- '1'
teamboxscore$teamRslt_tm= as.numeric(teamboxscore$teamRslt_tm)

officialboxscore$teamRslt_off<- '0'
officialboxscore$teamRslt_off[officialboxscore$teamRslt=='Win'] <- '1'
officialboxscore$teamRslt_off= as.numeric(officialboxscore$teamRslt_off)

# transformation the data from oponenet point of view

playerboxscore$opptRslt_ply<- '0'
playerboxscore$opptRslt_ply[playerboxscore$opptRslt=='Win'] <- '1'
playerboxscore$opptRslt_ply= as.numeric(playerboxscore$opptRslt_ply)

teamboxscore$opptRslt_tm<- '0'
teamboxscore$opptRslt_tm[teamboxscore$opptRslt=='Win'] <- '1'
teamboxscore$opptRslt_tm= as.numeric(teamboxscore$opptRslt_tm)

officialboxscore$opptRslt_off<- '0'
officialboxscore$opptRslt_off[officialboxscore$opptRslt=='Win'] <- '1'
officialboxscore$opptRslt_off= as.numeric(officialboxscore$opptRslt_off)

# team location home or away
# home is given 1 and away is given 0
playerboxscore$teamLoc_ply<- '0'
playerboxscore$teamLoc_ply[playerboxscore$teamLoc=='Home'] <- '1'
playerboxscore$teamLoc_ply= as.numeric(playerboxscore$teamLoc_ply)

teamboxscore$teamLoc_tm<- '0'
teamboxscore$teamLoc_tm[teamboxscore$teamLoc=='Home'] <- '1'
teamboxscore$teamLoc_tm= as.numeric(teamboxscore$teamLoc_tm)

officialboxscore$teamLoc_off<- '0'
officialboxscore$teamLoc_off[officialboxscore$teamLoc=='Home'] <- '1'
officialboxscore$teamLoc_off= as.numeric(officialboxscore$teamLoc_off)

# opposition location home or away

playerboxscore$opptLoc_ply<- '0'
playerboxscore$opptLoc_ply[playerboxscore$opptLoc=='Home'] <- '1'
playerboxscore$opptLoc_ply= as.numeric(playerboxscore$opptLoc_ply)

teamboxscore$opptLoc_tm<- '0'
teamboxscore$opptLoc_tm[teamboxscore$opptLoc=='Home'] <- '1'
teamboxscore$opptLoc_tm= as.numeric(teamboxscore$opptLoc_tm)


officialboxscore$opptLoc_off<- '0'
officialboxscore$opptLoc_off[officialboxscore$opptLoc=='Home'] <- '1'
officialboxscore$opptLoc_off= as.numeric(officialboxscore$opptLoc_off)

# transforming conference
# east is given 0 west is given 1

playerboxscore$teamConf_ply<- '0'
playerboxscore$teamConf_ply[playerboxscore$teamConf=='West'] <- '1'
playerboxscore$teamConf_ply= as.numeric(playerboxscore$teamConf_ply)

teamboxscore$teamConf_tm<- '0'
teamboxscore$teamConf_tm[teamboxscore$teamConf=='West'] <- '1'
teamboxscore$teamConf_tm= as.numeric(teamboxscore$teamConf_tm)

officialboxscore$teamConf_off<- '0'
officialboxscore$teamConf_off[officialboxscore$teamConf=='West'] <- '1'
officialboxscore$teamConf_off= as.numeric(officialboxscore$teamConf_off)


# opposition conference


playerboxscore$opptConf_ply<- '0'
playerboxscore$opptConf_ply[playerboxscore$opptConf=='West'] <- '1'
playerboxscore$opptConf_ply= as.numeric(playerboxscore$opptConf_ply)

teamboxscore$opptConf_tm<- '0'
teamboxscore$opptConf_tm[teamboxscore$opptConf=='West'] <- '1'
teamboxscore$opptConf_tm= as.numeric(teamboxscore$opptLoc_tm)


officialboxscore$opptConf_off<- '0'
officialboxscore$opptConf_off[officialboxscore$opptConf=='West'] <- '1'
officialboxscore$opptConf_off= as.numeric(officialboxscore$opptConf_off)

# player starter or bench

playerboxscore$playStat_ply<- '0'
playerboxscore$playStat_ply[playerboxscore$playStat=='Starter'] <- '1'
playerboxscore$playStat_ply= as.numeric(playerboxscore$playStat_ply)


# player position
# pg is given 0 sg is given 1 sf is given 2 pf is given 3 c is given 4

playerboxscore$playPos_ply<- '0'
playerboxscore$playPos_ply[playerboxscore$playPos=='SG'] <- '1'
playerboxscore$playPos_ply[playerboxscore$playPos=='SF'] <- '2'
playerboxscore$playPos_ply[playerboxscore$playPos=='PF'] <- '3'
playerboxscore$playPos_ply[playerboxscore$playPos=='C'] <- '4'
playerboxscore$playPos_ply= as.numeric(playerboxscore$playPos_ply)
 

# transforming div

playerboxscore$teamDiv_ply<- '0'
playerboxscore$teamDiv_ply[playerboxscore$teamDiv=='Central'] <- '1'
playerboxscore$teamDiv_ply[playerboxscore$teamDiv=='Atlantic'] <- '2'
playerboxscore$teamDiv_ply[playerboxscore$teamDiv=='Southwest'] <- '3'
playerboxscore$teamDiv_ply[playerboxscore$teamDiv=='Pacific'] <- '4'
playerboxscore$teamDiv_ply[playerboxscore$teamDiv=='Northwest'] <- '5'
playerboxscore$teamDiv_ply= as.numeric(playerboxscore$teamDiv_ply)



teamboxscore$teamDiv_tm<- '0'
teamboxscore$teamDiv_tm[teamboxscore$teamDiv=='Central'] <- '1'
teamboxscore$teamDiv_tm[teamboxscore$teamDiv=='Atlantic'] <- '2'
teamboxscore$teamDiv_tm[teamboxscore$teamDiv=='Southwest'] <- '3'
teamboxscore$teamDiv_tm[teamboxscore$teamDiv=='Pacific'] <- '4'
teamboxscore$teamDiv_tm[teamboxscore$teamDiv=='Northwest'] <- '5'
teamboxscore$teamDiv_tm= as.numeric(teamboxscore$teamDiv_tm)



officialboxscore$teamDiv_off<- '0'
officialboxscore$teamDiv_off[officialboxscore$teamDiv=='Central'] <- '1'
officialboxscore$teamDiv_off[officialboxscore$teamDiv=='Atlantic'] <- '2'
officialboxscore$teamDiv_off[officialboxscore$teamDiv=='Southwest'] <- '3'
officialboxscore$teamDiv_off[officialboxscore$teamDiv=='Pacific'] <- '4'
officialboxscore$teamDiv_off[officialboxscore$teamDiv=='Northwest'] <- '5'
officialboxscore$teamDiv_off= as.numeric(officialboxscore$teamDiv_off)


# opposition conference


playerboxscore$opptDiv_ply<- '0'
playerboxscore$opptDiv_ply[playerboxscore$opptDiv=='Central'] <- '1'
playerboxscore$opptDiv_ply[playerboxscore$opptDiv=='Atlantic'] <- '2'
playerboxscore$opptDiv_ply[playerboxscore$opptDiv=='Southwest'] <- '3'
playerboxscore$opptDiv_ply[playerboxscore$opptDiv=='Pacific'] <- '4'
playerboxscore$opptDiv_ply[playerboxscore$opptDiv=='Northwest'] <- '5'
playerboxscore$opptDiv_ply= as.numeric(playerboxscore$opptDiv_ply)



teamboxscore$opptDiv_tm<- '0'
teamboxscore$opptDiv_tm[teamboxscore$opptDiv=='Central'] <- '1'
teamboxscore$opptDiv_tm[teamboxscore$opptDiv=='Atlantic'] <- '2'
teamboxscore$opptDiv_tm[teamboxscore$opptDiv=='Southwest'] <- '3'
teamboxscore$opptDiv_tm[teamboxscore$opptDiv=='Pacific'] <- '4'
teamboxscore$opptDiv_tm[teamboxscore$opptDiv=='Northwest'] <- '5'
teamboxscore$opptDiv_tm= as.numeric(teamboxscore$opptDiv_tm)



officialboxscore$opptDiv_off<- '0'
officialboxscore$opptDiv_off[officialboxscore$opptDiv=='Central'] <- '1'
officialboxscore$opptDiv_off[officialboxscore$opptDiv=='Atlantic'] <- '2'
officialboxscore$opptDiv_off[officialboxscore$opptDiv=='Southwest'] <- '3'
officialboxscore$opptDiv_off[officialboxscore$opptDiv=='Pacific'] <- '4'
officialboxscore$opptDiv_off[officialboxscore$opptDiv=='Northwest'] <- '5'
officialboxscore$opptDiv_off= as.numeric(officialboxscore$opptDiv_off)



new_playerboxscore<-select_if(playerboxscore, is.numeric)
summary(new_playerboxscore)
new_teamboxscore<-select_if(teamboxscore, is.numeric)
summary(new_teamboxscore)



# salary solution
playerboxscore_1718 <- within(playerboxscore_1718,  Player <- paste(playFNm,playLNm), sep=" ")


playerboxscore_1718s<-playerboxscore_1718%>%
  select(playPTS,playAST,Player,playTO,playSTL,playBLK,playPF,playFGA,playFGM,playMin)%>%
  group_by(Player)%>%
  summarize(pts_mean=mean(playPTS),ast_mean=mean(playAST),to_mean=mean(playTO),stl_mean=mean(playSTL),blk_mean=mean(playBLK),PF_mean=mean(playPF),FGM_mean=mean(playFGM),min_mean=mean(playMin))

salary_table_order <- salary_table[order(salary_table$Player),]


# matching and merging data that is common in both salary table

x<-playerboxscore_1718s %>%
  full_join(salary_table_order, by = intersect(colnames(playerboxscore_1718s$Player), colnames(salary_table_order$Player))) %>%
  group_by(Player)

x_Df<- x[-which(is.na(x$pts_mean)), ]

x_Df$X = NULL
x_Df$Tm = NULL

summary(x_Df)

# replacing with mean value
x_Df[["season17_18"]][is.na(x_Df[["season17_18"]])] <-6439044

new_x_Df<-select_if(x_Df, is.numeric)
new_x_Df$Player=NULL
new_x_Df

par(mfrow=c(2,2)) 

sapply(names(new_x_Df), function(cname){
  if (is.numeric(new_x_Df[[cname]]))
    print(hist(new_x_Df[[cname]], main=cname,col="blue"))
})

par(mfrow=c(1,1))

par(mfrow=c(2,2))

sapply(names(new_x_Df), function(cname){
  if (is.numeric(new_x_Df[[cname]]))
    print(plot(new_x_Df[[cname]], main=cname,col="purple"))
})

par(mfrow=c(1,1))

par(mfrow=c(2,2))

# calculating outliers

sapply(names(new_x_Df), function(cname){
  if (is.numeric(new_x_Df[[cname]]))
    print(boxplot(new_x_Df[[cname]], main=cname))
})

par(mfrow=c(1,1))

# points outliers
pts_mean_IQR <- IQR(new_x_Df[['pts_mean']]) *1.5
quantile(new_x_Df[['pts_mean']])
pts_mean_IQR
pts_mean_Q1 <-3.485
pts_mean_Q3 <- 11.48
OUTLIER_TABLE_new_x_Df <- new_x_Df
OUTLIER_TABLE_new_x_Df$Outlier <- 'NO'
OUTLIER_TABLE_new_x_Df$Outlier[OUTLIER_TABLE_new_x_Df$pts_mean > pts_mean_IQR + pts_mean_Q3] <-'YES'
OUTLIER_TABLE_new_x_Df$Outlier[OUTLIER_TABLE_new_x_Df$pts_mean <pts_mean_Q1-pts_mean_IQR] <-'YES'

#salary

season17_18_IQR <- IQR(new_x_Df[['season17_18']]) *1.5
quantile(new_x_Df[['season17_18']])
season17_18_IQR
season17_18_Q1 <-1577230
season17_18_Q3 <- 6439044
OUTLIER_TABLE_new_x_Df <- new_x_Df
OUTLIER_TABLE_new_x_Df$Outlier_salary <- 'NO'
OUTLIER_TABLE_new_x_Df$Outlier_salary[OUTLIER_TABLE_new_x_Df$season17_18 > season17_18_IQR + season17_18_Q3] <-'YES'
OUTLIER_TABLE_new_x_Df$Outlier_salary[OUTLIER_TABLE_new_x_Df$season17_18 <season17_18_Q1-season17_18_IQR] <-'YES'


sapply(names(new_x_Df), function(cname){
  if (is.numeric(new_x_Df[[cname]]))
    print(shapiro.test(new_x_Df[[cname]]))
})

par(mfrow=c(2,2))

sapply(names(new_x_Df), function(cname){
  if (is.numeric(new_x_Df[[cname]]))
    print(qqnorm(new_x_Df[[cname]]))
    print(qqline(new_x_Df[[cname]]))
})

par(mfrow=c(1,1))



x_df_cor <- cor(new_x_Df, method='spearman')
round(x_df_cor, 2)
x_df_cor[abs(x_df_cor<0.5)] <- NA
x_df_cor

x_df_cor_plot<-x_df_cor
x_df_cor_plot.r <- abs(cor(x_df_cor_plot))                  
x_df_cor_plot.c <- dmat.color(x_df_cor_plot.r)              
cpairs(x_df_cor_plot, panel.colors=x_df_cor_plot.c, gap=.5, main="Key Variables Ordered and Coloured by Correlation")

# calculating principal components
pc<-prcomp(na.omit(new_x_Df),cor=TRUE)
summary(pc)
new_x_Df.pca <- prcomp(new_x_Df, scale. = TRUE)
ggbiplot::ggbiplot(new_x_Df.pca)


# Refree solution
# calculating home court advantage


criteria_teamboxscore<-subset(teamboxscore,select=c("teamPF","teamRslt_tm","teamPTS","teamFTA","teamTO","teamLoc_tm"))
x_away<-(criteria_teamboxscore[teamboxscore$teamLoc_tm == '0',])

y_home<-(criteria_teamboxscore[teamboxscore$teamLoc_tm == '1',])



boxplot(x_away$teamPTS,y_home$teamPTS)
boxplot(x_away$teamPF,y_home$teamPF)
boxplot(x_away$teamFTA,y_home$teamFTA)


par(mfrow = c(3,3))
hist(x_away$teamPTS,col="green")
hist(y_home$teamPTS,col="red")

hist(x_away$teamPF,col="blue")
hist(y_home$teamPF,col="yellow")


hist(x_away$teamFTA,col="orange")
hist(y_home$teamFTA,col="purple")

par(mfrow=c(1,1))



# wilcox test # running comparison tests
dif_PF<-wilcox.test(x_away$teamPF,y_home$teamPF)
dif_PF

dif_PTS<-wilcox.test(x_away$teamPTS,y_home$teamPTS)
dif_PTS

dif_FTA<-wilcox.test(x_away$teamFTA,y_home$teamFTA)
dif_FTA

boxplot(x_away$teamPF,y_home$teamPF,main="home fouls vs away fouls")
boxplot(x_away$teamFTA,y_home$teamFTA)

# tiredness  and impact of each quarter


summary(teamboxscore$teamPTS1)
summary(teamboxscore$teamPTS2)
summary(teamboxscore$teamPTS3)
summary(teamboxscore$teamPTS4)




shapiro.test(teamboxscore$teamPTS4[1:500])
shapiro.test(teamboxscore$teamPTS3[1:500])
shapiro.test(teamboxscore$teamPTS2[1:500])
shapiro.test(teamboxscore$teamPTS1[1:500])



qqnorm(teamboxscore$teamPTS1,main=' teamPTS1 is normal?')
qqline(teamboxscore$teamPTS1)

qqnorm(teamboxscore$teamPTS2,main=' teamPTS2 is normal?')
qqline(teamboxscore$teamPTS2)

qqnorm(teamboxscore$teamPTS3,main=' teamPTS3 is normal?')
qqline(teamboxscore$teamPTS3)

qqnorm(teamboxscore$teamPTS4,main=' teamPTS4 is normal?')
qqline(teamboxscore$teamPTS4)

wilcox.test(teamboxscore$teamPTS1,teamboxscore$teamPTS4)

wilcox.test(teamboxscore$teamPTS1,teamboxscore$teamPTS3)



# Do back to back games affect

back_teamboxscore<-subset(teamboxscore,select=c("teamPF","teamRslt_tm","teamPTS","teamFTA","teamTO","teamLoc_tm","teamDayOff"))
games_zero<-(back_teamboxscore[teamboxscore$teamDayOff == '0',])
games_not_zero<-(criteria_teamboxscore[teamboxscore$teamDayOff!='0',])


shapiro.test(games_zero$teamPF[1:500])
shapiro.test(games_not_zero$teamPF[1:500])
shapiro.test(games_zero$teamPTS[1:500])
shapiro.test(games_not_zero$teamPTS[1:500])


qqnorm(games_zero$teamPF,main='teamPF normal?')
qqline(games_zero$teamPF)

qqnorm(games_zero$teamPTS,main=' teamPTs Normal')
qqline(games_zero$teamPTS)

qqnorm(games_not_zero$teamPF,main=' teamPF is normal?')
qqline(games_not_zero$teamPF)

qqnorm(games_not_zero$teamPTS,main=' teamPTS is normal?')
qqline(games_not_zero$teamPTS)


t.test(games_zero$teamPTS,games_not_zero$teamPTS)
t.test(games_zero$teamRslt_tm,games_not_zero$teamRslt_tm)
t.test(games_zero$teamPF,games_not_zero$teamPF)


par(mfrow = c(1,2))
hist(games_zero$teamPTS,col="green")
hist(games_not_zero$teamPTS,col="red")

par(mfrow = c(3,3))
hist(games_zero$teamPTS,col="green")
hist(games_not_zero$teamPTS,col="red")



hist(games_zero$teamRslt_tm,col="blue")
hist(games_not_zero$teamRslt_tm,col="yellow")

hist(games_zero$teamPF,col="orange")
hist(games_not_zero$teamPF,col="purple")

par(mfrow=c(1,1))


# ideal skills to be a good player


playerboxscore <- within(playerboxscore,  Player <- paste(playFNm,playLNm), sep=" ")


playerboxscore_s<-playerboxscore%>%
  select(playPTS,playAST,Player,playTO,playSTL,playBLK,playPF,playFGA,playFGM,playMin,playFTM,playTRB,playHeight,playPos_ply,playWeight,opptDayOff,teamAbbr)%>%
  group_by(Player)%>%
  summarize(pts_mean=mean(playPTS),ast_mean=mean(playAST),to_mean=mean(playTO),stl_mean=mean(playSTL),blk_mean=mean(playBLK),PF_mean=mean(playPF),FGM_mean=mean(playFGM),min_mean=mean(playMin),fta_mean=mean(playFTM),trb_mean=mean(playTRB),height_mean=mean(playHeight),pos_mean=mean(playPos_ply),weight_mean=mean(playWeight),dayoff_mean=mean(opptDayOff))

par(mfrow=c(2,2))

sapply(names(playerboxscore_s), function(cname){
  if (is.numeric(playerboxscore_s[[cname]]))
    print(hist(playerboxscore_s[[cname]], main=cname,col='orange'))
})

par(mfrow=c(1,1))


new_playerboxscore_s<-select_if(playerboxscore_s, is.numeric)
skils<-cor(new_playerboxscore_s,method="spearman")
round(skils,2)
skils


skills_plot<-skils
skills_plot.r <- abs(cor(skills_plot))                  
skills_plot.c <- dmat.color(skills_plot.r)              
skills_plot.o <- order.single(skills_plot.r)          
cpairs(skills_plot, panel.colors=skills_plot.c, gap=.5, main="Key Variables Ordered and Coloured by Correlation")

# baseline model
skills_baseline = lm(new_playerboxscore_s$min_mean ~ pts_mean+ast_mean+to_mean+stl_mean+blk_mean+PF_mean+FGM_mean+fta_mean+trb_mean+height_mean+weight_mean+pos_mean+dayoff_mean,data=playerboxscore_s, na.action=na.omit)

skills_baseline
summary(skills_baseline)

par(mfrow = c(2, 2))  
plot(skills_baseline) 
par(mfrow = c(1, 1)) 

# backward
skills_baseline_backward = step(skills_baseline, direction="backward")
skills_baseline_backward
summary(skills_baseline_backward)

par(mfrow = c(2, 2))  
plot(skills_baseline_backward) 
par(mfrow = c(1, 1)) 


# criteria 
skills_baseline_criteria = step(skills_baseline)
skills_baseline_criteria
summary(skills_baseline_criteria)


par(mfrow = c(2, 2))  
plot(skills_baseline_criteria) 
par(mfrow = c(1, 1)) 


# manual model
skills_baseline_manual = lm(min_mean ~ pts_mean+ast_mean+to_mean+stl_mean+blk_mean+PF_mean+FGM_mean+min_mean+fta_mean+trb_mean,data=playerboxscore_s, na.action=na.omit)

skills_baseline_manual
summary(skills_baseline_manual)
pred_skills<-predict(skills_baseline_manual)
resd_skills<-residuals(skills_baseline_manual)


par(mfrow = c(2, 2))  
plot(skills_baseline_manual)
par(mfrow = c(1, 1))


# team success

teamboxscore_s<-teamboxscore%>%
  select(teamAbbr,teamRslt_tm,teamDayOff,teamPTS,teamTO,teamSTL,teamBLK,teamPF,teamFGA,teamFGM,teamMin,teamFTA,teamTRB,teamAST,teamLoc_tm,teamDiv_tm,teamPTS1,teamPTS2,teamPTS3,teamPTS4,teamRslt_tm,pace)%>%
  group_by(teamAbbr)%>%
  summarize(pts_mean=mean(teamPTS),ast_mean=mean(teamAST),to_mean=mean(teamTO),stl_mean=mean(teamSTL),blk_mean=mean(teamBLK),PF_mean=mean(teamPF),FGM_mean=mean(teamFGM),fta_mean=mean(teamFTA),
            trb_mean=mean(teamTRB),dayoff_mean=mean(teamDayOff),mean_teamPTS1=mean(teamPTS1),mean_teamPTS2=mean(teamPTS2),mean_teamPTS3=mean(teamPTS3),
            mean_teamPTS4=mean(teamPTS4),mean_teamRslt_tm=mean(teamRslt_tm),mean_pace=mean(pace))

summary(teamboxscore_s)
teamboxscore_s<-teamboxscore_s[order(-teamboxscore_s$mean_teamRslt_tm),]


par(mfrow = c(3, 3))
sapply(names(teamboxscore_s), function(cname){
  if (is.numeric(teamboxscore_s[[cname]]))
    print(hist(teamboxscore_s[[cname]], main=cname),col='blue')
})



par(mfrow = c(3, 3))
sapply(names(teamboxscore_s), function(cname){
  if (is.numeric(teamboxscore_s[[cname]]))
    print(barplot(teamboxscore_s[[cname]], main=cname,names.arg =teamboxscore_s$teamAbbr,col="blue"))
})

par(mfrow = c(1, 1))

trend_plot<-select_if(teamboxscore_s, is.numeric)

trend_plot_Cor<-cor(trend_plot)
round(trend_plot_Cor,2)

trend_plot.r <- abs(cor(trend_plot))                  
trend_plot.c <- dmat.color(trend_plot.r)              
trend_plot.o <- order.single(trend_plot.r)          
cpairs(trend_plot, panel.colors=trend_plot.c, gap=.5, main="Key Variables Ordered and Coloured by Correlation")

success_model<-lm(mean_teamRslt_tm~pts_mean+mean_teamPTS3+FGM_mean,data=teamboxscore_s)

success_model
summary(success_model)
pred_success<-predict(success_model)
resd_success<-residuals(success_model)


par(mfrow = c(2, 2))  
plot(success_model)
par(mfrow = c(1, 1))

#eastern western conference

x_eastern<-(teamboxscore[teamboxscore$teamConf_tm == '0',])
y_western<-(teamboxscore[teamboxscore$teamConf_tm == '1',])

x_eastern_num<-select_if(x_eastern, is.numeric)

summary(x_eastern_num)

y_western_num<-select_if(y_western, is.numeric)

summary(y_western_num)


t.test(x_eastern_num,y_western_num)

par(mfrow = c(1,2))

hist(y_western_num$teamRslt_tm,col="blue",main="western conference results")
hist(x_eastern_num$teamRslt_tm,col="red",main="eastern conference results")
par(mfrow = c(1,1))

par(mfrow = c(3,3))
hist(y_western_num$teamPTS,col="green")
hist(x_eastern_num$teamPTS,col="red")

hist(y_western_num$teamRslt_tm,col="green")
hist(x_eastern_num$teamRslt_tm,col="red")



hist(y_western_num$teamFGM/y_western_num$teamFGA,col="green")
hist(x_eastern_num$teamFGM/x_eastern_num$teamFGA,col="red")


 # players play for big team
#big teams are bos lal gsw ny 


x_bigmarket<-playerboxscore[playerboxscore$teamAbbr== 'BOS'|playerboxscore$teamAbbr== 'LAL'|playerboxscore$teamAbbr== 'NY'|playerboxscore$teamAbbr== 'GSW',]
x_smallmarket<-playerboxscore[playerboxscore$teamAbbr!= 'BOS'|playerboxscore$teamAbbr!= 'LAL'|playerboxscore$teamAbbr!= 'NY'|playerboxscore$teamAbbr!= 'GSW',]

x_bigmarket<-x_bigmarket%>%
  select(playPTS,playAST,Player,playTO,playSTL,playBLK,playPF,playFGA,playFGM,playMin,playFTM,playTRB,playHeight,playPos_ply,playWeight,opptDayOff,teamAbbr)%>%
  group_by(Player)%>%
  summarize(pts_mean=mean(playPTS),ast_mean=mean(playAST),to_mean=mean(playTO),stl_mean=mean(playSTL),blk_mean=mean(playBLK),PF_mean=mean(playPF),FGM_mean=mean(playFGM),min_mean=mean(playMin),fta_mean=mean(playFTM),trb_mean=mean(playTRB),height_mean=mean(playHeight),pos_mean=mean(playPos_ply),weight_mean=mean(playWeight),dayoff_mean=mean(opptDayOff),)
            
  
x_smallmarket<-x_smallmarket%>%
  select(playPTS,playAST,Player,playTO,playSTL,playBLK,playPF,playFGA,playFGM,playMin,playFTM,playTRB,playHeight,playPos_ply,playWeight,opptDayOff,teamAbbr)%>%
  group_by(Player)%>%
  summarize(pts_mean=mean(playPTS),ast_mean=mean(playAST),to_mean=mean(playTO),stl_mean=mean(playSTL),blk_mean=mean(playBLK),PF_mean=mean(playPF),FGM_mean=mean(playFGM),min_mean=mean(playMin),fta_mean=mean(playFTM),trb_mean=mean(playTRB),height_mean=mean(playHeight),pos_mean=mean(playPos_ply),weight_mean=mean(playWeight),dayoff_mean=mean(opptDayOff))

summary(x_smallmarket)
summary(x_bigmarket)

par(mfrow = c(3, 3))
sapply(names(x_smallmarket), function(cname){
  if (is.numeric(x_smallmarket[[cname]]))
    print(hist(x_smallmarket[[cname]], main=cname,col="blue"))
})


par(mfrow = c(3, 3))
sapply(names(x_bigmarket), function(cname){
  if (is.numeric(x_bigmarket[[cname]]))
    print(hist(x_bigmarket[[cname]], main=cname,col="red"))
})


par(mfrow = c(3, 3))
sapply(names(x_smallmarket), function(cname){
  if (is.numeric(x_smallmarket[[cname]]))
    print(boxplot(x_smallmarket[[cname]], main=cname,col="blue"))
})


par(mfrow = c(3, 3))
sapply(names(x_bigmarket), function(cname){
  if (is.numeric(x_bigmarket[[cname]]))
    print(boxplot(x_bigmarket[[cname]], main=cname,col="red"))
})

par(mfrow = c(1, 1))

# comparison test

sapply(names(x_bigmarket), function(cname){
  if (is.numeric(x_bigmarket[[cname]]))
    print(wilcox.test(x_bigmarket[[cname]],x_smallmarket[[cname]] ))
})


# Standings summary

standings_table_s<-standings_table%>%
  select(teamAbbr,rank)%>%
  group_by(teamAbbr)%>%
  summarize(rank=mean(rank))


standings_table_s <- standings_table_s[order(standings_table_s$rank) , ]
print(standings_table_s)


barplot(standings_table_s$rank,names.arg =standings_table_s$teamAbbr,col = ifelse( standings_table_s$rank< 4,'red','green') )

plot(standings_table_s$rank,names.arg =standings_table_s$teamAbbr,col = ifelse( standings_table_s$rank< 4,'red','green') )

#
teamboxscore$gmdates1<-as.Date(teamboxscore$gmDate,)
teamboxscore_trend<-teamboxscore[teamboxscore$gmdates1 >= "2012-9-30" & teamboxscore$gmdates1 <= "2013-06-27",]



teamboxscore_trend<-teamboxscore_trend%>%
  select(teamAbbr,teamDayOff,teamPTS,teamTO,teamSTL,teamBLK,teamPF,teamFGA,teamFGM,teamMin,teamFTA,teamTRB,teamAST,teamLoc_tm,teamDiv_tm,teamPTS1,teamPTS2,teamPTS3,teamPTS4,pace,team3PM)%>%
  group_by(teamAbbr)%>%
  summarize(pts_mean=mean(teamPTS),ast_mean=mean(teamAST),to_mean=mean(teamTO),stl_mean=mean(teamSTL),blk_mean=mean(teamBLK),PF_mean=mean(teamPF),FGM_mean=mean(teamFGM),fta_mean=mean(teamFTA),
            trb_mean=mean(teamTRB),dayoff_mean=mean(teamDayOff),mean_teamPTS1=mean(teamPTS1),mean_teamPTS2=mean(teamPTS2),mean_teamPTS3=mean(teamPTS3),
            mean_teamPTS4=mean(teamPTS4),mean_pace=mean(pace),mean_3PM=mean(team3PM))


teamboxscore_1718$gmdates1<-as.Date(teamboxscore_1718$gmDate,)

teamboxscore_trend_1718<-teamboxscore_1718%>%
  select(teamAbbr,teamDayOff,teamPTS,teamTO,teamSTL,teamBLK,teamPF,teamFGA,teamFGM,teamMin,teamFTA,teamTRB,teamAST,teamPTS1,teamPTS2,teamPTS3,teamPTS4,pace,team3PM)%>%
  group_by(teamAbbr)%>%
  summarize(pts_mean=mean(teamPTS),ast_mean=mean(teamAST),to_mean=mean(teamTO),stl_mean=mean(teamSTL),blk_mean=mean(teamBLK),PF_mean=mean(teamPF),FGM_mean=mean(teamFGM),fta_mean=mean(teamFTA),
            trb_mean=mean(teamTRB),dayoff_mean=mean(teamDayOff),mean_teamPTS1=mean(teamPTS1),mean_teamPTS2=mean(teamPTS2),mean_teamPTS3=mean(teamPTS3),
            mean_teamPTS4=mean(teamPTS4),mean_pace=mean(pace),mean_3PM=mean(team3PM))


summary(teamboxscore_trend_1718)
summary(teamboxscore_trend)






par(mfrow = c(3,3))
hist(teamboxscore_trend_1718$pts_mean,col="green")
hist(teamboxscore_trend$pts_mean,col="red")

hist(teamboxscore_trend_1718$mean_pace,col="green")
hist(teamboxscore_trend$mean_pace,col="red")

hist(teamboxscore_trend_1718$mean_3PM,col="green")
hist(teamboxscore_trend$mean_3PM,col="red")

par(mfrow = c(1,1))


par(mfrow = c(3,3))
boxplot(teamboxscore_trend_1718$pts_mean,col="green",main= "pts in 17 18")
boxplot(teamboxscore_trend$pts_mean,col="red",main="pts in 12 13")

boxplot(teamboxscore_trend_1718$mean_pace,col="green",main="pace in 12-13")
boxplot(teamboxscore_trend$mean_pace,col="red",main="pace in 17-18")
boxplot(teamboxscore_trend_1718$mean_3PM,col="green")
boxplot(teamboxscore_trend$mean_3PM,col="red")
par(mfrow = c(1,1))

shapiro.test(teamboxscore_trend_1718$mean_pace)
qqnorm(teamboxscore_trend_1718$mean_pace,main='normal?')
qqline(teamboxscore_trend_1718$mean_pace)
shapiro.test(teamboxscore_trend$mean_pace)
qqnorm(teamboxscore_trend$mean_pace,main='Normal?')
qqline(teamboxscore_trend$mean_pace)

qqnorm(teamboxscore_trend$pts_mean,main='normal?')
qqline(teamboxscore_trend$pts_mean)

qqnorm(teamboxscore_trend_1718$pts_mean,main='normal?')
qqline(teamboxscore_trend_1718$pts_mean)

t.test(teamboxscore_trend$mean_pace,teamboxscore_trend_1718$mean_pace)
t.test(teamboxscore_trend$pts_mean,teamboxscore_trend_1718$pts_mean)
t.test(teamboxscore_trend$mean_3PM,teamboxscore_trend_1718$mean_3PM)




