###############################################################
### PROJECT OF:   Rutger Kemperman & Awan Al Koerdi

###############################################################
### TITLE:   <informative title of your project (max 80 chars)>

###############################################################
### SHORT INTRODUCTION: <between 100 and 200 words: Introduce the ecological process and/or problem that will logically lead to your research question.>

###############################################################
### RESEARCH QUESTION: How does the temperature relate to the vital rates in a population?

###############################################################
### HYPOTHESES AND EXPECTATIONS: 
#H0 There is no difference
#Ha A1-A4 show similarity in relation to the vital rates, whereas L does less, due to differences in ponds.

###############################################################
### METHODS: STUDY SYSTEM: <between 50 and 200 words: Describe the ecosystem/species.>

###############################################################
### METHODS: DATA ACQUISITION: <between 100 and 200 words: Describe how the data were collected.>

###############################################################
### METHODS: ANALYSES: <include R code of data exploration and manipulation>
rm(list=ls(all=TRUE))
data <- read.csv("Raw_data/data.csv", na.strings=c("", NA), header=T)

#Data exploration
summary(data)
data$gen<-as.factor(data$gen)
data$temp<-as.numeric(data$temp)
data_realval<-data
data_realval$temp<-(data_realval$temp*4.86)+18.6



par(mar=c(2,2,0,0),las=1)
layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
hist(data$size,col="grey",main="");mtext(side=3,line=-5,'size',cex=2)
hist(data$n,col="grey",main="");mtext(side=3,line=-5,'n',cex=2)
hist(data$diff,col="grey",main="");mtext(side=3,line=-5,'diff',cex=2)
hist(data$surv,col="grey",main="");mtext(side=3,line=-5,'surv',cex=2)
hist(data$growth,col="grey",main="");mtext(side=3,line=-5,'growth',cex=2)



subset_A1<-subset(data,gen=="A1")
subset_A2<-subset(data,gen=="A2")
subset_A3<-subset(data,gen=="A3")
subset_A4<-subset(data,gen=="A4")
subset_L<-subset(data,gen=="L")


###############################################################
### RESULTS: <describe your results (calculations, tables and/or graphs) between the R code needed to show these results.>

###############################################################
### DISCUSSION: <between 150 and 300 words>

###############################################################
### CONCLUSIONS:  <a short (ca. 50 words) summary>

###############################################################
### LITERATURE: <just a few, using the concise Oecologia style shown in the examples below>

# Eley KA, Dhariwal DK (2010) A lucky catch: Fishhook injury of the tongue. J Emerg Trauma Shock 3:92-93

# McNoleg O (1996) The integration of GIS, remote sensing, expert systems and adaptive co-kriging for environmental habitat modeling of the Highland Haggis using object-oriented, fuzzy-logic and neural-network techniques. Comput Geosci 22:585-588

# Meyer-Rochow VB, Gal J (2003) Pressures produced when penguins pooh - calculations on avian defaecation. Polar Biol 27:56-58

# Sheldon RW, Kerr SR (1972) The population density of monsters in Loch Ness. Limnol Oceanogr 17:796-798
