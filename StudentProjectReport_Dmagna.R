###############################################################
### PROJECT OF:   Rutger Kemperman & Awan Al Koerdi

###############################################################
### TITLE:   Daphnia magna population modelling

###############################################################
### SHORT INTRODUCTION:
# Climate change is known to lead to increasing temperatures globally, and these temperature increases can have detrimental effects on cold-blooded animals, such as water fleas (Daphnia magna). Although D. magna clearly suffers from climate change, it is unclear what vital rates are affected most. As to sketch a clearer image of how D. magna are affected, several vital rates have been measured at increasing water temperatures. The effect of increased water temperature on D. magna was determined by measuring population density, size, growth, and survival. Herein it was assumed that increasing the water temperature would have a negative effect on several of the measured vital rates. 

###############################################################
### RESEARCH QUESTION:
# What vital rates in a D. magna populations are negatively affected by increased water temperature?

###############################################################
### HYPOTHESES AND EXPECTATIONS:
# H0: There is no difference in the vital rates of a Daphnia magna population as an effect of increased water temperature.
# Ha: The vital rates of a Daphnia magna population are negatively affected by increased water temperature. 

###############################################################
### METHODS: STUDY SYSTEM: 
# Description of the dataset: The Daphnia magna dataset consists of 3 response variables ( size, growth, and n), 1 fixed variable (temp), 1 random variable (day, as measurements were performed every 3 or 4 days), and 4 explanatory variables (surv, eggs, offspring, and size.offspring), as well as gen (Daphnia population lineage) as fixed variable.
# This dataset was compiled of observation data obtained from measuring several effects and constants in the study system of the Daphnia.
# The study was conducted under controlled circumstances in a simulated environment that matches real ponds. This means the experiment took place in a climate chamber with a set 16:8 hour light-dark regime and a set environmental temperature. Water temperature varied between 10 and 26 degrees Celsius by a created gradient, using a continuous overflow of demineralized water which kept the temperature gradient in place. 

###############################################################
### METHODS: DATA ACQUISITION: 
# The dataset was obtained from Eelke Jongejans and was primarily collected for another research targeting population level responses to temperature, density, and clonal differences in Daphnia.
# Dormant Daphnia magna eggs were collected from a small lake in Hilversum, and stored dark at 4 degrees Celsius.
# From 50 eggs 22 lineages were established, leading to 12 surviving populations. 4 of these clonal lineages were chosen randomly for research and observation, and 1 additional laboratory grown lineage was observed as well. These populations are A1 to A4, and population L.
# Individual measurements took place, performed with metrics as follows: three individuals × five clones × eight temperatures × twice a week × 11 weeks of measuring. Since some populations died off this resulted in 2293 individual observations.
# Vital rates were researched; population size and density, growth, survival, and reproduction were measured.
# Date, environment, and lineage were recorded at each observation.

###############################################################
### METHODS: ANALYSES:

#Import data and read file.
rm(list=ls(all=TRUE))
data <- read.csv("Raw_data/data.csv", na.strings=c("", NA), header=T)

#Data exploration
summary(data)

#Change classes from character to factor and numeric to be able to make linear models.
data$gen<-as.factor(data$gen)
data$temp<-as.numeric(data$temp)
data$gen<-as.numeric(data$gen) #transform to numeric values A1 becomes 1, A2 becomes 2 etc. 

#Data transformation
#Since part of the data was already standardized, we transformed the standardized data back to its normal values. These values were saved in new columns with "_norm" behind the original column name.
data$temp_norm<-(data$temp*4.86)+18.6
data$size_norm<-(data$size*0.67)+2.09
data$n_norm<-(data$n*75)+96
summary(data)

#We have a lot of observations which makes the data rather complex. To narrow it down, we made subsets of the data based on the lineage (gen). There are 5 populations. 
subset_A1<-subset(data,gen=="1")
subset_A2<-subset(data,gen=="2")
subset_A3<-subset(data,gen=="3")
subset_A4<-subset(data,gen=="4")
subset_L<-subset(data,gen=="5")

#Data exploration
#We chose one population, A2 which has the highest amount of observations. We looked at the distributions of this population by making histograms per variable.  

hist(subset_A2$size_norm,col="grey",main="");mtext(side=3,line=-5,'size_stand',cex=2)
hist(subset_A2$n_norm,col="grey",main="");mtext(side=3,line=-5,'n',cex=2)
hist(subset_A2$diff,col="grey",main="");mtext(side=3,line=-5,'diff',cex=2)
hist(subset_A2$surv,col="grey",main="", breaks = 2);mtext(side=3,line=-5,'surv',cex=2)
hist(subset_A2$growth,col="grey",main="");mtext(side=3,line=-5,'growth',cex=2)
hist(subset_A2$temp_norm,col="grey",main="");mtext(side=3,line=-5,'temp_stand',cex=2)
hist(subset_A2$day,col="grey",main="");mtext(side=3,line=-5,'day',cex=2)
hist(subset_A2$eggs,col="grey",main="");mtext(side=3,line=-5,'eggs',cex=2)

#Most of the variables are normally distributed. We decided not to log transform the data because it was not necessary.

###############################################################
### RESULTS: <describe your results (calculations, tables and/or graphs) between the R code needed to show these results.>

#Since survival rate was one of the vital rates, we made a boxplot to visualize the correlation between the survival rate and the temperature. 
boxplot(temp_norm~surv,
        data=subset_A2,
        main="Survival rate for different temperatures",
        xlab="Survival rate",
        ylab="Temperature",
        col="lightblue",
        border="black"
)

#At both lower and higher temperatures it is observed that the survival of the Daphnia decreases. as the average temperature for survival is lower than the average temperature of populations dying off, it can be assumed that the higher the temperature, the lower the survival rate of Daphnia.

#We made different linear models for each response variable against the temperature. We looked at each model to decide whether the temperature is correlated to the different response variables.
#m1 plots the mean size of a population against the temperatures. We observed that higher temperatures lead to bigger extremes in body size; meaning both larger and smaller bodied Daphnia. In general, at average temperatures Daphnia tend to show less dispersion in body size. The Q-Q plot shows that it is improbable however that the size of Daphnia correlates to the temperature.
#m2 plots the population density (n) against the temperature. Here we observed less linearity in the data. The data is more dispersed, although the Q-Q plot shows that the data is not normally distributed. However, significance can be found in this, thus we decided not to throw out any observations.


par(mfrow=c(2,2))
m1<-lm(size~temp,data=subset_A2)
abline(m1)
plot(m1)
summary(m1)

m2<-lm(n~temp, data=subset_A2)
abline(m2)
plot(m2)
summary(m2)

#To look into further detail at the distribution of the residuals we performed a shapiro test. The p-value
plot(subset_A2$n)
resn <- resid(m2, type = "pearson")
shapiro.test(resn)


#m3 plots the the growth against the temperature.It is shown that the data is well dispersed. The increased temperature does not seem to have an effect on the growth of Daphnia. The Q-Q plot shows a better probability here, meaning that it is possible that the growth correlates to the temperature. The Cook's distance shows that even the outliers are not incredibly far off. 
m3<-lm(growth~temp,data=subset_A2)
abline(m3)
plot(m3)
summary(m3)

require(nlme)
m4<-lme(size~temp,data=subset_A2,random=~1|day,na.action="na.omit")
plot(m5)
summary(m5)

m5<-lme(n~temp, data=subset_A2,random=~1|day,na.action="na.omit")
plot(m6)
summary(m6)


m6<-lme(growth~temp, data=subset_A2,random=~1|day,na.action="na.omit")
plot(m7)
summary(m7)
res1 <- resid(m7, type = "pearson") # Extract standardized residuals
res1
subset_A2[which(abs(res1) > 5.0),] # Get the rows which absolute residuals


all<-lme(size~temp+growth+n,
         data=subset_A2,
         random=~1|day,
         na.action="na.omit")
plot(all)
summary(all)

lme2<-lme(size~temp+growth,
          data=subset_A2,
          random=~1|day,
          na.action="na.omit")
plot(lme2)
summary(lme2)



###############################################################
### DISCUSSION: 
# This research aimed at exploring and visualizing the relationship between water temperature and the vital rates of Daphnia magna populations. Herein it was hypothesized that Daphnia in water at increased temperatures would see negative effects to their vital rates. The vital rates population density, growth, size, and survival were measured, leading to the dataset modelled. Since the dataset when obtained was quite usable and the data was clean, not a lot of manipulation on the data was required. Furthermore, the aforementioned vital rates of Daphnia were modelled against the temperature to get insights of the effect. To do so, both linear models and linear mixed effect models (with day as random effect) were created and assessed. It should be noted that day was a random variable since measurements were performed every 3 or 4 days. From the models a few assumptions can be made: According to the linear models, temperature had the greatest effect on the size whereas increased temperature led to bigger extremes in terms of size. On the other hand, growth, population density, and survival did not seem to directly be affected by increased temperature. With population density specifically, when looking at the Q-Q plot it can be seen that although it is not greatly affected by temperature, the graph bends off a lot at the edges, suggesting in this case that other effects might be at play. The LMEs that were created for each of these vital rates mostly reinforce the results gained from the linear models, although it also suggests that size is negatively affected by temperature.

###############################################################
### CONCLUSIONS:
# It can be concluded that temperature does not have an effect on all vital rates of Daphnia magna. Certain vital rates jump out however, such as size which is negatively affected by increased water temperatures. Furthermore, population density seems to be indirectly affected by increased temperature.


###############################################################
### LITERATURE: <just a few, using the concise Oecologia style shown in the examples below>

# Eley KA, Dhariwal DK (2010) A lucky catch: Fishhook injury of the tongue. J Emerg Trauma Shock 3:92-93

# McNoleg O (1996) The integration of GIS, remote sensing, expert systems and adaptive co-kriging for environmental habitat modeling of the Highland Haggis using object-oriented, fuzzy-logic and neural-network techniques. Comput Geosci 22:585-588

# Meyer-Rochow VB, Gal J (2003) Pressures produced when penguins pooh - calculations on avian defaecation. Polar Biol 27:56-58
# Sheldon RW, Kerr SR (1972) The population density of monsters in Loch Ness. Limnol Oceanogr 17:796-798
