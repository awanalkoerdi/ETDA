###############################################################
### PROJECT OF:   Rutger Kemperman & Awan Al Koerdi

###############################################################
### TITLE:   Daphnia magna population modelling

###############################################################
### SHORT INTRODUCTION: <between 100 and 200 words: Introduce the ecological process and/or problem that will logically lead to your research question.>

###############################################################
### RESEARCH QUESTION:
# How are the vital rates in D. magna populations negatively affected by increased water temperature?

###############################################################
### HYPOTHESES AND EXPECTATIONS:
# H0: There is no difference in the vital rates of Daphnia magna populations A1-A4 and L as effect of increased water temperature.
# Ha: Vital rates of Daphnia magna populations A1-A4 and L are negatively affected by increased water temperature.

###############################################################
### METHODS: STUDY SYSTEM: <between 50 and 200 words: Describe the ecosystem/species.>


# Description of the dataset: The Daphnia magna dataset consists of 4 response variables (temp, size, gen, and n), 1 random variable (day, which is the date of each measurement), and 5 explanatory variables (surv, growth, eggs, offspring, and size.offspring).
# This dataset was compiled of observation data obtained from measuring several effects and constants in the study system of the Daphnia.
# The study was conducted under controlled circumstances in a simulated environment that matches real ponds. This means the experiment took place in a climate chamber with a set 16:8 hour light-dark regime and a set environmental temperature. Water temperature varied between 10 and 26 degrees Celsius by a created gradient, using a continuous overflow of demineralized water which kept the temperature gradient in place.





###############################################################
### METHODS: DATA ACQUISITION: <between 100 and 200 words: Describe how the data were collected.>
# The dataset was obtained from Eelke Jongejans and was primarily collected for another research targeting population level responses to temperature, density, and clonal differences in Daphnia.
# Dormant Daphnia magna eggs were collected from a small lake in Hilversum, and stored dark at 4 degrees Celsius.
# From 50 eggs 22 lineages were established, leading to 12 surviving populations. 4 of these clonal lineages were chosen randomly for research and observation, and 1 additional laboratory grown lineage was observed as well. These populations are A1 to A4, and population L.
# Individual measurements took place, performed with metrics as follows: three individuals × five clones × eight temperatures × twice a week × 11 weeks of measuring. Since some populations died off this resulted in 2293 individual observations.
# Vital rates were researched; population size and density, growth, survival, and reproduction were measured.
# Date, environment, and lineage were recorded at each observation.


###############################################################
### METHODS: ANALYSES: <include R code of data exploration and manipulation>
rm(list=ls(all=TRUE))
data <- read.csv("Raw_data/data.csv", na.strings=c("", NA), header=T)

#Data exploration
summary(data)
data$gen<-as.factor(data$gen)
data$temp<-as.numeric(data$temp)
data$gen<-as.numeric(data$gen)

#Data transformation
data$temp_stand<-(data$temp*4.86)+18.6
data$size_stand<-(data$size*0.67)+2.09
data$n_stand<-(data$n*75)+96
summary(data)

subset_A1<-subset(data,gen=="1")
subset_A2<-subset(data,gen=="2")
subset_A3<-subset(data,gen=="3")
subset_A4<-subset(data,gen=="4")
subset_L<-subset(data,gen=="5")

#Data exploration
# par(mar=c(2,2,0,0),las=1)
# layout(matrix(c(1,1,2,3,4,5,6,7),4,2,byrow=TRUE))
hist(data$size_stand,col="grey",main="");mtext(side=3,line=-5,'size_stand',cex=2)
hist(data$n_stand,col="grey",main="");mtext(side=3,line=-5,'n',cex=2)
hist(data$diff,col="grey",main="");mtext(side=3,line=-5,'diff',cex=2)
hist(data$surv,col="grey",main="", breaks = 2);mtext(side=3,line=-5,'surv',cex=2)
hist(data$growth,col="grey",main="");mtext(side=3,line=-5,'growth',cex=2)
hist(data$temp_stand,col="grey",main="");mtext(side=3,line=-5,'temp_stand',cex=2)
hist(data$day,col="grey",main="");mtext(side=3,line=-5,'day',cex=2)
hist(data$gen,col="grey",main="");mtext(side=3,line=-5,'gen',cex=2)
hist(data$eggs,col="grey",main="");mtext(side=3,line=-5,'eggs',cex=2)

panel.smooth2 <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                           cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}

panel.cor <- function(x, y, digits=1, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# Now we can plot the scatterplots and correlation coefficients:
pairs(data[,c('day','temp','gen','size','n','diff','surv', 'growth')], 
      lower.panel=panel.smooth2, 
      upper.panel=panel.cor,
      diag.panel=panel.hist)


###############################################################
### RESULTS: <describe your results (calculations, tables and/or graphs) between the R code needed to show these results.>



summary(subset_A1)
plot(subset_A1$temp,subset_A1$growth)

m1<-lm(subset_A1$temp~subset_A1$growth)
abline(m1)


m2<-lme(growth~temp*gen,data=data,random=~1|day,na.action="na.omit")
plot(m2)
m3<-lme(size_stand~temp*gen,data=data,random=~1|day,na.action="na.omit")
plot(m3)
#m2<-lme(biomass~ownForeign*species,data=rootBiomass2,random=~1|day,na.action="na.omit")

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
