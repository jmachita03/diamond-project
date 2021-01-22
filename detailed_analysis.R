# Buckley Dowdle (bd6fr)
# Xun Liu (xl4xw)
# Michael Pajewski (mtp9k)
# Jordan Machita (jm8ux)
# Stat 6021: Project 1
                

#load libraries
library(tidyverse)
library(ggplot2)
library(MASS)
library(leaps)
library(hexbin)
#load data
data <- read_csv('clean_diamond_data.csv')

#select only rows with carat size equal to or less than 2.0
data <- subset(data,carat<=2.0)
attach(data)


data <- read.table("clean_diamond_data.csv", header=TRUE, sep = ",") # importing data 

#setting up data 
attach(data)
#limiting the carrot to 2
data <- subset(data,carat<=2)

head(data)


#make clarify cut and color into factors 
clarity<-factor(clarity)
color<-factor(color) 
cut<-factor(cut) 


# Histogram of data for explotiry analysis 
#price histogram
ggplot(data, aes(x = price)) + 
  geom_histogram(binwidth = 250, fill="blue4") + 
  scale_x_continuous() + xlab('Price') + 
  ylab('Frequency')+labs(title="Histogram of Diamond Price")+xlim(0,20000)+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#sumary of price
summary(carat) 


#Diamond Price According to Cut
ggplot(data, aes(factor(cut), price, fill=cut)) + 
  geom_boxplot(outlier.colour = NA, ) + ggtitle("Diamond Price and Cut") + 
  xlab("Cut") + ylab("Price") + 
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(limits=c(0,10000), breaks=seq(0,10000,1000), expand = c(0, 0))+
  scale_fill_brewer(palette="BuPu")

#Diamond Price According to color
ggplot(data, aes(factor(color), price, fill=color)) + 
  geom_boxplot() + ggtitle("Diamond Price and Color") + 
  xlab("Color") + ylab("Price") + 
  scale_y_continuous(limits=c(0,10000), breaks=seq(0,10000,1000), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_fill_brewer(palette="BuPu")

#Diamond Price According to clarity
ggplot(data, aes(factor(clarity), price, fill=clarity)) + 
  geom_boxplot() + ggtitle("Diamond Price and Clarity") + 
  xlab("Clarity") + 
  ylab("Price")+
  scale_y_continuous(limits=c(0,10000), breaks=seq(0,10000,1000), expand = c(0, 0))+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette="BuPu")



#plot of carat vs price
ggplot(data, aes(x = carat, y = price)) + geom_point()
ggplot(data, aes(x = carat, y = price), xlim = c(0,5)) + geom_point() + xlab('Carat') + 
  ylab('Price')+labs(title="Distribution of Price of Diamonds with Weight")+
  theme(plot.title = element_text(hjust = 0.5 , ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) + stat_smooth(method="lm") 



# plot of carat vs price with clarity as color
ggplot(data, aes(x=carat, y=price, color=color)) + geom_point() +
  labs(title="Distribution of Price of Diamonds with Weight") +
  theme(plot.title = element_text(hjust = 0.5 , ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# plot of carat vs price with carat as cut
ggplot(data, aes(x=carat, y=price, color=cut)) + geom_point()  +
  labs(title="Distribution of Price of Diamonds with Weight") +
  theme(plot.title = element_text(hjust = 0.5 , ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# plot of carat vs price clarity as color
ggplot(data, aes(x=carat, y=price, color=clarity)) + geom_point() +
  labs(title="Distribution of Price of Diamonds with Weight") +
  theme(plot.title = element_text(hjust = 0.5 , ), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))










#ramomly order the data

set.seed(6021)
rows <- sample(nrow(data))
data <- data[rows, ]


#force categorical columns to factors
clarity <- factor(clarity)
cut <- factor(cut)
color <- factor(color)


regnull <- lm(price~1, data=data)
regfull <- lm(price~., data=data)

step(regnull, scope=list(lower=regnull, upper=regfull), direction="forward")
step(regfull, scope=list(lower=regnull, upper=regfull), direction="backward")
step(regnull, scope=list(lower=regnull, upper=regfull), direction="both")

#fit simple linear regression of price and carat
base_model <- lm(price~carat)
summary(base_model)


#fit linear model with all variables
model0 <- (lm(price~carat))
summary(model0)
boxcox(base_model)
#all p values are significant, no variables should be removed

#SLR for different catgories
ggplot(data, aes(x=log(carat),y=log(price),color =color)) +geom_point(aes(alpha=0.01))+geom_smooth(method="lm",aes(alpha=1))
ggplot(data, aes(x=log(carat),y=log(price),color =cut)) + geom_point(aes(alpha=0.01))+geom_smooth(method="lm",aes(alpha=1))
ggplot(data, aes(x=log(carat),y=log(price),color =clarity)) +geom_point(aes(alpha=0.01,stroke=0))+geom_smooth(method="lm",aes(alpha=1))

#anova of model0 compared to simple linear regression of price and carat
anova(base_model,model0)

#reject null hypothesis and use complex model

#will interaction between variables enchance model
model_int0 <- (lm(price~carat*color+clarity+cut))
summary(model_int0)

model_int1 <- (lm(price~carat+color*clarity+cut))
summary(model_int1)

model_int2<- (lm(price~carat+color+clarity*cut))
summary(model_int2)



#examine model1 to see if it meets assumptions
plot(price~carat)
abline(model_int0,col="red")

plot(model_int0$fitted.values,model0$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")


acf(model_int0$residuals, main="ACF of Residuals")

qqnorm(model_int0$residuals)
qqline(model_int0$residuals, col="red")

#model0 violates multiple assumptions

#use Box Cox plot to determine what transformation of the response variable
#is appropriate
boxcox(model_int0)

#based on the boxcox plot, a log transformation of the response is most approptiate

#fit linear model with all variables and log transformation of response
model1 <- (lm(log(price)~carat+color+clarity+cut))
summary(model1)

#all p values are significant, no variables should be removed



#examine model1 to see if it meets assumptions
plot(price~carat)
abline(model1,col="red")

plot(model1$fitted.values,model1$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")

?plot
acf(model1$residuals, main="ACF of Residuals")

qqnorm(model1$residuals)
qqline(model1$residuals, col="red")


#model1 still fails to meet assumptions

#refit model with a log transformation to carat
model2 <- (lm(log(price)~log(carat)*color+clarity+cut))
summary(model2)

#all p values are significant, no variables should be removed



#examine model2 to see if it meets assumptions
plot(log(price)~log(carat))
abline(model2,col="red")

plot(model2$fitted.values,model2$residuals, main="Plot of Residuals against Fitted Values")
abline(h=0,col="red")


acf(model2$residuals, main="ACF of Residuals", ylim=c(-.1,.3))

qqnorm(model2$residuals)
qqline(model2$residuals, col="red")

#model meets assumptions

#perform anova test of model2 against a simple linear regression of log(price) and log(carat) to assess
#if other predictors can be removed

log_base_model <- lm(log(price)~log(carat))

anova(model2, model1)

#null hypothesis rejected, use more complex model

library(qpcR)

PRESS <- function(linear.model) {
  #' calculate the predictive residuals
  pr <- residuals(linear.model)/(1-lm.influence(linear.model)$hat)
  #' calculate the PRESS
  PRESS <- sum(pr^2)
  
  return(PRESS)
}

PRESS(model2)

