library(readxl)
library(lubridate)
library(tidyverse)
library(ggplot2)

dates=read_excel("risks.xlsx", sheet = "dates")
dates <- dates %>% 
  mutate(date= start + (end-start)/2) 

data=read_excel("risks.xlsx", sheet = "english")
data$waves=8-rowSums(is.na(data[,2:9]))

data <-  data %>% 
  filter(waves>2) %>% 
  filter(food=="yes") %>% 
  gather(wave,value,w1:w8,na.rm=T)

data <- left_join(data, dates, by = "wave")

rank <- data %>% 
  group_by(risk) %>% 
  summarize(maxval=max(value))

data$risk=factor(data$risk,levels=rank$risk[order(rank$maxval,decreasing=T)])

#data$wave_num=as.numeric(substr(data$wave,2,2))
data$day=difftime(data$date,data$date[1],units="days")

cairo_pdf("timeseries.pdf",width=4.5,height=6)
ggplot(data,aes(x=date,y=value,color=risk)) +
  xlab("Year") + ylab("% Reporting Concern") +  ylim(0,80) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~risk,ncol=3)
dev.off()

#
library(lme4)

model1=lmer(value~1+(1|risk),data=data)
summary(model1)

model2=lmer(value~1+scale(day)+(1|risk),data=data)
summary(model2)

model3=lmer(value~1+scale(day)+(1+scale(day)|risk),data=data)
summary(model3)

anova(model1,model2,model3)

data$predictions=fitted(model3)
cairo_pdf("timeseries_reg.pdf",width=4.5,height=6)
ggplot(data,aes(x=date,y=value,color=risk)) +
  xlab("Year") + ylab("% Reporting Concern") +  ylim(0,80) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(show.legend = FALSE) +
  #geom_line(show.legend = FALSE) +
  geom_line(aes(x=date,y=predictions),show.legend = FALSE) +
  facet_wrap(~risk,ncol=3)
dev.off()

library(directlabels)
cairo_pdf("timeseries_reg_all.pdf",width=5,height=5)
ggplot(data,aes(x=as.Date(date),y=value,color=risk)) +
  xlab("Year") + ylab("% Reporting Concern") +  ylim(5,75) +
  theme_minimal() +
  xlim(as.Date(c('1/9/2012', '1/6/2019'), format="%d/%m/%Y") ) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_point(show.legend = FALSE,alpha=.4) +
  #geom_line(show.legend = FALSE,alpha=.2,lty=2) +
  geom_line(aes(x=as.Date(date),y=predictions),show.legend = FALSE) +
  geom_dl(aes(y=predictions,label=risk), method = list("first.bumpup",cex=.9,dl.trans(x = x - .2)))
dev.off()