library(ggplot2)  # Visualization
library(dplyr)
library(tidyverse)

pldata <- read.csv("~\\Placement_Data_Full_Class.csv")

summary(pldata)

ggplot(pldata, aes(gender))+
  geom_bar(color='black', fill='blue')+
  labs(title = "Barchart on count of Sex")

ggplot(pldata, aes(ssc_b))+
  geom_bar(color='black', fill= 'red')+
  labs(x="Board of Education",title = "Barchart of Board of Education")

ggplot(pldata, aes(ssc_p, hsc_p, color=factor(gender)))+
  geom_point(alpha= 0.5, size=1.5)+
  labs(x="Secondary Education percentage",y="Higher Secondary Education percentage",title = "Higher secondary Education Percentage Vs Secondary Education percentage")

ggplot(pldata, aes(ssc_p, hsc_p, color=factor(ssc_b)))+
  geom_point(alpha= 0.5, size=1.5)+
  labs(x="Secondary Education percentage",y="Higher Secondary Education percentage",title = "Higher secondary Education Percentage Vs Secondary Education percentage")

ggplot(pldata, aes(ssc_p, hsc_p, color=factor(hsc_s)))+
  geom_point(alpha= 0.5, size=1.5)+
  labs(x="Secondary Education percentage",y="Higher Secondary Education percentage",title = "Higher secondary Education Percentage Vs Secondary Education percentage")

ggplot(pldata, aes(ssc_p, hsc_p, color=factor(status)))+
  geom_point(alpha= 0.5, size=1.5)+
  geom_smooth(method = "lm",se=FALSE)+
  labs(x="Secondary Education percentage",y="Higher Secondary Education percentage",title = "Higher secondary Education Percentage Vs Secondary Education percentage")

ggplot(pldata,aes(x=gender,fill=ssc_b))+
  theme_bw()+
  facet_wrap(~hsc_s)+
  geom_bar()+
  labs(y="Gender Count",title = "Gender count by SSC Board and HSC Stream")

ggplot(pldata,aes(x=status,fill=degree_t))+
  theme_bw()+
  facet_wrap(~workex)+
  geom_bar()+
  labs(x="Work Experience",y="Count",title = "No. of Placed Students of Different degree w.r.t Experience")


ggplot(pldata, aes(degree_p, mba_p ,color=factor(degree_t)))+
  geom_point(aes(alpha= 0.5, size=1.5))+
  geom_smooth(method = "lm",se=FALSE)+
  labs(x="Degree Percent",y="MBA percentage",title = "Degree Percentage Vs MBA")


ggplot(pldata,aes(x=ssc_p,group=status))+
  geom_density(aes(color=status))+
  labs(title = "SSC percentage")


ggplot(pldata,aes(x=hsc_p,group=status))+
  geom_density(aes(color=status))+
  labs(title = "HSC percentage")

ggplot(pldata,aes(x=degree_p,group=status))+
  geom_density(aes(color=status))+
  labs(title = "Degree percentage")

ggplot(pldata,aes(x=mba_p,group=status))+
  geom_density(aes(color=status))+
  labs(title = "MBA percentage")

ggplot(pldata,aes(x=degree_p,fill=status))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  labs(y="Count",x="Degree Precentage",title = "Degree percentages with the status of the placement")

ggplot(pldata,aes(x=degree_p,fill=status))+
  theme_bw()+
  geom_histogram(binwidth = 5)+
  facet_wrap(~gender)+
  labs(y="Count",x="Degree Precentage",title = "Degree percentages with the status of the placement")


ggplot(pldata,aes(y=degree_p)) + 
  geom_boxplot(fill="lightblue",alpha=0.9) +
  scale_x_continuous(labels = NULL)+
  facet_wrap(~gender)+
  ggtitle("Boxplot of Degree % w.r.t Gender") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Degree Percentage (%)",caption = "Box plot showing the distribution of Degree percentages")

ggplot(pldata, aes(x = status, y = degree_p, group = status, fill = status))+
  geom_boxplot(outlier.color = "darkgreen", alpha = 0.25, show.legend = FALSE)+
  ggtitle("Distribution per the placement status") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Degree Percentage (%)",caption = "Box plot showing the distribution of Degree percentages per different placement status")


ggplot(pldata, aes(x = status, y = degree_p, group = status, fill = status))+
  geom_boxplot(show.legend = FALSE,outlier.color = "darkgreen", alpha = 0.25)+
  scale_y_continuous(breaks = seq(50,90,5))+
  facet_wrap(workex~gender)+
  ggtitle("Distribution per placement status for different values of work experience and genders") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y="Degree Percentage (%)",caption = "Box plot showing the distribution of Degree percentages per different placement status with work experience and gender")
