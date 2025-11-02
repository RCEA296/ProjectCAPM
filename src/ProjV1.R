
library("dplyr")
library("ggplot2")
rm(list = ls())
tsunami_data = read.csv("src/data/TsunamiData.csv", header = TRUE)

#The aim of study of this data set is to achieve to predict a Tsunami event and 
#look for the most relevant indicators 

#Feature Engineering

##Remove variables without useful information. 
#Categorical variables will be treated first
tsunami_data %>% count(alert)

tsunami_data = tsunami_data %>% 
  select(-c("title", "continent", "location", "country", "net","alert"))

#Different methods were used to measure earthquakes magnitude. Therefore, 
#a bias arises. To solve this, we filter by the moment magnitude based 
#methods, which are the modern standard and more accurate measurements (indicated with a "w" suffix)

tsunami_data %>% count(magType)#There is no significant loss of observations

tsunami_data = tsunami_data%>% 
  filter(magType %in% c("mw","mwb","mwc","mww"))  %>% select(-magType)

tsunami_data = tsunami_data %>% mutate(year = as.numeric(substr(date_time, 7,11)))%>% 
  select(-date_time)# We will focus on tendencies over the years, so we can convert the variable "year" into a numeric value

#We now have only numerical values. We must look for outliers
tsunami_data %>% select(sig, nst, dmin, gap, depth) %>% stack()%>% 
  
  ggplot(aes(y = values)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  facet_wrap(~ ind, scales = "free") + 
  labs(title = "Univariate Boxplots per Variable", x = "", y = "Value") +
  theme_bw()

#From this plot we can infer that most

tsunami_data %>% 

#Cite
#https://creativecommons.org/licenses/by/4.0/



