library("tidyverse")
library("dplyr")
library("ggplot2")
library("factoextra")
library("GGally")
library("scatterplot3d")


rm(list = ls())
tsunami_data = read.csv("src/data/TsunamiData.csv", header = TRUE)

#The aim of study of this data set is to achieve to predict a Tsunami event and 
#look for the most relevant indicators 

#Feature Engineering

##Remove variables without useful information. 
#Categorical variables will be treated first
names(tsunami_data)
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

#From this plot we can infer that most values are concentrated at 0. But it might
#have a huge impact in tsunami prediction due to the nature of variables.
tsunami_data %>% count(tsunami) #We can see 2/3 of the times there was no tsunami, 
#so these parameters would indicate the absence of factors that determine a tsunami

#Here, oultiers can actually contribute to a models prediction, therefore only
#extreme values are deleted. Thinking instead about transforming data instead of 
# deleting it is the correct approach

tsunami_data %>% select(sig, nst, dmin, gap, depth) %>% stack()%>% 
  ggplot(aes(x = values)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~ ind, scales = "free") +  # separate y-axis for each variable
  labs(title = "Distribution of Variables", x = "Value", y = "Count") +
  theme_bw()

#After seeing the distributions we can determine that a logarithmic transformation of this variables
#is the best way to proceed. This observation completely matches with how magnitude is measured

tsunami_data = tsunami_data %>%
  mutate(across(c(sig, nst, gap, dmin, depth),
                ~ log10(.x + 1)))

#Now we can see the the transformation
tsunami_data %>% select(sig, nst, dmin, gap, depth) %>% stack()%>% 
  ggplot(aes(x = values)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~ ind, scales = "free") +  # separate y-axis for each variable
  labs(title = "Distribution of Variables", x = "Value", y = "Count") +
  theme_bw()

normalized_tsunami_data = tsunami_data %>%
  mutate(across(-tsunami,~ scale(.x)))

##Descriptive analysis (PCA)

#Since our data is already standarized: PCA measured by covariance.
#Since our data set contains multiple variables 


ggcorr(normalized_tsunami_data, label= T) # We cannot really exctract much information
 # from correlation matrix 


#Lets see if we can reduce dimensions with PCA. 
pca = prcomp(normalized_tsunami_data, scale=T) 
summary(pca)
fviz_screeplot(pca, addlabels = TRUE) #We see tha

#If we were to focus on a target variable (tsunami) PCA would be done without taking into account this
#variable so we would be able to see which has the greatest contributions

#But we want to explore which variables contribute the most to these principal compontents. We can focus mainly ont he first 5 or 4 components
#Since they concentrate more than 80% of the contribution

#Lets see what can we extract from this
barplot(pca$rotation[,1], las=2, col="darkblue")
fviz_contrib(pca, choice = "var", axes = 1)

# So you at most could look at a correlation between variables;
# for example if year contributes the most and then something like size contributes 
# if both go along the same direction. Then you could determine which samples
# (say animals) contribute more to this linear correlation and which diverge the
# most;

# Relationships between PCs get more complex as you go higher.
# 
# Focus on dimensionality reduction: use the first few PCs for visualization and interpretation.
# 
# Look at variable and observation contributions to understand the main patterns in the data.
# 
# Don’t overinterpret higher-order PCs unless they capture a significant portion of variance.


fviz_pca_var(pca, col.var = "contrib")
fviz_pca_biplot(pca, repel = TRUE)
#Try to see some correlations...


# Factor Analysis 

x.f <- factanal(tsunami_data, 3, rotation="none")
x.f


#Cite
#https://creativecommons.org/licenses/by/4.0/







