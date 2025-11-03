install.packages("mclust")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("maps")
install.packages("rpart")
install.packages("factoextra")
install.packages("cluster")
install.packages("tidyr")
install.packages("tidyverse")

library("tidyr")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("maps")
library("rpart")
library(factoextra)
library(cluster)
library(mclust)
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

#From this plot we can infer that most values are concentrated at 0. But it might
#have a huge impact in tsunami prediction due to the nature of variables.
tsunami_data %>% count(tsunami) #We can see 2/3 of the times there was no tsunami, 
#so these parameters would indicate the absence of factors that determine a tsunami

#Here, oultiers can actually contribute to a models prediction, therefore only
#extreme values are deleted. Transforming the data instead of  deleting it
# is the correct approach

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



  
# Now we'll make a multiple linear regression model to check some relations.
# Since the target variable is binary this will not be extremely informative
#and later on we will have to use logistic regression in order to consider
# the output as probability.

multi.tsunami = lm(tsunami ~  cdi + mmi + sig + nst + dmin + gap + depth + latitude + longitude
                 + year, tsunami_data) 
multi.tsunami %>% summary()

#At plain sight from the summary, we can see that various variables are not
# statistically significant. This is not really insightful as a binary target
#variable is better handled by a logistic regression model. 

#At the moment, sig, nst, cdi, depth and year appear significant, let's check
# their plots

# sig

pred = predict(multi.tsunami)
qplot(tsunami_data$sig, pred, main="Prediction model")

# nst
pred = predict(multi.tsunami)
qplot(tsunami_data$nst, pred, main="Prediction model")

# cdi
pred = predict(multi.tsunami)
qplot(tsunami_data$cdi, pred, main="Prediction model")

#depth

pred = predict(multi.tsunami)
qplot(tsunami_data$depth, pred, main="Prediction model")

# year

pred = predict(multi.tsunami)
qplot(tsunami_data$year, pred, main="Prediction model")

#Surprisingly enough, out of all of them, only year shows a very apparent linear
#relation. 

#It is interesting to analyze latitude and longitude as a pair and not individually
#So we will plot two dimensional map (axis being latitude and longitude) where
#each plotted point will be colored depending on whether there was an earthquake
#or not. This is only preliminary and will be better analyzed by clustering
#Additionally we will make the plot with a worldmap as background for
#illustrative purposes.

world = map_data("world")

tsunami_data %>% select(latitude, longitude, tsunami) %>% 
  ggplot() +
  geom_polygon(data = world,
               aes(x = long, y = lat, group = group),
               fill = "gray90", color = "white") +
  geom_point(
    aes(x = longitude, y = latitude, color = factor(tsunami)),
    size = 2, alpha = 0.8
    ) +
  coord_fixed(1.3) +
  labs(title = "Earthquake-tsunami events", 
       x = "Longitude", 
       y = "Latitude") +
  theme_bw()

#It is clear to see where most points are concentrated, they are observations in
#tectonic plate boundaries or places of interest, however, evidently
# the relation is not linear, much less so for tsunamis. 

#With this we can observe how linear regression isn't appropriate for this
#dataset, and even 'insignificant' variables can hold much importance.
#I will proceed to attempt a logistic regression model
logistic.tsunami = tsunami_data %>% glm(tsunami ~  cdi + mmi + sig + nst + dmin + gap + 
                                          depth + latitude + longitude
                                        + year, data = ., family = binomial )
logistic.tsunami %>% summary()

#From the summary we can see the year, cdi, and nst are strong predictors
# meanwile, latitude and dmin are slightly weaker ones, almost negligible
# finally, the rest are entirely statistically unimportant.

#I will make a revised version of the model with only the significant variables
#I will manually discard latitude as we saw before in the map that the connection
#while important, is better analyzed later on by clustering.

revisedlogistic.tsunami = tsunami_data %>% glm(tsunami ~  cdi + nst + dmin 
                                               + year, data = ., family = binomial )
revisedlogisticsummary = revisedlogistic.tsunami %>% summary()


#Before extracting any information we will check how well our model explains the
#target variable. This is done by various formulas that produce a 'pseudo r^2'
# in this case we will use two, McFaddens and Cohen's versions

rdev = revisedlogisticsummary$deviance
nulldev = revisedlogisticsummary$null.deviance
mcf = 1 - ( rdev/nulldev )
cohen = (nulldev - rdev)/nulldev

#Both are identical, at a result of 0.4825, which means the model explains
#almost 50% of the deviance in tsunami probability, which is quite high for
#a binary model. 



#Now that we know our model explains a sizable amount of deviance,we proceed to check
#the mean squared error. 


# Predicted probabilities 
predicted_probs <- predict(revisedlogistic.tsunami, type = "response")

# Actual values
actual <- tsunami_data$tsunami

# Mean Squared Error
mse <- mean((actual - predicted_probs)^2)
mse

#this result indicates sizable predictive power, however this is a logistic model
#and not a linear model, so we should account for this by calculating a different
#measure, the log-loss. 

# Clip probabilities to avoid log(0)
predicted_probs <- pmin(pmax(predicted_probs, 1e-15), 1 - 1e-15)

# Compute log loss manually
log_loss <- -mean(actual * log(predicted_probs) + (1 - actual) * log(1 - predicted_probs))
log_loss

#the result shows strong predictive power.


#With all this done, now from the summary we can extract some information, 
#such as the effect that each
# of the predictors has on the probability of a tsunami ocurring for every
#one unit of the predictor increased. this is done by using the 'Estimate'
#column, however this column is expressed logarithmically, so we exponentiate it
#first, which will grant us the multiplicative change. For example, 0.5 in 'year' would mean
#that for every year, the odds of a tsunami halve. 

multiplicative = revisedlogisticsummary$coefficients[-1, "Estimate"]
multiplicative = multiplicative %>% exp()

multiplicative

#within this vector we can see what the probability is multiplied by whenever we
#increase those predictors by one unit (for example, one year)

#By analyzing these results, we can extract information.

# CDI


#the higher the cdi, the lower the odds of tsunami. CDI is community determined intensity
#how intense an earthquake was determined by those who noticed it. 

#From this we can see that tsunamis are more common when the perceived intensity
#of an earthquake is lower. This could be explained by the fact that tsunamis
#are a consequence of oceanic disruption. The higher the CDI, the more likely
#the earthquake happened near land (which is where people are), which would
#lower the chances of it causing a tsunami.


#NST

#NST refers to the number of seismic stations that detected the event.
#From the vector we can see that the odds are more than halved for each station
#that detected the event. This could be explained in similar fashion to the effect
#of CDI, the more stations that detect it, the higher the odds that the earthquake
#was near them, on land, rather than on the sea bed 

#DMIN

#dmin is the minimum distance between the earthquake's epicenter and a seismic station
#this predictor's statistical importance was low but was still included in this model.
#The results appear counterintuitive, the higher the 'dmin' (the further the earthquake)
#the lower the odds of a tsunami, which seems to contradict the prior established
#explanation. This could be related to position bias of seismic stations, an earthquake
#far in land can also be far away from seismic stations. 


#YEAR

#this refers to the year in which the events happen, the closer to the present day, the higher
#the odds of a tsunami. there are two explanations we have come up with
#1. These results are biased by the increasing detection measures, by measuring more events, the
#'odds' increase overtime as lesser events are taken into acount
#2. Over time, tectonic plate movements have higher odds of producing tsunami
#generating earthquakes. 


#Now that all of this has been done preliminarily, we refine our model to complete
#this stage by applying resampling techniques. In this case, K-fold cross validation
#to make sure there isn't overfitting and our model is artifically producing
#'strong' predictions. 

#number of folds

k = 10 

#divide the dataset

kfolds = sample(rep(1:k, length.out=nrow(tsunami_data)))

#set up the parameters

comb = expand.grid(maxdepth = c(seq(1, 5, 1)),
                   mins = c(seq(2, 40, 5)),
                   cp = c(seq(0.01, 1, 0.1)))

#for the accuracy and the log-loss
acc =c()
logloss = c()

#k fold loop

for (i in 1:nrow(comb)){
  
  kacc = c()
  klog = c()
  
  for (j in 1:k){
    train = tsunami_data[kfolds!=j, ]
    test = tsunami_data[kfolds == j,]
    
    model = rpart(tsunami~., data=train, method="class",
                  control = rpart.control(
                    cp = comb$cp[i],
                    minsplit = comb$mins[i],
                    maxdepth=comb$maxdepth[i]
                  ))
    pred=predict(model, newdata=test, type="class")
    kacc[j] = mean(pred==test$tsunami)
    
    # predict probabilities (for log loss)
    pred_prob = predict(model, newdata = test, type = "prob")[, 2]
    
    
    pred_prob = pmax(pmin(pred_prob, 1 - 1e-15), 1e-15)
    
    # log loss
    y = test$tsunami
    klog[j] = -mean(y * log(pred_prob) + (1 - y) * log(1 - pred_prob))
  }
  acc[i]= mean(kacc)
  logloss[i] = mean(klog)
}

#Highest accuracy

acc[which.max(acc)]
logloss[which.min(logloss)]

#parameters corresponding to highest accuracy and lowest log loss

best.comb = comb[which.max(acc),]
best.comb.logloss = comb[which.min(logloss), ]

best.comb
best.comb.logloss


#the best parameters for highest accuracy and lowest log loss do not match in
#maxdepth and mins. We will check the logloss for the parameters of highest
#accuracy

idx = which(comb$maxdepth == 5 & comb$mins == 2 & comb$cp == 0.01)
logloss[idx]

#it is slightly higher, however the difference is minimal. So we will remain
#with the parameters 5, 2, 0.01 whic hhave an accuracy of 0.9461 and logloss of
#0.2245, which means high accuracy and a lot of predictive power. 

#Now we will proceed to perform unsupervised learning, clustering, in order to group
#similar earthquakes and see if any trends appear.

#Starting with the simplest form, K means, which may be affected by the sparse
#location of latitude and longitude that we saw previously.

#We do not take tsunami into account while clustering as it's our target.
clusternormalized = normalized_tsunami_data


clusternormalized$tsunami = NULL


tsunaminot = tsunami_data
tsunaminot$tsunami = NULL

 #simple Kmeans
k = 10 # Your code here
fit = kmeans(clusternormalized, k, nstart=1000) # your code here
groups = fit$cluster
barplot(table(groups), col="darkblue")

#visualization of the values of the variables (scaled) for each cluster

centers=fit$centers
tidy = cbind(
  gather(as.data.frame(t(centers)), "cluster", "coor"),
  var=rep(colnames(centers, k)),
  size=rep(table(fit$cluster), each=ncol(centers))
)

tidy %>%
  ggplot(aes(x=var, y=coor, fill=var)) +
  geom_col() +
  facet_wrap(~cluster) +
  theme(axis.text.x = element_text(angle=45))




#before any conclusions we should be more rigurious and apply elbow method

clusterscaled=kmeans (clusternormalized,k,nstart=1000)
fviz_cluster(clusterscaled,data =clusternormalized,
             labelsize = 0,
             stand=FALSE)

fviz_cluster(clusterscaled,data =tsunaminot,
             labelsize = 0,
             stand=FALSE)

sum((clusternormalized-clusterscaled$centers[clusterscaled$cluster,])^2)
c(clusterscaled$tot.withinss,sum((clusternormalized-clusterscaled$centers[clusterscaled$cluster,])^2))
meanval=apply(clusternormalized,MARGIN = 2,mean)
c(clusterscaled$totss,sum(sweep(clusternormalized,MARGIN = 1,STATS = meanval,FUN = "-")^2))
c(clusterscaled$betweenss,sum(sweep(clusterscaled$centers[clusterscaled$cluster,],MARGIN = 1,STATS = meanval,FUN = "-")^2))

# Elbow method
ks = seq(2,10,1)

tot.withinss =ks
for(i in 1:length(ks)){
  clusterscaled=kmeans (clusternormalized,ks[i],nstart=10)
  tot.withinss[i]=clusterscaled$tot.withinss
}


elbow = data.frame(ncluster=ks,Intravariance=tot.withinss)
ggplot(data=elbow)+aes(x=ncluster,y=Intravariance)+geom_line()+
  labs(title="Elbow method")+xlab("Num. clusters")+
  ylab("Intra cluster variance")+geom_vline(xintercept=4)+
  geom_point(aes(x=ks[3],y=tot.withinss[3]),size=3,color='blue')+
  theme(text=element_text(size=12))

#from this we can see that 4 clusters is optimal. 

k = 4 # Your code here
fit = kmeans(clusternormalized, k, nstart=1000) # your code here
groups = fit$cluster
barplot(table(groups), col="darkblue")

# tsunami mean within each cluster to extract conclusions
tapply(normalized_tsunami_data$tsunami, groups, mean)

#there are two groups with high chances of tsunami and two groups with minimal
#chances. Once more we visualize for ease of use

centers=fit$centers
tidy = cbind(
  gather(as.data.frame(t(centers)), "cluster", "coor"),
  var=rep(colnames(centers, k)),
  size=rep(table(fit$cluster), each=ncol(centers))
)

tidy %>%
  ggplot(aes(x=var, y=coor, fill=var)) +
  geom_col() +
  facet_wrap(~cluster) +
  theme(axis.text.x = element_text(angle=45))

#High year and high minimum distance are associated with a very high chance of tsunami
#High year, magnitude, significance, and perceived intensity, are also associated
#with an almost 50% chance. This can seem counter intuitive at first, as more 'sensed'
#tsunamis are likelier to be closer to land, however most data-points
#are set in areas extremely close to the ocean, where more intense earthquakes
#can span both land sea. When analyzing individual relationship across all observations
#we reach the conclusion that higher cdi and significance lower the chance of tsunamis
#but when observing clusters, a wider prespective is achieved.


#On the contrary, the further back, the less tsunamis there are, and when minimum
#distance is above average and the number of stations that detected it is far above the
#average, tsunami odds plummets, which follows our previous predictions.
#This also backs up the conclusions drawn by the almost 50% chance cluster.
#In said cluster, the number of stations that detected it is almost exactly average
#which further supports the tsunamis ocurring between land and sea. 




d <- dist(clusternormalized, method="euclidean")
sil = silhouette(groups, d)
plot(sil, col=1:5, main="", border=NA)
summary(sil)

#the coefficients are somewhat low, but the current structure is the best we can
#achieve with kmeans.

#Evidently, which clusters have higher or lower chances change in every iteration
#but there is always this '2 and 2' division. And in general, the two 'tsunami likely'
#clusters have smaller coefficients than the two 'tsunami unlikely' ones, which
#could indicate similarity between them. 

#Since kmeans can be disbalanced, and positions are greatly concentrated as seen
#in the map previously, we will perform clustering with medioids. 

fit.pam = eclust(clusternormalized, FUNcluster="pam", stand = TRUE, k=5,
                 graph = F, nstart=1000)
fviz_cluster(fit.pam, geom = c("point", "text"),
             main = "Clusters with Medoids",
             ellipse.type = "norm",
             labelsize = 3,
             repel = TRUE)

#with five clusters there is significant overlap, we will attempt less.

fit.pam = eclust(clusternormalized, FUNcluster="pam", stand = TRUE, k=4,
                 graph = F, nstart=1000)
fviz_cluster(fit.pam, geom = c("point", "text"),
             main = "Clusters with Medoids",
             ellipse.type = "norm",
             labelsize = 3,
             repel = TRUE)

#also some overlap
fit.pam = eclust(clusternormalized, FUNcluster="pam", stand = TRUE, k=3,
                 graph = F, nstart=1000)
fviz_cluster(fit.pam, geom = c("point", "text"),
             main = "Clusters with Medoids",
             ellipse.type = "norm",
             labelsize = 3,
             repel = TRUE)



#Cite
#https://creativecommons.org/licenses/by/4.0/
#Cohen, Jacob; Cohen, Patricia; West, Steven G.; Aiken, Leona S. (2002). 
#Applied Multiple Regression/Correlation Analysis for the Behavioral Sciences (3rd ed.). 
#Routledge. p. 502.

#Hardin, J. W., Hilbe, J. M. (2007). Generalized linear models and extensions. 
#USA: Taylor & Francis. Page 60, Google Books







