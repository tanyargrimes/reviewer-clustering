##################################################
#                                                #
# K-Means Clustering                             #
#                                                #
# Written by Tanya Grimes                        #
#                                                #
##################################################



#-------------------------------------------------------------------
# Clean Slate & Environment Initialization                                  

# if a list of plots exists, clears plots
if(!is.null(dev.list())) dev.off()

# clears entire console
cat("\014")

# clears all objects in the current workspace and global environment window
rm(list=ls())

# sets the current working directory for any files read from or saved to
setwd("C:/Users/CreateDirectory")

# prevents use of exponential notation until the numeric value width
# is greater than the scipen limit specified
options(scipen=9)

# controls the number of digits to print. Default is 7.
options(digits=5)



#-------------------------------------------------------------------
# Package Installation & Load

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")



#-------------------------------------------------------------------
# Read in Reviews data

reviews_data_TG <- read.csv("Reviews.csv", header = TRUE, sep = ",")

# get initial look at data
head(reviews_data_TG)

# get structure of initial data set to confirm data is there
str(reviews_data_TG)



#-------------------------------------------------------------------
# Assignment Task 1 - Data Transformation


# drop unnecessary identifiers
reviews_data_TG <- reviews_data_TG[-c(1)]

str(reviews_data_TG)

# rename variables with _TG suffix
names(reviews_data_TG) <- c("Spt_TG", "Rel_TG", "Nat_TG", "Thr_TG",
                            "Shp_TG", "Pcc_TG", "Age_TG", "Inc_TG", "Nbr_TG")


str(reviews_data_TG)

# summary to get an idea of the dispersion of data
summary(reviews_data_TG)
#stat.desc(reviews_data_TG)


# creates standardization function
# using N(0,1) because data ranges from variables that are tightly clustered 
# to more dispersed even though there are no outliers present
stdnrm_TG <- function(x) {
  return ((x-mean(x))/sd(x))
}

# to store normalized data
reviews_ndata_TG <- reviews_data_TG

reviews_ndata_TG$Spt_n_TG <- stdnrm_TG(reviews_data_TG$Spt_TG)
reviews_ndata_TG$Rel_n_TG <- stdnrm_TG(reviews_data_TG$Rel_TG)
reviews_ndata_TG$Nat_n_TG <- stdnrm_TG(reviews_data_TG$Nat_TG)
reviews_ndata_TG$Thr_n_TG <- stdnrm_TG(reviews_data_TG$Thr_TG)
reviews_ndata_TG$Shp_n_TG <- stdnrm_TG(reviews_data_TG$Shp_TG)
reviews_ndata_TG$Pcc_n_TG <- stdnrm_TG(reviews_data_TG$Pcc_TG)
reviews_ndata_TG$Age_n_TG <- stdnrm_TG(reviews_data_TG$Age_TG)
reviews_ndata_TG$Inc_n_TG <- stdnrm_TG(reviews_data_TG$Inc_TG)
reviews_ndata_TG$Nbr_n_TG <- stdnrm_TG(reviews_data_TG$Nbr_TG)

str(reviews_ndata_TG)

# drop unnecessary variables
reviews_ndata_TG <- reviews_ndata_TG[-c(1:9)]

str(reviews_ndata_TG)



#-------------------------------------------------------------------
# Assignment Task 2 - Descriptive Data Analysis


# summary of original variables
summary(reviews_data_TG)

# summary of transformed variables
summary(reviews_ndata_TG)


# pie chart of review categories
# generate table of summaries for only the percentage variables
prct_means_TG <- round(sapply(reviews_data_TG[, 1:6], mean),2)* 100

# generate list of labels
prct_labels_TG <- paste(names(prct_means_TG), "\n", prct_means_TG, sep="")

# generate a pie chart of the percentages of the review categories
pie(prct_means_TG, labels = prct_labels_TG, main="Review Categories by Mean Percentage")


# histograms of review categories
# generate histogram of reviewers by age
histogram( ~ Age_TG, data = reviews_data_TG, breaks = 10, type = "count",
           main = "Distribution of Reviewer Ages")

# generate histogram of reviewers by income
histogram( ~ Inc_TG, data = reviews_data_TG, breaks = 10, type = "count",
           main = "Distribution of Reviewer Income")

# generate histogram of reviewers by income
histogram( ~ Nbr_TG, data = reviews_data_TG, breaks = 10, type = "count",
           main = "Distribution of Number of Reviews")

# generate histogram of reviewers by income
histogram( ~ Spt_TG, data = reviews_data_TG, breaks = 5, type = "count",
           main = "Distribution of Sport Reviews")

# generate histogram of reviewers by income
histogram( ~ Rel_TG, data = reviews_data_TG, breaks = 5, type = "count",
           main = "Distribution of Religious Reviews")



#-------------------------------------------------------------------
# Assignment Task 3 -  Clustering


# prepare dataset for clustering, choosing 2 centroid variables
# one for storing cluster variables
reviews_spt_rel_n_TG <- reviews_ndata_TG[c('Spt_n_TG','Rel_n_TG')]
# one only containing the centroid variables
cluster_spt_rel_n_TG <- reviews_ndata_TG[c('Spt_n_TG','Rel_n_TG')]



# Creating clusters 2 to 6 inclusive

# create cluster 2
cluster2_TG <- kmeans(cluster_spt_rel_n_TG, iter.max=10, centers=2, nstart=10)

# add cluster tags to variables
reviews_spt_rel_n_TG$Cls_2_TG <- factor(cluster2_TG$cluster)

# generate dataframe of centers by factor
centers2_TG <- data.frame(cluster=factor(1:2), cluster2_TG$centers)

# rename default cluster column to match reviews_spt_rel_n_TG column for plotting
names(centers2_TG)[names(centers2_TG) == 'cluster'] <- 'Cls_2_TG'



# create cluster 3
cluster3_TG <- kmeans(cluster_spt_rel_n_TG, iter.max=10, centers=3, nstart=10)

# add cluster tags to variables
reviews_spt_rel_n_TG$Cls_3_TG <- factor(cluster3_TG$cluster)

# generate dataframe of clusters by factor
centers3_TG <- data.frame(cluster=factor(1:3), cluster3_TG$centers)

# rename default cluster column to match reviews_spt_rel_n_TG column for plotting
names(centers3_TG)[names(centers3_TG) == 'cluster'] <- 'Cls_3_TG'



# create cluster 4
cluster4_TG <- kmeans(cluster_spt_rel_n_TG, iter.max=10, centers=4, nstart=10)

# add cluster tags to variables
reviews_spt_rel_n_TG$Cls_4_TG <- factor(cluster4_TG$cluster)

# generate dataframe of clusters by factor
centers4_TG <- data.frame(cluster=factor(1:4), cluster4_TG$centers)

# rename default cluster column to match reviews_spt_rel_n_TG column for plotting
names(centers4_TG)[names(centers4_TG) == 'cluster'] <- 'Cls_4_TG'



# create cluster 5
cluster5_TG <- kmeans(cluster_spt_rel_n_TG, iter.max=10, centers=5, nstart=10)

# add cluster tags to variables
reviews_spt_rel_n_TG$Cls_5_TG <- factor(cluster5_TG$cluster)

# generate dataframe of clusters by factor
centers5_TG <- data.frame(cluster=factor(1:5), cluster5_TG$centers)

# rename default cluster column to match reviews_spt_rel_n_TG column for plotting
names(centers5_TG)[names(centers5_TG) == 'cluster'] <- 'Cls_5_TG'



# create cluster 6
cluster6_TG <- kmeans(cluster_spt_rel_n_TG, iter.max=10, centers=6, nstart=10)

# add cluster tags to variables
reviews_spt_rel_n_TG$Cls_6_TG <- factor(cluster6_TG$cluster)

# generate dataframe of clusters by factor
centers6_TG <- data.frame(cluster=factor(1:6), cluster6_TG$centers)

# rename default cluster column to match reviews_spt_rel_n_TG column for plotting
names(centers6_TG)[names(centers6_TG) == 'cluster'] <- 'Cls_6_TG'



str(reviews_spt_rel_n_TG)


# All clusters
cluster2_TG
cluster3_TG
cluster4_TG
cluster5_TG
cluster6_TG



#-------------------------------------------------------------------
# Assignment Task 4 -  Evaluation of Clusters

# K4 selected using elbow method

# K - 1: K3 plot
ggplot(data=reviews_spt_rel_n_TG, aes(x=Spt_n_TG, y=Rel_n_TG, color=Cls_3_TG, shape=Cls_3_TG)) +
  geom_point(alpha=.8) + ggtitle("K3 Cluster Plot") +
  geom_point(data=centers3_TG, aes(x=Spt_n_TG, y=Rel_n_TG), size=3, stroke=2)


# K: K4 plot
ggplot(data=reviews_spt_rel_n_TG, aes(x=Spt_n_TG, y=Rel_n_TG, color=Cls_4_TG, shape=Cls_4_TG)) +
  geom_point(alpha=.8) + ggtitle("K4 Cluster Plot") +
  geom_point(data=centers4_TG, aes(x=Spt_n_TG, y=Rel_n_TG), size=3, stroke=2)


# K + 1: K5 plot
ggplot(data=reviews_spt_rel_n_TG, aes(x=Spt_n_TG, y=Rel_n_TG, color=Cls_5_TG, shape=Cls_5_TG)) +
  geom_point(alpha=.8) + ggtitle("K5 Cluster Plot") +
  geom_point(data=centers5_TG, aes(x=Spt_n_TG, y=Rel_n_TG), size=3, stroke=2)



# Add selected cluster to reviews to generate a summary table
reviews_data_TG$Ck4_TG <- factor(reviews_spt_rel_n_TG$Cls_4_TG)
head(reviews_data_TG)


# Summary table of clustering scheme
summary_reviews_TG <- reviews_data_TG %>% 
  group_by(Ck4_TG) %>% 
  summarise(Sptm_TG = mean(Spt_TG), Relm_TG = mean(Rel_TG), Natm_TG = mean(Nat_TG), 
            Thrm_TG=mean(Thr_TG), Shpm_TG=mean(Shp_TG),Pccm_TG=mean(Pcc_TG),
            Agem_TG=mean(Age_TG), Incm_TG=mean(Inc_TG), Nbrm_TG=mean(Nbr_TG), Nobs_TG=n())

summary_reviews_TG




#-------------------------------------------------
# Package Unload

# uncomment to unload packages
# detach(package:dplyr)
# detach(package:pastecs)
# detach(package:ggplot2)

