library("readr")
library("tidyverse")
library("dplyr")
library("data.table")
##2016 and 2017 have common months january,june,august,septemebr,october,november and december
jan2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_january_2014.csv', col_names = TRUE)
jun2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_june_2014.csv', col_names = TRUE)
aug2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_august_2014.csv', col_names = TRUE)
sep2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_september_2014.csv', col_names = TRUE)
oct2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_october_2014.csv', col_names = TRUE)
dec2014=read_csv('/home/noel/r_projects/2014/principal_offence_category_december_2014.csv', col_names = TRUE)
jan2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_january_2015.csv', col_names = TRUE)
jun2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_june_2015.csv', col_names = TRUE)
aug2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_august_2015.csv', col_names = TRUE)
sep2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_september_2015.csv', col_names = TRUE)
oct2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_october_2015.csv', col_names = TRUE)
dec2015=read_csv('/home/noel/r_projects/2015/principal_offence_category_december_2015.csv', col_names = TRUE)
jan2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_january_2016.csv', col_names = TRUE)
jun2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_june_2016.csv', col_names = TRUE)
aug2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_august_2016.csv', col_names = TRUE)
sep2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_september_2016.csv', col_names = TRUE)
oct2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_october_2016.csv', col_names = TRUE)
dec2016=read_csv('/home/noel/r_projects/2016/principal_offence_category_december_2016.csv', col_names = TRUE)
jan2017=read_csv('/home/noel/r_projects/2017/principal_offence_category_january_2017.csv', col_names = TRUE)
jun2017=read_csv('/home/noel/r_projects/2017/Principal_Offence_Category_Jul.csv', col_names = TRUE)
aug2017=read_csv('/home/noel/r_projects/2017/Principal_Offence_Category_Aug.csv', col_names = TRUE)
sep2017=read_csv('/home/noel/r_projects/2017/Principal_Offence_Category_Sep.csv', col_names = TRUE)
oct2017=read_csv('/home/noel/r_projects/2017/Principal_Offence_Category_Oct.csv', col_names = TRUE)
dec2017=read_csv('/home/noel/r_projects/2017/Principal_Offence_Category_Dec.csv', col_names = TRUE)

##adding year and month for further analysis
jan2014['month'] <- 'jan'
jan2014['year'] <- '2014'
jun2014['month'] <- 'jun'
jun2014['year'] <- '2014'
aug2014['month'] <- 'aug'
aug2014['year'] <- '2014'
sep2014['month'] <- 'sep'
sep2014['year'] <- '2014'
oct2014['month'] <- 'oct'
oct2014['year'] <- '2014'
dec2014['month'] <- 'dec'
dec2014['year'] <- '2014'
jan2015['month'] <- 'jan'
jan2015['year'] <- '2015'
jun2015['month'] <- 'jun'
jun2015['year'] <- '2015'
aug2015['month'] <- 'aug'
aug2015['year'] <- '2015'
sep2015['month'] <- 'sep'
sep2015['year'] <- '2015'
oct2015['month'] <- 'oct'
oct2015['year'] <- '2015'
dec2015['month'] <- 'dec'
dec2015['year'] <- '2015'
jan2016['month'] <- 'jan'
jan2016['year'] <- '2016'
jun2016['month'] <- 'jun'
jun2016['year'] <- '2016'
aug2016['month'] <- 'aug'
aug2016['year'] <- '2016'
sep2016['month'] <- 'sep'
sep2016['year'] <- '2016'
oct2016['month'] <- 'oct'
oct2016['year'] <- '2016'
dec2016['month'] <- 'dec'
dec2016['year'] <- '2016'
jan2017['month'] <- 'jan'
jan2017['year'] <- '2017'
jun2017['month'] <- 'jun'
jun2017['year'] <- '2017'
aug2017['month'] <- 'aug'
aug2017['year'] <- '2017'
sep2017['month'] <- 'sep'
sep2017['year'] <- '2017'
oct2017['month'] <- 'oct'
oct2017['year'] <- '2017'
dec2017['month'] <- 'dec'
dec2017['year'] <- '2017'
## end adding year and month
## binding all data sets
datasheet2014 <- rbind(jan2014,jun2014,aug2014,sep2014,oct2014,dec2014)
datasheet2015 <- rbind(jan2015,jun2015,aug2015,sep2015,oct2015,dec2015)
datasheet2016 <- rbind(jan2016,jun2016,aug2016,sep2016,oct2016,dec2016)
datasheet2017 <- rbind(jan2017,jun2017,aug2017,sep2017,oct2017,dec2017)
datasheet <- rbind(datasheet2014,datasheet2015,datasheet2016,datasheet2017)
#view(datasheet)
##end of binding all data sets
##data cleansing to analyse
class(datasheet)
dim(datasheet)
summary(datasheet)
names(datasheet)
datasheet <- datasheet %>% rename(cities = `...1`,hom_convic = `Number of Homicide Convictions`,
                                  hom_unsuc= `Number of Homicide Unsuccessful`,
                                  off_convic = `Number of Offences Against The Person Convictions`,
                                  off_unsuc = `Number of Offences Against The Person Unsuccessful`,
                                  sex_convic = `Number of Sexual Offences Convictions`,
                                  sex_unsuc = `Number of Sexual Offences Unsuccessful`,
                                  bur_convic = `Number of Burglary Convictions`,
                                  bur_unsuc = `Number of Burglary Unsuccessful`,
                                  rob_convic  = `Number of Robbery Convictions`,
                                  rob_unsuc = `Number of Robbery Unsuccessful`,
                                  the_convic  = `Number of Theft And Handling Convictions`,
                                  the_unsuc  = `Number of Theft And Handling Unsuccessful`,
                                  fra_convic  =`Number of Fraud And Forgery Convictions`,
                                  fra_unsuc  = `Number of Fraud And Forgery Unsuccessful`,
                                  cri_convic  =`Number of Criminal Damage Convictions`,
                                  cri_unsuc=  `Number of Criminal Damage Unsuccessful`,
                                  drug_convic =`Number of Drugs Offences Convictions`,
                                  drug_unsuc = `Number of Drugs Offences Unsuccessful`,
                                  pub_convic =`Number of Public Order Offences Convictions`,
                                  pub_unsuc  =`Number of Public Order Offences Unsuccessful`,
                                  other_convic  =`Number of All Other Offences (excluding Motoring) Convictions`,
                                  other_unsuc= `Number of All Other Offences (excluding Motoring) Unsuccessful`,
                                  mon_convic= `Number of Motoring Offences Convictions`,
                                  mon_unsuc = `Number of Motoring Offences Unsuccessful`
                                  
)
datasheet_new=datasheet %>%
  select( month,year,cities,
          hom_convic,hom_unsuc,off_convic,off_unsuc,sex_convic,sex_unsuc,bur_convic,bur_unsuc,
          rob_convic,rob_unsuc,the_convic,the_unsuc,fra_convic,fra_unsuc,cri_convic,cri_unsuc,
          drug_convic,drug_unsuc,pub_convic,pub_unsuc,other_convic,other_unsuc,mon_convic,mon_unsuc
  )
str(datasheet_new)
hist(datasheet_new$off_convic)
boxplot(datasheet_new$off_convic)
any(is.na(datasheet_new))
#end of data cleansing
#view(datasheet_new)
#view numerical attributes from summary
library(skimr)
skim(datasheet_new)
# visualize the data type and missing data
library(devtools)
devtools::install_github("ropensci/visdat")
library(visdat)
vis_miss(datasheet_new)
vis_dat(datasheet_new)
library(DataExplorer)
DataExplorer::create_report(datasheet_new)
library(inspectdf)
inspect_types(datasheet_new) %>% show_plot()
#hypotheis 1
offmin <- datasheet_new %>% group_by(year) %>% summarise(mean_offense=mean(off_convic),max_offense=max(off_convic))
#view(offmin)
mean_offences_conviction_graph <- ggplot(offmin, aes(x = year, y = `mean_offense`))+ geom_bar(stat = "identity", fill= "#33acff")
mean_offences_conviction_graph
max_offences_conviction_graph <- ggplot(offmin, aes(x = year, y = `max_offense`))+ geom_bar(stat = "identity", fill= "#4fff33")
max_offences_conviction_graph
#end of hypotheis 1
# hypotheis 2
allData <- datasheet_new
#view(allData)
# Viewing the unique values in the "Month" variable===
unique(allData$month)
class(allData$month)
allData2 <- allData %>%
  mutate(month = factor(month, levels = c("jan","feb","mar","apr","may","jun","aug","sep","oct","dec")))
# convictions month wise===
homicide_convictions_graph <- ggplot(allData2, aes(x = month, y = `hom_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
offences_convictions_graph <- ggplot(allData2, aes(x = month, y = `off_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
sextual_convictions_graph <- ggplot(allData2, aes(x = month, y = `sex_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
burglary_conviction_graph <- ggplot(allData2, aes(x = month, y = `bur_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
robbery_conviction_graph <- ggplot(allData2, aes(x = month, y = `rob_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
theft_conviction_graph <- ggplot(allData2, aes(x = month, y = `the_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
fraud_conviction_graph <- ggplot(allData2, aes(x = month, y = `fra_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
criminal_conviction_graph <- ggplot(allData2, aes(x = month, y = `cri_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
drug_conviction_graph <- ggplot(allData2, aes(x = month, y = `drug_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
public_offence_convic_graph <- ggplot(allData2, aes(x = month, y = `pub_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
other_conviction_graph <- ggplot(allData2, aes(x = month, y = `other_convic`))+ geom_bar(stat = "identity", fill= "#693ead")
monitor_conviction_graph <- ggplot(allData2, aes(x = month, y = `mon_convic`))+ geom_bar(stat = "identity", fill= "#693ead")

#Viewing  graph===
homicide_convictions_graph 
sextual_convictions_graph 
burglary_conviction_graph 
robbery_conviction_graph 
theft_conviction_graph 
fraud_conviction_graph 
criminal_conviction_graph 
drug_conviction_graph 
public_offence_convic_graph 
other_conviction_graph 
monitor_conviction_graph 
#end of hypotheis 2

#correlation
library(Hmisc) 
library(corrplot)
# Compute covariance and correlation matrix
data_matrix<-as.matrix(allData2[,4:26])
#view(data_matrix)
rescov = cov(data_matrix, method = "pearson")
rescov
res <- cor(data_matrix)
res
round(res, 2)
res
# Correlation matrix with significance levels (p-value)
rcorr(res, type = "pearson")
res2 <- rcorr(as.matrix(data_matrix))
res
# Correlation matrix
res2
# Significance Level
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P

# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor =(cormat)[ut],
    p = pmat[ut]
  )
}
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = res, col = col, symm = TRUE)
#clusturing
library(ggplot2)
#install.packages("factoextra")
library(ggpubr)
library(factoextra)
# Compute k-means with k = 3
set.seed(123)
res.km2 <- kmeans(scale(data_matrix), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km2$cluster
# Plot cluster The func fviz_cluster() [factoextra package] can be used to easily visualize
#k-means clusters. It takes k-means results and the original data as arguments.
fviz_cluster(res.km2, data = data_matrix,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"),
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw()
)
#Regression Analysis
library(broom)
#linear regression between offensive convictions and unsuccessful
lmoffenses = lm(off_convic~off_unsuc, data = allData2) #Create the linear regression
summary(lmoffenses) #Review the results
offenses.graph<-ggplot(allData2, aes(x=off_convic, y=off_unsuc))+ geom_point()
offenses.graph <- offenses.graph + geom_smooth(method="lm", col="#ff6e33")
offenses.graph <-
  offenses.graph +
  theme_bw() +
  labs(title = "Offences in UK",
       x = "Offensive Convictions",
       y = "Offensive Unsuccessful")
offenses.graph
#decision tree
library(tree)
library(ISLR)
suppressWarnings(data_matrix) 
data <- allData2
str(data)
Salecat <- ifelse(data$off_convic<=600,"No","Yes")
data <- data.frame(data,Salecat)
sale.tree <- tree(Salecat~.-off_convic,data = data)
summary(sale.tree)
plot(sale.tree)
text(sale.tree, pretty=0)

set.seed(40)
tree.train <- sample(1:nrow(data),250)
sale.tree <- tree(Salecat~.-off_convic,data,subset=tree.train) 
plot(sale.tree)
text(sale.tree, pretty=0)

sale.pred = predict(sale.tree, data[-tree.train,], type="class")
with(data[-tree.train,], table(sale.pred, Salecat))

sale.cv = cv.tree(sale.tree, FUN = prune.misclass)
sale.cv
plot(sale.cv)

sale.prune = prune.misclass(sale.tree, best = 3)
plot(sale.prune)
text(sale.prune, pretty=0)

