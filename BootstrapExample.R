################################################################
####EfronBoot is a function which takes bootstrap samples   ####
#### from an input data vector and returns the bootstrap    ####
#### sample  means and bootstrap samples.                   ####
####                                                        ####
####  data:  vector of data (should contain no missing vals ####
####         and be quantitative
####  B:  number of bootstrap samples to take.  Default=100 ####
####                                                        ####
#### returns bootstrapMeans:  vector of bootstrap sample means
####         bootstrapSamples: matrix containing bootstrap  ####
####                           samples.  Each column is a   ####
####                           single sample.               ####
################################################################
EfronBoot <- function(data, B=100){

  #Function should check inputs are correct

  bootData <- matrix(NA, nrow=length(data), ncol=B) #matrix to fill with bootstrap samples
  for(b in 1:B){
    bootData[ , b] <- sample(data, size=length(data), replace=TRUE) #SRSWR from orig. data
  }

  meanBootData <- apply(bootData, 2, mean) #get mean from each bootstrap sample
  return(list(bootstrapMeans =meanBootData, bootstrapSamples=bootData))
}



################################################################
###Use EfronBoot to take 1000 bootstrap samples from our card data

set.seed(141414)                                        #need to do this to make replicable results
myData <- c(3,7,19, 4, 35, 20, 300)                     #data from our cards
myBootstrap <- EfronBoot(myData, B=1000)$bootstrapMeans #results of the bootstrap (means)

#plot the 1000 bootstrap sample means
boxplot(myBootstrap, main="1000 Bootstrap sample means from myData", ylab="sample mean")






################################################################
###Use EfronBoot to take 100 bootstrap samples from 2016/17 NBA salary data

library(RCurl)                                 #need this package to interpret https URL
URL1 <- getURL("https://raw.githubusercontent.com/elegacy/MA-223-Data/master/20160916_BBSalary.csv")
BBSalary <- read.csv(text = URL1, header=TRUE) #read the data from github


SalBoot <- EfronBoot(BBSalary$X2016.17)$bootstrapMeans                       #bootstrap the variable X2016.17
boxplot(SalBoot, main="100 Bootstrap sample means from 2016 BBall Salaries") #plot the bootstrap means







################################################################
###Use EfronBoot to take 100 bootstrap samples from Spam Email data -- number of capital letters

URL2 <- getURL("https://raw.githubusercontent.com/elegacy/MA-223-Data/master/20160916_SpamEmails.csv")
Emails <- read.csv(text=URL2, header=TRUE)  #read the data from github
SpamEmails <- Emails[Emails$Spam==1,]       #subset the spam email data


SpamBoot <- EfronBoot(SpamEmails$TotalCapitals)$bootstrapMeans            #bootstrap the variable TotalCapitals
boxplot(SpamBoot, main="100 Bootstrap sample means from Spam Email Caps") #plot the bootstrap means




################################################################
###Use EfronBoot to take 500 bootstrap samples from North Atlantic Storm data -- max windspeed
### -- kind of slow with 500

URL3 <- getURL("https://raw.githubusercontent.com/elegacy/MA-223-Data/master/20160916_NAStorms.txt")
Storms <- read.table(text=URL3, sep=" ", header=TRUE)

WindBoot <- EfronBoot(Storms$maxWS, B=50)$bootstrapMeans                        #bootstrap the variable maxWS
boxplot(WindBoot, main="50 Bootstrap sample means of Hurricane Max Wind Speed") #plot the bootstrap means




