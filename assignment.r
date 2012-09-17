#############################
########Assignment 2#########
#############################

#Starting with R

setwd("C:/Users/Emanuela/Desktop/SequenceCourse/Lezione 2_11092012")
library (TraMineR)

#1. Load the biofam data set that comes with the TraMineR library.
data(biofam)

#2. Print the variable names.
names(biofam)

#3. Create an age variable by subtracting the birth year from the year of the survey 
#and add it to the biofam data frame.
biofam<-cbind(biofam,2000-biofam$birthyr)
names(biofam)<-c(names(biofam)[1:dim(biofam)[2]-1],"age")

#4. What is the minimum, maximum, median and mean age in the sample?
summary(biofam$age)

#5. What is the minimum, maximum, median and mean age of the women?
summary(biofam[biofam$sex=="woman",]$age)

#6. Add a cohort factor to the biofam data frame grouping the birth years into the
#following categories: 1900-1929, 1930-1939, 1940-1949, 1950-1959.
biofam<-cbind(biofam,cut(biofam$birthyr,c(1900,1930,1940,1950,1959),labels=c("1900-29","1930-39","1940-49","1950-59"),right=F))
names(biofam)<-c(names(biofam)[1:dim(biofam)[2]-1],"coho")

#####PROBLEM with N.7###########

#7. Generate an histogram of the distribution of birthyear using the above birth year
#classes. (Look at the help of the hist function for how to do that.)

breaks<-c(1900,1930,1940,1950,1959)
hist(biofam$birthyr,breaks=(c(biofam$coho) min(biofam$coho)))
hist(biofam$birthyr,breaks=(c(by(biofam$birthyr,biofam$coho,min),max(biofam$coho==1950-59))))
hist(biofam$birthyr,breaks=(c(by(biofam$birthyr,biofam$cohort,min),max(biofam$birthyr))))

###############################

#8. Produce a frequency table of the cohort factor.
table (biofam$coho)

#9. Cross tabulate the cohort with the state at 25 years old.
?biofam
table (biofam$coho,biofam$a25) 

#10. Fit a logistic regression for the probability to be married with a child and having
#left home at 25 years old in terms of the language of the questionnaire and the sex.
#Comment the results.
lg.gr<-glm(a25==6~plingu02+sex, family=binomial, data=biofam)
summary (lg.gr)

#11. Fit the same logistic regression, but for the youngest cohort only.
lg.gr2<-glm(a25==6~plingu02+sex, family=binomial, data=biofam[biofam$coho=="1950-59",])
summary (lg.gr2)
