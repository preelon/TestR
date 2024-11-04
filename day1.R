20*10.5
17+23-7
sqrt(25)
?sqrt
#here I am using the formula for the area of a circle pi * r^2
area_of_a_circle<-pi*4^2
my_first_variable<-20
my_first_variable
malaria_prevalence_2020<-2/10
ls() #to list all the variables i created
my_first_variable<-20
my_first_string<-"juju"
my_first_variable * my_first_variable
class(my_first_variable)
str(my_first_string)
typeof(my_first_variable)
str(my_first_variable)
v1<-c(1,2,3,4,5)
v2<-c(0.1,0.15,0.2,0.4,0.5)
v3<-c("red","blue","green","black","orange")
v1[2]
v3[4]
v2[6] #looking for an element that doesn't exist returns NA
v1[0] #looking for the 0th value returns 0 with the type of the vector
v3[0]
sum(v1[c(1,4)]) #we use this to some only few elements in the vector
length(v1)
plot(v2,v1)
plot(v1)
v1*v2
v1*v3
v4<-rep(0,14) #this is to mean repete "0" 14 times
v4[2:14]<-c(11:23) #this is to command to replace the 2nd-14th elements in vector 4 by numbers 11 to 23
v5<-c(101:200)
?co2
data("co2")
head(CO2)
dim(CO2) #to see how many rows and columns the data frame has
#it returns 84  5  (84 is the row and 5 is the column numbers)

CO2[3,2] #to command to find the value on the 3rd row and 2nd column 
CO2[6,]
CO2[,3] 
CO2_op2<-CO2[,2:3] #to creat a sub table containing all rows but only the 2nd and 3rd columns
head(CO2_op2)
str(CO2)
summary(CO2)
CO2$Treatment #this and the below gives similar result
CO2[,3]
table(CO2$Treatment) #to request a table summary of the treatment column
CO2[14,5] #the value in 14th row and 5th column
CO2[1:7,4] #the values in the 1st to 7th row and 4th column
table(CO2$Type) #i wanted to know how many of the plants were from Quebec
mean(CO2$uptake)
?range
range(CO2$uptake)
median(CO2$uptake)
mean(CO2$uptake[which(CO2$Type=="Quebec")]) #the traditional approach of 
#calculating the mean of uptakes from Quebec only
#the tidyverse version of doing the above is as follow
op1=filter(CO2,Type =="Quebec")
mean(op1$uptake) #Type object not found silemil enji yiseral, why endeza maletu
chilled_missisipi<-filter(CO2,Type=="Mississippi", Treatment=="chilled")
names(wes_palettes)
cols = wes_palette("GrandBudapest1")
cols
cols2<-wes_palette("Rushmore1")
cols2
say("my favorite person in this world is juju", by="cow")
#"janitor::" to see all the functions available in janitor package, don't use the cotation when using

dat <- read_csv("data/training_case_data_wide.csv")  
head(dat)
options(dplyr.width=Inf) #use this to remedy some variable not shown as a result of screen size
abobo_conf_2021<-filter(dat,woreda=="Ababo",year==2021)
sum(abobo_conf_2021$confirmed,na.rm = TRUE)
sum(abobo_conf_2021$presumed,na.rm = T)
Hudet_confirmed_2020<-filter(dat,woreda=="Hudet",year==2020)
Hudet_presumed_2020<-filter(dat,woreda=="Hudet",year==2020)
sum(Hudet_confirmed_2020$confirmed,na.rm = TRUE)
sum(Hudet_presumed_2020$presumed,na.rm = TRUE)
389+445
Awra_woreda<-filter(dat,woreda=="Awra",year==2020)
sum(Awra_woreda$confirmed,na.rm = TRUE)
sum(Awra_woreda$presumed,na.rm = TRUE)
hulet_ej<-filter(dat,woreda=="Hulet Ej Enese",year==2021)
takusa<-filter(dat,woreda=="Takusa",year==2021)
sum(hulet_ej$confirmed)
sum(takusa$confirmed)
