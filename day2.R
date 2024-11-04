case_data<-read.csv("data/training_case_data_long.csv")
skim(case_data)
str(case_data)
select(case_data,region) #to select 1 column
select(case_data, region,woreda,zone) #to select multiple columns
filter(case_data,region=="Afar" | region=="Oromia", count >1000)
#to  Keep rows where data type could be presumed or confirmed,%in%=match operator
filter(case_data, data_type %in% c("confirmed","presumed") )
study_woredas <- c("Adama Town", "Liban Jawi", "Mieso", "Wondo")
str(filter(case_data,woreda %in% study_woredas))
somali_presumed<-filter(case_data,region=="Somali",data_type=="presumed")
str(somali_presumed)
str(filter(case_data,count>2000))
str(filter(case_data,region !="Addis Ababa"))
sub_table1<-filter(case_data,region=="Amhara"|region=="Harari",year==2020,data_type=="confirmed")
sub_table2<-filter(case_data,region=="Amhara"|region=="Harari",year==2020,data_type=="presumed")
sub_table3<-filter(case_data, region !="Amhara"& region!="Harari", year==2020)
sub_table3 <- subset(case_data, year == 2020 & !(region %in% c("Amhara", "Harari")))
confirmed_over500<-filter(case_data,data_type=="confirmed",count>500)
presumed_over500<-filter(case_data,data_type=="presumed",count>500)
notpresumed_over500<-filter(case_data,data_type !="presumed" & count>500)
#Vector of workshop days
workshop_days <- c("2023-09-04", "2023-09-05", "2023-09-06", "2023-09-07","2023-09-08")
class(workshop_days) #this results in character
ymd(workshop_days) #to change the class to date
workshop_days<-ymd(workshop_days)
class(workshop_days)
year(workshop_days)
month(workshop_days)
month(workshop_days,label = T)
ymd(case_data$period)
case_data$period<-ymd(case_data$period) #period column to "date" class convert
class(case_data$period)
filter(case_data, year(period) == 2020) #why is it not working??-troubleshot
#why is it not working??-Troubleshot
#Question 8 in the 2nd PDF:-
case_data %>%
  filter(woreda == "Bambasi", year(period) == 2018, month(period) %in% 1:12)
#Question 9 in the 2nd PDF:-
case_data %>% filter(woreda=="Gudetu Kondole", year(period)==2020, month(period) %in% 4)
#Question 10 in the 2nd PDF:-
case_data %>% filter(woreda=="Bambasi",data_type=="confirmed",month(period)%in%9:11)
case_data_dates<-mutate(case_data,month=month(period))
case_data_dates<-mutate(case_data,month=month(period),month_name=month(period,label=T))
#Question 11 in the 2nd PDF:-
case_data_dates_dates<-mutate(case_data,quarter(period, with_year = F))
#Question 11 in the 2nd PDF:-
case_data_dates_dates<-mutate(case_data,last_2digits_of_the_year=format(period,"%y"))
#how to use intermediate steps to filter and select at the same time
case_data_oromia_confirmed<-filter(case_data,region=="Oromia",data_type=="confirmed")
case_data_oromia_woreda_months<-select(case_data_oromia_confirmed,woreda,period,count)
head(case_data_oromia_woreda_months)
#how to use nested functions to filter and select at the same time
case_data_oromia_woreda_months <- select(
  filter(case_data, region == "Oromia", data_type == "confirmed"),
  woreda, period, count)
#how to use a pipe function to filter and select at the same time
case_data %>%
  filter(region == "Oromia", data_type == "confirmed") %>%
  select(woreda, period, count)
#Question 13 in the 2nd PDF:-
head(case_data %>%
       filter(data_type=="confirmed",year(period)==2019,month(period)==1)%>%
       select(woreda,count))

oromia_presumed <- case_data %>%
  filter(region=="Oromia", data_type=="presumed")
sum(oromia_presumed$count,na.rm=T)
case_data %>%
  filter(data_type== "presumed") %>%
  group_by(region)
summarise(mean_presumed=mean(count, na.rm=T))


