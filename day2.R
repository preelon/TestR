library(tidyverse)
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
  group_by(region) %>% summarise(mean_presumed=mean(count, na.rm=T))

case_data %>%
  filter(data_type == "presumed") %>%
  group_by(region, woreda, year) %>%
  summarise(mean_presumed_per_month = mean(count, na.rm = TRUE),
            total_presumed_per_month = sum(count, na.rm = TRUE))
case_data %>%
  filter(data_type=="presumed",
         year(period)==2019)%>%
  group_by(woreda, period)%>%
  summarise(total_presumed_per_month=sum(count))%>%
  arrange(total_presumed_per_month)
case_data %>%
  filter(data_type=="presumed",
         year(period)==2019) %>%
  group_by(woreda,period)%>%
  summarise(total_presumed_per_month=sum(count))%>%
  arrange(desc(total_presumed_per_month)) #to arrange in a descending order, the default is in ascending order
#PDF2, Question 14
case_data%>%
  filter(data_type=="confirmed",year(period)==2019)%>%
  group_by(region)%>%
summarise(total_confirmed_2019=sum(count,na.rm = T))
#PDF2, Question 15
case_data %>%
  group_by(region,month(period,label=T), year(period))%>%
  summarise(total_cases=sum(count,na.rm = T))
#PDF2, Question 16
case_data %>%
  filter(region=="Somali",year(period)==2020,
         month(period)%in% 9:12|month(period) %in%1:4)%>%
  group_by(woreda,year(period),month_name=month(period,label=T))%>%
  summarise(sum(count,na.rm = T))
case_data %>%
  filter(woreda=="Awabel",period==ymd("2020-01-01"))

wide_data<-case_data %>%
  filter(woreda=="Awabel",period==ymd("2020-01-01"))%>%
  pivot_wider(names_from = data_type,values_from = count)
#how to create longer data format from wider format
wide_data%>%
  pivot_longer(names_to = "data_type", values_to = "count", 
               cols =(c("presumed","confirmed")))
case_data %>%
  filter(data_type=="confirmed")%>%
table(year(period),region)%>%
  pivot_wider(names_from = region, values_from = count)
wide_table_Q19 <- case_data %>%
  filter(data_type=="confirmed")%>%
  group_by(region,year(period))%>%
  pivot_wider(names_from = region, values_from = count, values_fill = list(count = 0))

    #practice with Amir
#1.confirmed cases betwen 2019 and 2023:monthly filtered
confirmed_2019_2023<-case_data %>%
  filter(data_type=="confirmed",year(period)%in%2019:2023)%>%
  group_by(period)%>%
  summarise(total_confirmed= sum(count,na.rm = T))
 
#2.visualize bar chart
confirmed_2019_2023 %>%
  ggplot(aes(x=period,y=total_confirmed)) + 
  geom_col()

#3.confirmed cases sep 2021
confirmed_sep_2021<-case_data %>%
  filter(data_type=="confirmed",month(period)==9, year(period)==2021) %>%
  group_by(region,zone, woreda) %>%
  summarise(total_confirmed=sum(count,na.rm = T))

#4.visualization sep 2021 histo
confirmed_sep_2021 %>%
  ggplot(aes(x=total_confirmed)) +
  geom_histogram() 
  
#5.confirmed cases between 2019 and 2023:yearly
case_data %>%
  filter(data_type=="confirmed", year(period) %in% 2019:2023) %>%
  group_by(year=year(period)) %>%
  summarise(total_confirmed_cases=sum (count,na.rm = T)) %>%
  ggplot(aes(x=year,y=total_confirmed_cases)) +
  geom_bar(stat = "identity", position = "dodge")

#confirmed cases in west harerge zone with visualization
case_data %>%
  filter(region=="Oromia", zone=="West Hararge", data_type=="confirmed") %>%
  group_by(region,zone,woreda,period) %>%
  summarise(total_confirmed=sum(count,na.rm = T)) %>%
  ggplot(aes(y=total_confirmed)) +
  geom_boxplot()
