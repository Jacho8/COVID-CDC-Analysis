install.packages("tibble")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("anytime")
install.packages("lubridate")
library(ggplot2)
library(anytime)
library(lubridate)
library(zoo)
library(scales)


cdIRver <- read.csv("Rates_of_COVID_19_Cases_or_Deaths_by_Age_Group_and_Vaccination_Status")

dataVacc <- read.csv("CDC_DS_Vacc.csv")

dataCD <- read.csv("CDC_DS_CaseDeath.csv")

#making char values in dataCD to int
dataCD$Fully.vaccinated.population <- as.integer(as.character(CDC_DS_CaseDeath$`Fully vaccinated population`))
dataCD$Unvaccinated.with.outcome <- as.integer(as.character(CDC_DS_CaseDeath$`Unvaccinated with outcome`))
dataCD$Vaccinated.with.outcome <- as.integer(as.character(CDC_DS_CaseDeath$`Vaccinated with outcome`))
dataCD$Unvaccinated.population <- as.integer(as.character(CDC_DS_CaseDeath$`Unvaccinated population`))

#Date format for dataVacc
vaccDates <- as.Date(CDC_DS_Vacc$Date)
dataVacc$Date <- mdy(CDC_DS_Vacc$Date)

#Converting dataCD date chars to Date value
cdDates <- CDC_DS_CaseDeath$month
dataCD$month <- CDC_DS_CaseDeath$month #Reset Months
cdDates <- my(CDC_DS_CaseDeath$month)

dataCD$month <- my(CDC_DS_CaseDeath$month)

as_tibble(dataVacc)
as_tibble(dataCD)
as_tibble(dataCDnew)


dataVacc%>%
  separate(col = date, into = c("year", "month"), sep = "-")#%>%
  filter(dataVacc, year >= 2022)

dataCDnew%>%
  ggplot()+
  geom_line(mapping = aes(x = month, y = dataCDnew$`Age adjusted IRR`))
  
  
#Vaccinated population of all ages 
 
filter(dataCD, Age.group == "all_ages_adj" & Vaccine.product == "all_types")%>%
  ggplot() +
  geom_smooth(mapping = aes(x = month, y = Vaccinated.with.outcome , group = outcome,linetype = outcome))
  #scale_x_discrete(limits = month.name)

#UNVaccinated population of all ages 

filter(dataCD, Age.group == "all_ages_adj")%>%
  ggplot() +
  geom_smooth(mapping = aes(x = month, y = Unvaccinated.with.outcome , group = outcome , linetype = outcome))

#Rate of booster shots over time. 

dataVacc%>%
  ggplot() +
  geom_line(mapping = aes(x= Date, y = Booster_Daily))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-12-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

#Total Vacc administered over time
dataVacc%>%
  group_by(Date)%>%
  ggplot() +
  geom_line(mapping = aes(x=Date, y = Administered_Cumulative))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-12-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  labs(y = "Total Vaccines Administered")

#Unvaxx pop death over time
filter(dataCD, Age.group == "all_ages_adj" & Vaccine.product == "all_types" & outcome == "death")%>%
  ggplot(mapping = aes(x = month, y = Unvaccinated.with.outcome))+
  geom_smooth()+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  labs(x = "Death outcomes")
    

#Current unvaxx case v death)
filter(dataCD, Age.group == "all_ages_adj")%>%
  ggplot()+
  geom_smooth(mapping = aes(x = month, y = Unvaccinated.with.outcome, group = outcome, linetype = outcome))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

#vaxx case v death
filter(dataCD, Age.group == "all_ages_adj")%>%
  ggplot()+
  geom_smooth(mapping = aes(x = month, y = Vaccinated.with.outcome, group = outcome, linetype = outcome))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  #scale_y_discrete(labels = label_number(scale = 10))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

#Vaxx pop death over time
filter(dataCD, Age.group == "all_ages_adj" & Vaccine.product == "all_types" & outcome == "death")%>%
  ggplot()+
  geom_smooth(mapping = aes(x = month, y = Vaccinated.with.outcome))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  labs(x = "Death outcomes")

#Vaxx case over time
filter(dataCD, Age.group == "all_ages_adj" & Vaccine.product == "all_types" & outcome == "case")%>%
  ggplot()+
  geom_smooth(mapping = aes(x = month, y = Vaccinated.with.outcome))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  labs(y = "Vaccinated cases")

#Unvaxx case over time
filter(dataCD, Age.group == "all_ages_adj" & Vaccine.product == "all_types" & outcome == "case")%>%
  ggplot()+
  geom_smooth(mapping = aes(x = month, y = Unvaccinated.with.outcome))+
  scale_x_date(date_labels = "%b-%y", breaks = seq.Date(
    from = as.Date("2021-04-01"), to = as.Date("2022-08-01"), by = 30))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))+
  labs(y = "Unvaccinated cases")
  
library(ggplot2)
library(anytime)
library(lubridate)
library(zoo)
library(scales)

citation("tibble")
