getwd()
install.packages("lubridate")
install.packages("directlabels")
install.packages("corrplot")
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(directlabels)
library(corrplot)
cv <- read.csv("COVID.csv")
cv
head(cv$Date)
colnames(cv)

datanew <- cv %>%
  mutate(Date = mdy(Date))
datanew1 <- datanew %>%
  mutate(First.Case = mdy(First.Case))
head(datanew1)
datanew2 <- datanew1 %>%
  mutate(Lockdown.State.of.Emergency = mdy(Lockdown.State.of.Emergency))
head(datanew2)



head(datanew$Date)
head(datanew)
str(datanew)
typeof(datanew["Total.Case"])


covid <- datanew2 %>% filter(Country %in% c("Azerbaijan","Bosnia and Herzegovina","Denmark","Estonia","France","Germany","Italy","Spain"))
covid
typeof(covid)
class(covid)
head(covid)

#plotting total cases 
plot_case <- ggplot(covid, aes(x = Date,y = as.numeric(Total.Case), color = Country)) + geom_line(lwd = 1.25) + ylab("confirmed total cases") + labs(title = "Total covid19 cases from January 22 to May 13,2020") + geom_dl(aes(label= Country),method = "smart.grid")
plot_case

#plotting new cases
plot_case_daily <- ggplot(covid, aes(x =Date,y =  as.numeric(New.Case), color = Country)) + geom_line(lwd = 1.25) + ylab("Confirmed new cases") +labs(title = "Daily new covid19 cases from January 22 to May 13,2020") + geom_dl(aes(label= Country),method = "smart.grid")
plot_case_daily

#plotting total deaths
plot_death <- ggplot(covid, aes(Date, Total.Death, color = Country))+ geom_line(lwd = 1.25) + ylab("Cumulative confirmed deaths")+ labs(title = "Total deaths due to covid 19 from January 22 to May 13,2020")
plot_death

#plotting daily deaths
plot_death_daily <- ggplot(covid) + geom_line(aes(Date, New.Death, group = Country, color = Country), lwd = 1.25) + ylab(" confirmed new deaths") + labs(title = "Daily new deaths due to covid 19 from January 22 to May 13,2020")
plot_death_daily

#plotting population
pop <-  ggplot(covid)+geom_col(mapping =aes(x=Country,y=Population,fill=Country))+
  labs(y="Population",x="Country",title ="Populations of the countries ")
pop



#plotting infection density
m3 <- as.numeric(covid$Total.Case)/ covid$Population
m3
case_density <- m3*1000; case_density
covid1 <- cbind(covid,case_density)
head(covid1)


plot_case_density <- ggplot(covid1) + geom_line(aes(Date, case_density, group = Country, color = Country), lwd = 1.25) + ylab("infected cases per 1000 people") + labs(title = "covid 19 cases per 10000 people from January 22 to May 13,2020")
plot_case_density 



#plotting death density
m4 <- covid$Total.Death/ covid$Population
m4
death_density <- m4*1000; death_density
covid2 <- cbind(covid1,death_density)
head(covid2)


plot_death_density <- ggplot(covid2) + geom_line(aes(Date, death_density, group = Country, color = Country), lwd = 1.25 ) + ylab("deaths per 1000 people") + labs(title = "Deaths due to covid 19 per 10000 people from January 22 to May 13,2020")
plot_death_density


#plotting case_density vs death_density
plot_cased_vs_deathd <- ggplot(covid) + geom_line(aes(case_density, death_density, group = Country, color = Country), lwd = 1.25) + ylab("confirmed deaths per 1000 people") + xlab("Confirmed covid cases per 1000 people") + labs(title = "Case per 1000 people vs deaths per 1000 people") 
plot_cased_vs_deathd


#plotting elderly rate
er <-  ggplot(covid)+geom_col(mapping =aes(x=Country,y=Elderly.Rate/100,fill=Country))+
  labs(y="Elderly rate",x="Country",title ="Elderly rates of the countries ")
er 


#plotting delay in lockdown
delay_in_lockdown <- covid3$Lockdown.State.of.Emergency - covid3$First.Case; delay_in_lockdown
covid4 <- cbind(covid3, delay_in_lockdown)
head(covid4)



delay <-  ggplot(covid4)+geom_col(mapping =aes(x=Country,y= delay_in_lockdown/100,fill=Country))+
  labs(y="Delay in lockdown",x="Country",title ="Days taken to impose lockdown after the first case got detected in each countries ")
delay


#plotting total recovery
plot_recovery <- ggplot(covid) + geom_line(aes(Date, Total.Recovery, group = Country, color = Country), lwd = 1.25) + ylab(" confirmed total recovery from covid 19") + labs(title = "Confirmed total recoveries from covid 19 from January 22 to May 13,2020")
plot_recovery

#plotting daily recovery
plot_recovery_daily <- ggplot(covid) + geom_line(aes(Date, New.Recovered, group = Country, color = Country), lwd = 1.25) + ylab(" confirmed new recoveries from covid 19") labs(title = "Confirmed daily recoveries from covid 19 from January 22 to May 13,2020")
plot_recovery_daily

#plotting recovery density
m7 <- covid$Total.Recovery/ covid$Population
m7
recovery_density <- m5*1000; recovery_density
covid5 <- cbind(covid4,recovery_density)
head(covid5)


plot_recovery_density <- ggplot(covid5) + geom_line(aes(Date, recovery_density, group = Country, color = Country), lwd =1.5) + ylab("number of people recovered per 1000 people") + labs(title = "Recovery from  covid 19 per 10000 people from January 22 to May 13,2020")
plot_recovery_density

#plotting cased vs recoveryd
plot_cased_vs_recoveryd <- ggplot(covid) + geom_line(aes(case_density, recovery_density , group = Country, color = Country), lwd = 1.25) + ylab("confirmed recovery per 1000 people") + xlab(" covid cases per 1000 people") + labs(title = "Case per 1000 people vs recovery per 1000 people")
plot_cased_vs_recoveryd


#plotting total tests
plot_test <- ggplot(covid) + geom_line(aes(Date, Total.Test, group = Country, color = Country), lwd = 1.25) + ylab(" confirmed total tests") + labs(title = "Total covid19 tests from January 22 to May 13,2020")
plot_test

#plotting new tests daily
plot_test_daily <- ggplot(covid) + geom_line(aes(Date, New.Test, group = Country, color = Country), lwd = 1.25) + ylab(" confirmed new tests") + labs(title = "Total covid19 tests per day from January 22 to May 13,2020")
plot_test_daily


#plotting test density
m5 <- covid$Total.Test/ covid$Population
m5
test_density <- m5*1000; test_density
covid3 <- cbind(covid2,test_density)
head(covid3)


plot_test_density <- ggplot(covid3) + geom_line(aes(Date, test_density, group = Country, color = Country), lwd =1.5) + ylab("number of tests performed per 1000 people") + labs(title = "Tests of covid 19 performed per 10000 people from January 22 to May 13,2020")
plot_test_density





