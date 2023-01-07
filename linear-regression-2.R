### Confounding correlation is not causation 
## Lets explore the concept of data fishin data dredging 

N <- 25 
g <-1000000


## We are creating a normal distribution that is off the size one million
## and we are taking it 25 times and creating a normal distribution out of it 



sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N*g), y = rnorm(N*g))



head(sim_data)


res <- sim_data %>%
       group_by(group)%>%
       summarise(r = cor(x,y))%>%
       arrange(desc(r))
 

res

### Lets see how an oulier can make us have a higher correlation than 
## expected 

set.seed(1985, sample.kind = "Rounding")


x <- rnorm(100,100,1)
y <- rnorm(100,84,1)

x[-23] <- scale(x[-23])


y[-23] <- scale(y[-23])
  


qplot(x, y, alpha = 0.5)

#### the outlier makes it appear as if we had a big correalation 



cor(x,y)


#but if we remove the outlier we can see that the correlation is not 
## what it looked like 


cor(x[-23], y[-23])


### When you use the rank instead that fake correlation does not look to 
## have an influence on it this is the spearman correlation 


cor(rank(x), rank(y))

## you can use the spearman method argument within the correlation function 


cor(x,y, method = "spearman")



### Lets explore the reverse relation problem 
library(dplyr)
library(HistData)
library(tidyverse)
library(ggplot2)
library(broom)
data("GaltonFamilies")






set.seed(1983, sample.kind = "Rounding")


galton_1 <- GaltonFamilies %>%
            filter(gender == "male")%>%
            group_by(family)%>%
            sample_n(1)%>%
            ungroup()%>%
            select(childHeight, father)%>%
            rename(son = childHeight)


galton_1 %>% summarise(tidy(lm(father ~ son , data = .)))


inverse_coef            


## we can see a clear correlation even if whats causin the fathers height
## is clearly not the sons height but the other way arround the sons height
## has a correlation with the fathers height 




### Now lets learn about the confounding which whis a a variable having 
## a correlation in different variables 

library(dslabs)


data("admissions")


head(admissions)

str(admissions)



admissions %>% group_by(gender)%>%
               summarise(percentage = sum(admitted)/sum(applicants))


### If we see this percentage of the admited applicants we would say that 
## we have a gender bias 


### Test wether gender and admissions are indepentend 

admissions %>% group_by(gender) %>%
               summarize(total_admitted = round(sum(admitted / 100 * applicants)),
                         not_admitted = sum(applicants)- sum(total_admitted))%>%
              select(-gender)%>%
              summarize(tidy(chisq.test(.)))


### We just have to understand here that we are converting the admitted 
## columns in to percentages and we are working with that 


admissions %>% group_by(gender) %>% 
  summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
            not_admitted = sum(applicants)- sum(total_admitted)) %>%
  select(-gender) %>% 
  summarize(tidy(chisq.test(.)))              
               



### Now we are going to wrangle a little bit the data base to be able to 
## have information about admissions by gender 


admissions %>% select(gender, admitted, major)%>%
              pivot_wider(names_from = gender, values_from = admitted)%>%
              mutate(women_minus_men = women - men)

## Plot total percent admitted against major vs percent women applicants 




admissions %>% group_by(major)%>% 
summarise(major_selectivity = sum(admitted * applicants)/sum(applicants),
         percent_women_applicants = 
        sum(applicants * (gender == "women"))/sum(applicants) * 100) %>%
  ggplot(aes(major_selectivity,percent_women_applicants, label = major ))+
  geom_text()



### This is displaying how the mayors that admit the least applicants 
## are actually having more women applying 


### Lets remember how to do a bar plot that is differenciating the different 
## components on the bar 

head(admissions)

admissions %>%
         mutate(percent_admitted = admitted * applicants/ sum(applicants))%>%
         ggplot(aes(gender, y = percent_admitted, fill = major))+
         geom_bar(stat = "identity", position = "stack")



### In this plot with the help of the last plot we can see how the big ammount
## of male applicants admitted is comming from majors that have a high 
# acceptance rate


admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants))+
               geom_point()



### Lets see it in a table that is stratifying by gender the percentage of 
## admissions 


admissions %>% group_by(gender)%>% summarise(average = mean(admitted))



## Assesment confounding 

library(dslabs)
data("research_funding_rates")


head(research_funding_rates)


str(research_funding_rates)

### Two ways of constructing those estimates 



research_funding_rates %>% mutate(not_awarded_men = applications_men - awards_men, 
                                  not_awarded_women = applications_women - awards_women)%>%
                        select(awards_men,awards_women, not_awarded_men,not_awarded_women,awards_total)%>%
                        pivot_longer(-awards_total, names_to = "awarded_men_women",
                                     values_to = "awarded_n_by_gender")%>%
                        group_by(awarded_men_women)%>%
                        summarise(total_awards = sum(awarded_n_by_gender))
                        ungroup()%>%
                        summarise(percent_awards = sum(total_awards *(total_awards %in% c("awards_men","awards_woman")))



help("summarise_all")



asses <- research_funding_rates %>%
              summarise(across(where(is.numeric), sum))%>%
              summarise(yes_men = awards_men,
                        no_men = applications_men - awards_men,
                        yes_women = awards_women,
                        no_women = applications_women- awards_women)%>%
              gather()%>%
              separate(key, c("awarded", "gender"))%>%
              spread(gender,value)
 
## Until the second summarize we are going to have a string of useful values 
## but we want to format them as the 2X2 table that we want 
## we use the gather function that format this values as a table 
## remember that spread is similar that using pivot_wider

asses

options( digits = 3)

asses$men[1]
asses %>% mutate(percent_man_awarded = men/sum(men),
                 percent_women_awarded = women/sum(women))%>%
                 arrange(percent_man_awarded)


asses %>% select(-awarded)%>% chisq.test()

head(research_funding_rates)

dat <-

  






