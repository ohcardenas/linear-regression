### text mining



library(tidyverse)
library(lubridate)
library(ggplot2)
library(tidyr)
library(scales)


## We can extract data directly from twitter with rtweet too 

### in this case we can use data readily available to us in the next link 

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


Teams %>% head


### sccatter plots are the visualization tool for exelence making linear regressions

### Lets create a correlation scatter plot between two new variables that 
## we will create in the dataset 


Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate(HR_per_game = HR / G , R_per_game = R / G ) %>%
          ggplot(aes(HR_per_game, R_per_game))+
          geom_point(alpha = 0.5)


### lets do the same analysis to SB stolen basis per game and runs per game 


Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate( SB_per_game = SB / G , R_per_game = R / G)%>%
          ggplot(aes(SB_per_game, R_per_game))+
          geom_point( alpha = 0.5)

## The correlation of stolen bases and the runs per game is not as strong as with 
## home runs 


## Now lets look at the correlation between  BB base on balls and the runs in a 
## game 

Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate( BB_per_game = BB / G , R_per_game = R / G)%>%
          ggplot(aes(BB_per_game, R_per_game))+
          geom_point( alpha = 0.5)


### Here we see a stronger correlation that with the SB stolen bases but this can
## be do to confounding a concept that we are going to review in debt 



## Lets see what's the correlation of AB at bat per game and runs per game 
## we are going to do the same process as with the other variables 


Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate(AB_per_game = AB / G , R_per_game = R / G)%>%
          ggplot(aes(AB_per_game, R_per_game))+
          geom_point( alpha = 0.5)



## Now we are going to plot the wint rate vs the filding errors per game 




Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate(Wins_per_game = W / G, E_per_game = E / G)%>%
          ggplot(aes(E_per_game, Wins_per_game))+
          geom_point( alpha = 0.5)



### Lets make a correlation between triples X3B and doubles  X2B per game to see how 
## the two case of successes correlate 



Teams %>%  filter( yearID %in% 1961:2001)%>%
           mutate(two_B_per_game = X2B / G , three_B_per_game = X3B / G )%>%
           ggplot(aes(two_B_per_game,three_B_per_game))+
           geom_point( alpha = 0.5)
           




library(Lahman)

data("Teams")


Teams %>% head()

r_1 <- Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate( Wins_per_game = W / G, E_per_game = E / G ) %>%
          summarise( r = cor(Wins_per_game, E_per_game)) %>%
          pull(r)


signif(r_1,3)

r_2 <- Teams %>% filter( yearID %in% 1961:2001)%>%
                mutate( X2B_per_game = X2B / G , X3B_per_game = X3B / G) %>%
                summarise( r = cor(X2B_per_game, X3B_per_game))%>%
                pull(r)


signif(r_2, 3)


### Let's understand correlation between averages and standart deviations 

library(tidyverse)
install.packages("HistData")

library(HistData)
data("GaltonFamilies")

## remember thats how the set seed used to work 

set.seed(1983, sample.kind = "Rounding")

GaltonFamilies %>% head



### In this code by grouping by family and and 


galton_heights <- GaltonFamilies %>%
                  filter(gender == "male")%>%
                  group_by(family)%>%
                  sample_n(1)%>%
                  ungroup() %>%
                  select( father, childHeight)%>%
                  rename(son = childHeight)
                  

galton_heights %>% summarise( mean(father), sd(father), mean(son), sd(son))



galton_heights %>% ggplot(aes(father, son))+
                   geom_point(alpha = 0.5)


##We cannot see a clear relationship between the two variables even if they 
##do have a correalationship 

### lets compute a correlation of two variables using R 



galton_heights_2 <- GaltonFamilies %>%
                    filter( childNum == 1 & gender == "male")%>%
                    select( father, childHeight)%>%
                    rename( son = childHeight)



nrow(galton_heights_2)


galton_heights_2 %>% summarise(cor(father,son))


set.seed(0)


my_sample <-  slice_sample(galton_heights_2, n = 25, replace = TRUE)


R <- my_sample %>% summarize(r = cor(father, son))



R


B <- 10000
N <- 25 

R <- replicate(B, {
  
  
  sample <- slice_sample(galton_heights_2, n = N, replace = TRUE)%>%
  summarise(r = cor(father, son))%>% .$r
  

})



print(R)


data.frame(R) %>% ggplot(aes(R))+ geom_histogram(binwidth = 0.05, color= "firebrick")



mean(R)


### As we can see because of the small size of the population we can see that 
## sd or our random varible is big 

sd(R)



## we know that because roh is a random variblable the central limit thorem 
## aplies here too some for a random varible with a big large amount of abservations
## the variable behaves normally 


             
data.frame(R) %>%
              ggplot(aes(sample = R)) +
              stat_qq() +
              geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2))
                          ,color = "blue")


## We can see that the behavior of the curve is not normal 


library(Lahman)


data("Teams")


head(Teams)


cor_1 <-Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate(R_per_game = R / G , AB_per_game = AB / G) %>%
          summarise(r = cor(R_per_game, AB_per_game))%>% .$r



signif(cor_1, 3)


### To construct a conditional mean the estimated mean of a son height based on 
## fathers height

## let's say we know that the fater is 72 inches 



galton_heights_2 <- GaltonFamilies %>%
  filter( childNum == 1 & gender == "male")%>%
  select( father, childHeight)%>%
  rename( son = childHeight)


## ltes see how many entries we have for fathers that are 72 inches 


sum(galton_heights_2$father == 72)

### if we change that for 72.5 is even less data 


sum(galton_heights_2$father == 72.5)



## let's round that 72 inches so we have more data 

conditional_avg <- galton_heights_2 %>%
                   filter(round(father) == 72)%>%
                   summarise(avg = mean(son))%>%
                   pull(avg)

conditional_avg

### this would be our conditional average notice that is much higher than the 
##69 inches average 


### now we have many more entries 2012


### Lets stratify that fathers height having in to account the values available in
## inches and make a graph to see the influenze of the father height on the sons
## height 



galton_heights_2 %>% mutate(father_strata = factor(round(father)))%>%
                  ggplot(aes(father_strata, son)) +
                  geom_boxplot()+
                  geom_point()



## center of each plot
### Another way of stratifying your values with the group by function and 
### computing the conditional average after with summarize 



galton_heights_2 %>% mutate(father = round(father))%>%
                     group_by(father)%>%
                     summarise(son_conditional_avg = mean(son))%>%
                     ggplot(aes(father, son_conditional_avg)) + 
                     geom_point()
                     




### Lets add a regression line to the standardized data 



r <- galton_heights_2 %>% summarise(r = cor(father, son)) %>% pull(r)

## with the scale command we are just going to normalize the data 


help("scale")

galton_heights_2 %>% mutate(father = scale(father), son = scale(son)) %>%
                     mutate(father = round(father))%>%
                     group_by(father)%>%
                     summarise(son = mean(son))%>%
                     ggplot(aes(father, son))+
                     geom_point()+
                     geom_abline(intercept = 0, slope = r)




#### Add reggression line to the original data

mu_x <- mean(galton_heights_2$father)
mu_y <- mean(galton_heights_2$son)


s_x <- sd(galton_heights_2$father)
s_y <- sd(galton_heights_2$son)

r <- cor(galton_heights_2$father, galton_heights_2$son)


m <- r * mu_y/mu_x

b <- mu_y - m *mu_x


## this is going to be the linear regression 

galton_heights_2 %>% ggplot(aes(father, son))+
                     geom_point(alpha = 0.5) +
                     geom_abline(intercept = b , slope = m)
    

### lets plot in standarized units and see that the intercet is 0 

galton_heights_2 %>% ggplot(aes(scale(father), scale(son))) +
                     geom_point(alpha = 0.5) + 
                     geom_abline(intercept = 0 , slope = r)





galton_heights_2 %>% mutate(z_father = round((father - mean(father))/ sd(father))) %>%
                     filter(z_father %in% -2:2)%>%
                     ggplot() +
                     stat_qq(aes(sample = son))+
                     facet_wrap(~z_father)



### Assesent question 6



0.5*3/2




library(HistData)

data("GaltonFamilies")


GaltonFamilies %>% head

set.seed(1989, sample.kind = "Rounding")

female_heights <- GaltonFamilies %>%
                  filter( gender == "female")%>%
                  group_by(family)%>%  
                  sample_n(1)%>%
                  ungroup()%>%
                  select(mother, childHeight)%>%
                  rename(daughter = childHeight)

female_heights
#### seems that we ust take a random sample of one the the families because
### we want to take just one of the daughters per family and to do it randomly 



m_x <- mean(female_heights$mother)

m_x

s_x <- sd(female_heights$mother)

s_x

m_y <- mean(female_heights$daughter)

s_y <- sd(female_heights$daughter)


r <- cor(female_heights$mother, female_heights$daughter)

r

### significatice numners of the correlation


signif(0.3245199, 4)


m <- r * s_y/s_x

signif(m, 4)



b <- m_y - (m*m_x)


signif(b, 4)


### What percentage in the variability of the daughters hight is explained by the 
## mothers hight

r^2*100


### Formula of the expected value of an x knowing that the biariate distribution 
## aplies for all of the groups included on the bivariate distribution 


60*m + b


### Lets start using a simpler formula for calculating the 
## intercept point and the slope of our linear regressions 


lm(son ~ father, data = galton_heights_2)




## We want the intercept of our model to be interretable so we run the same 
## model so we run the same model as before but now we substract the mean 


galton_heights_c <- galton_heights_2 %>%
                    mutate(father_centered = father- mean(father))


lm(son ~ father_centered, data = galton_heights_c)



#### lets come back to using lm to get the least square estimates 


set.seed(1983, sample.kind = "Rounding")


galton_heights_2 <- GaltonFamilies %>%
                    filter( gender == "male")%>%
                    group_by(family)%>%
                    sample_n(1)%>%
                    ungroup()%>%
                    select( father, childHeight)%>%
                    rename(son = childHeight)





### How do you built your RSS function

rss <- function (beta0, beta1){
   resid <- galton_heights_2$son - (beta0+beta1*galton_heights_2$father)
    return(sum(resid^2))
  
  }

### so we are just applying the residual sum of squares to each of the
## elements on the object galton_heights_2 the function being 
### sumatoria de Y - (B0+B1*x1) squared 



### So this would be a 3 dimentional plot with beta0 = x and beta1 = y 
## and RSS on the z axis 
### we'll keep beta0 constant to keep it simple 




beta1 = seq(0, 1, len=nrow(galton_heights_2))



library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights_2 <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)



rss <- function(beta0, beta1){
  resid <- galton_heights_2$son - (beta0+beta1*galton_heights_2$father)
  return(sum(resid^2))
}


### the lenght argument is wtritten down a little bit weird

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights_2))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))



results %>% ggplot(aes(beta1, rss)) + geom_line() +
            geom_line(aes(beta1, rss), color = "firebrick")
  


results
## On this plot we can see a clear minimum for beta 1 at about 0.65
### Bu we dont know if thats the minimum for beta 0 we just know 
### that this is the minimum for beta1 when beta0 is fixed at 25 


## Is better to use calculus to we will use partial derivatives and well 
## set them at 0 and solve beta1 and beta0 

## of couse if we have many parameters this equations can get rather complx 
## and thats why we will use R to use functions to perform thi equations 
## for us 



fit <-lm( son ~ father, data = galton_heights_2)

fit

summary(fit)


## knowing that our Bs are random variables lets 
## try some montecarlo simulations


N <- 50 
B <- 10000

lse <- replicate(B, {
  
  sample_n(galton_heights_2, N, replace = TRUE)%>%
    lm(son ~ father, data = .)%>%
    .$coef

})
### we are going to take samples random samples of N size of our galton_heights_2


lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
lse

library(gridExtra)

p1 <- lse %>% ggplot(aes(beta_0))+ geom_histogram( binwidth = 5
                                       , color = "firebrick")


p1                     
                     
p2 <- lse %>% ggplot(aes(beta_1))+ geom_histogram(binwidth = 0.1
                                                  ,color = "firebrick")

p2


grid.arrange(p1, p2, ncol = 2)




### summary statistics 


sample(galton_heights_2, N, replace = TRUE)%>%
       lm(son ~ father, data = .)%>%
        summary%>%
        .$coef



lse %>% summarise(se_0 = sd(beta_0), se_1 = sd(beta_1))


####  Its useful to know that the LSE can be strongly correlated 

lse %>% summarise(cor(beta_0, beta_1))



### however the correlation depends on how the predictors are defined or 
## transformed Here we standarize the father heights which changes xi to 
## xi- x --- x being the mean or aswe say x bar

N <-50 
B <- 10000

## standarized LSE 

lse_s <- replicate(B, {
   
    sample_n(galton_heights_2, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father , data = .) %>% .$coef

})

cor(lse[1,], lse[2,])



### A fast way of seeing graphically this the regession line between two 
## variables and their respective confidence intervals is to use the ggplot
## package 


galton_heights_2 %>% ggplot(aes(father, son)) +
                     geom_point()+
                     geom_smooth(method = "lm")


## notice that in this case you have to put the conditional variable first
## and after that you put the value to predict against what you do when 
## you use the lm function 


fit <- galton_heights_2 %>% lm(son ~ father , data = .)


Y_hat <- predict(fit , se.fit = TRUE)

names(Y_hat)

Y_hat


galton_heights_2 %>% mutate( Y_hat = predict(lm(son ~ father, data = .))) %>%
                     ggplot(aes(son, Y_hat)) +
                     geom_line()


### let's create the function to compute the RSS again 

library(HistData)

data("GaltonFamilies")

galton_heights_2 <- GaltonFamilies %>%
  filter( childNum == 1 & gender == "male")%>%
  select( father, childHeight)%>%
  rename( son = childHeight)



rss <- function(beta0,beta1){
     
  resid <- galton_heights_2$son -(beta0+beta1*galton_heights_2$father)
  return(sum(resid^2))
  
}


## lets built beta one as a sequence of the values or proportions knowing that 
## our beta 1 is going to be a random variable for easiness qe are going to 
## create a sequence 

set.seed(1983, sample.kind = "Rounding")


beta1 = seq(0,1, len=nrow(galton_heights_2))

results <- data.frame(beta1 = beta1, 
                  rss = sapply(beta1, rss, beta0 = 36))


results


results %>% ggplot(aes(beta1, rss)) + geom_line()+
             geom_line(aes(beta1,rss), col = "firebrick")




library(Lahman)



data("Teams")

help("lm")

## if you want to make the linear regression with more thatn one variable 
### You just add the arguments to the function with a +


Teams %>% head(n=10)


Teams %>% filter( yearID %in% 1961:2001)%>%
          mutate(BB_per_game = BB / G, Runs_per_game = R / G
                 ,HR_per_game = HR / G) %>%
          lm(Runs_per_game ~ BB_per_game + HR_per_game, data = .)%>% summary



B <- 1000
N <- 100


lse_s <- replicate(B, {
  
  sample_n(galton_heights_2, N, replace = TRUE)%>%
  lm(son ~ father, data = .) %>% .$coef
  
})


lse_s <- data.frame(beta0 = lse_s[1,], beta1 = lse_s[2,])



lse_s


mean(lse_s$beta0)



model <- lm(son ~ father, data = galton_heights_2)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights_2$father)

data



predictions

ggplot(data, aes(x = father, y = fit)) +
  geom_line(col = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights_2, aes(x = father, y = son))



### lets come back to our galton heights but on the female side and apply 
### the knoledge adquired 



set.seed(1989, sample.kind = "Rounding")

options(digits = 3)


library(HistData)
data("GaltonFamilies")


galton_heights_female <- GaltonFamilies %>%
                         filter(gender == "female")%>%
                         group_by( family)%>%
                         sample_n(1)%>%
                         ungroup()%>%
                         select(mother, childHeight)%>%
                         rename(daughter = childHeight)




galton_heights_female

galton_heights_female %>% ggplot(aes(mother, daughter)) + geom_point() +
                          geom_smooth(method = "lm")


### We design the fit to predict the height of the mother from the doughters
## heights 

fit <- lm(mother ~ daughter, data = galton_heights_female)


fit
summary(fit)





predictions_2 <- predict(fit, se.fit = TRUE)


dat <- as_tibble(predictions_2) %>% bind_cols(mother = galton_heights_female$mother)

dat %>% head



### Lets define the stability of the diferent data that we have to predict 
## performance accross the years 


library(Lahman)

Batting %>% head





bat_01 <- Batting %>% mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa,
                             bb = BB / pa )%>%
                      filter(yearID %in% 1999:2001 & pa >= 100)%>%
                      group_by(playerID)%>%
                      summarise(mean_singles_99_01 = mean(singles) , mean_bb_99_01 = mean(bb))
                      
 
                     
### remember that in the summarise statement if you operate with a  
### function that includes all of the entries in the players id per player 
### like the mean the result will include only one entry per player but you 
## have to aperate with a mean or a function like that 
                      


sum(bat_01$mean_singles > 0.2)


sum(bat_01$mean_bb > 0.2)


                      
                      

bat_01 %>% head

                     


bat_02 <- Batting %>% filter(yearID == 2002) %>%
                      mutate(pa = AB + BB, mean_singles_02 = (H - X2B - X3B - HR)/pa,
                              mean_bb_02 = BB / pa)%>%
                      filter( pa >= 100)%>%
                      select(playerID, mean_singles_02, mean_bb_02)



### now lets create an inner join to be able to see the correlation between 
### variables in both of the tables 





full_bat <-inner_join(bat_01, bat_02, by = "playerID")





cor(full_bat$mean_singles_02, full_bat$mean_singles_99_01)

cor(full_bat$mean_bb_02, full_bat$mean_bb_99_01)

head(bat_02)



### Lets make a scatter plot between the single rate in 2002 and 99to01 
### and another for the bb rate in 02 and 99to01 

library(gridExtra)

p1 <- full_bat %>% ggplot(aes(mean_singles_02, mean_singles_99_01)) +
             geom_point()


p2 <- full_bat %>% ggplot(aes(mean_bb_02, mean_bb_99_01)) + 
                   geom_point()
p2

grid.arrange(p1, p2)



fit_1 <- full_bat %>% lm(mean_singles_02 ~ mean_singles_99_01, data = .)

fit_1


fit_2 <- full_bat %>% lm(mean_bb_02 ~ mean_bb_99_01, data = .)

fit_2


### Let's learn how to use broom to connect the cuntionalities of the lm 
## function to th functionalities of the tidyverse package lets stratify 
#s the Teams data by HR per game 



dat <- Teams %>% filter(yearID %in% 1961:2001)%>%
                 mutate(HR_per_game = round(HR / G , 1),
                        BB_per_game = BB / G , R_per_game = R / G)%>%
                        select(HR_per_game, BB_per_game, R_per_game)%>%
                        filter(HR_per_game >= 0.4 & HR_per_game <= 1.2)



### before we were just pluggin the function of the slope in the summarize 
## funtion to get this information 



dat %>% group_by(HR_per_game)%>%
 summarise(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))




### If we wuld try to get the slope with the lm function after grouping 
## it it woulnd't work just like this 



dat %>% group_by(HR_per_game)%>%
        lm(R_per_game ~ BB_per_game, data = .)%>%
        .$coef


## As you can see it's not giving us the different slopes for the groups 
## or strata of HR_per game 


## What we need to use is the lm function inside the summarize function 

library(broom)

## tidy funtion from broom returns estimates in and information in a data 
## frame 

fit <- dat %>% lm(R_per_game ~ BB_per_game, data = .)


## Lets us the tidy function 

tidy(fit)


## if we want the confidence intervals we can use this argument within 
## the function dy


tidy(fit, conf.int = TRUE)


### This next code wouldnt work 


dat %>% group_by(HR_per_game)%>%
        summarise(lm(R_per_game ~ BB_per_game, data = .))




### But using the broom package we can 
### You dont have to call the data frame this time within the lm function 


dat %>% group_by(HR_per_game)%>%
        summarise(tidy(lm(R_per_game ~ BB_per_game), conf.int = TRUE))


### lets filter it to make it a litlle bit more clear



dat %>% group_by(HR_per_game)%>%
        summarise(tidy(lm(R_per_game ~ BB_per_game), conf.int = TRUE))%>%
        filter(term == "BB_per_game")%>%
        select(HR_per_game, estimate, conf.low, conf.high)



### WIth this new object defined as a tible we can easily visualize the 
### the slopes to see if we have a multivariate distribution 





dat %>% group_by(HR_per_game)%>% 
        summarise(tidy(lm(R_per_game ~ BB_per_game), conf.int = TRUE))%>%
        filter(term == "BB_per_game")%>%
        select(HR_per_game, estimate, conf.low, conf.high )%>%
        ggplot(aes(HR_per_game, y = estimate, ymin = conf.low, ymax = conf.high)) +
        geom_errorbar()+
        geom_point()


## We can see how the confidence intervals overlap and that's allowing 
## us to asumme that we hae a multivariate distribution 


### If we want just a summarry of the different statistics of our linear 
## regression 


glance(fit)


## lets define the function get_slope to extract specific information 
## out of the summary of our lm function 




fit <- lm(R_per_game ~ HR_per_game , data = dat)
fit
sum_fit <- summary(fit)
sum_fit


## we can see that our second row is containing the coefficients that 
## we need 

get_slope <- function(data){
  
  fit <- lm(R_per_game ~ HR_per_game, data = data)
  sum_fit <- summary(fit)
  
  
  data.frame(slope = sum_fit$coefficients[2, "Estimate"],
             se = sum_fit$coefficients[2, "Std. Error"], 
             p_value = sum_fit$coefficients[2,"Pr(>|t|)"])

}


### Understanding the subsetting [2,xxx] we are calling the second row 
## in our summary and after that we are just calling to specific columns 
## by name 



dat <- Teams %>% filter(yearID %in% 1961:2001)%>%
  mutate(HR_per_game = round(HR / G , 1),
         BB_per_game = BB / G , R_per_game = R / G)%>%
  select(HR_per_game, BB_per_game, R_per_game)%>%
  filter(HR_per_game >= 0.4 & HR_per_game <= 1.2)


dat %>% group_by(HR_per_game)%>%
         summarize(get_slope(across()))
        
  
### You have to realize that the object that you are creating when you 
## create the sum_fit within the function is not the data frame of teams
## any more you are woring with the summary of the function lm so 
### you are working with the coefitients 
### even if thats the case when you use the group_by in this case you can 
g





help("across")

### the across funtion allows us to apply the same formula accross diferent 
## columns 


### We want to know if the relationship between home runs and runs per game 
## varies across different leagues so se are goin to create a dat object
## that includes the lg id and the cariables that we have been analizing 
library(Lahman)
data("Teams")

dat_2 <- Teams %>% filter( yearID %in% 1961:2001 )%>%
                   mutate(BB_per_game = BB / G , HR_per_game = HR / G,
                          R_per_game = R / G)%>%
                   select(lgID,BB_per_game, HR_per_game, R_per_game)



dat_2 %>% head()


#### Now lets stratify the data by league 



### Lets see it on a graph 


dat_2 %>% ggplot(aes(HR_per_game, R_per_game)) +
          geom_point()+
          geom_smooth( method = "lm")
          

dat_2 %>% group_by(lgID)%>%
          summarise(tidy(lm(R_per_game ~ HR_per_game, data = across()), conf.int = TRUE))%>%
          filter(term == "HR_per_game")


help("gather")


### We have investigated the relationship between fathers' heights and 
##sons' heights. But what about other parent-child relationships? 
###Does one parent's height have a stronger association with child height? 
##How does the child's gender affect this relationship in heights? 
##Are any differences that we observe statistically significant?

##The galton dataset is a sample of one male and one female child from
##each family in the GaltonFamilies dataset. The pair column denotes whether the pair is father and daughter, father and son, mother and daughter, or mother and son.



library(tidyverse)
library(HistData)
data("GaltonFamilies")

GaltonFamilies %>% head()

set.seed(1, sample.kind = "Rounding")


### When we group it by two categories in the case of the sample_n 
## we are going to draw in this case one male and one female from the 
## families group 

galton <- GaltonFamilies %>%
          group_by(family, gender)%>%
          sample_n(1)%>%
          ungroup()%>%
          pivot_longer(father:mother, names_to = "parent", 
                       values_to = "parent_height")%>%
          mutate( child = ifelse( gender == "female", "daughter", "son"))%>%
          unite(pair, c("parent", "child"))
          

## remember thw functioning of the pivot longer function where you merge 
## the info of one two or several columns in the first argument and after 
## you sen de names of those columns to a new column and the values to the 
## next argument 

### the ifelse function where if gender is equal female in this case we 
## are going to input daughter and if not we are going to input son 






galton


head(galton$pair)


galton %>% group_by(pair)%>%
           summarise(n = n())

help("summarise")



### how to calculate the correlation coefficient of the different pairs 
  

galton %>% group_by(pair) %>%
           summarise(r = cor(childHeight, parent_height))



## we are going to use the lm function and the broom package to calculate 
## the least minimum squares for each of the pairs constructed in our example 





galton %>% group_by(pair)%>%
           summarise(tidy(lm(childHeight ~ parent_height), conf.int = TRUE))%>%
           filter(term == "parent_height")




### Remember that to proof that the variables that the variables that we 
## are working with follow a multivariate distribution we can see it on a 
### by stratifying our data to observe the relevant variables and use the 
## lm arguments 


### remember that the x that you are using in the gg plot to plot the 
## confidence intervals is the variable that you used in the group_by argument 
## to group the 

galton %>% group_by(pair)%>%
           summarize(tidy(lm(childHeight ~ parent_height), conf.int = TRUE))%>%
           filter( term == "parent_height")%>%
           ggplot(aes(pair, y = estimate, ymax = conf.high, ymin = conf.low)) +
           geom_errorbar()+
           geom_point()





### if we just want to check the different statistics of our lm model 
## remember that you can just use the broom package to see the different 
## statistics of the fit 



galton %>% group_by(pair)%>%
  summarize(tidy(lm(childHeight ~ parent_height), conf.int = TRUE))%>%
  filter( term == "parent_height", p.value < 0.05)


## to operate the columns in the data set always prefer to use summarise


galton %>% group_by(pair)%>%
  summarize(tidy(lm(childHeight ~ parent_height), conf.int = TRUE))%>%
  filter( term == "parent_height")%>%
  summarise(x = conf.high - conf.low)




### Lets see a linear regression model with more than one variable 


fit <- Teams %>% filter(yearID %in% 1961:2001)%>%
                 mutate(BB = BB/G , HR = HR/G, R = R/G)%>%
                 lm(R ~ BB+ HR, data = .)


tidy(fit, conf.int = TRUE)



### Remember that you can do a linear regression with as many variables 
## that are multivariate between them 

fit <- Teams %>% filter( yearID %in% 1961:2001)%>%
                 mutate( BB = BB/G ,
                         singles = (H- X2B - X3B - HR)/G , 
                         doubles = X2B /G , 
                         triples = X3B/G, 
                         HR = HR/G,
                         R = R/G )%>%
      lm(R ~ BB+singles+doubles+triples+HR, data = .)


coefs <- tidy(fit, conf.int = TRUE)


coefs



Teams %>% filter(yearID %in% 2002) %>%
          mutate(BB = BB/G,
                 singles = (H-X2B-X3B-HR)/G, 
                 doubles = X2B/G, 
                 triples = X3B/G, 
                 HR =HR/G, 
                 R =R/G) %>%
     mutate(R_hat = predict(fit, newdata = .))%>%
     ggplot(aes(R_hat, R,  label = teamID)) +
     geom_point() +
     geom_text(nudge_x=0.1, cex = 2) +
     geom_abline(col="blue")
     
   


### Now we are going to create statistiques that will allow us to redict the 
## performance of players instead of the performance of the teams 



## first we will crate the pa per game 
library(Lahman)
data("Batting")

Batting %>% head

pa_per_game <- Batting %>% filter( yearID %in% 2002) %>%
               group_by(playerID)%>%
               summarise(pa_per_game = sum(AB+BB)/max(G))%>%
               pull(pa_per_game)%>%
               mean
pa_per_game


### By use the sum function on all of the statistics we are just making sure 
## that each of the statistics that we are mesuring are going to the for 
## all of the entries that we have for each player that we grouped by 

players <- Batting %>% filter( yearID %in% 1999:2001)%>%
                       group_by(playerID)%>%
                       mutate( PA = AB + BB ) %>%
                       summarise( G = sum(PA) / pa_per_game, 
                                  BB = sum(BB) / G , 
                                  singles = sum(H-X2B-X3B-HR)/G,
                                  doubles = sum(X2B)/G, 
                                  triples = sum(X3B)/G, 
                                  HR = sum(HR)/G, 
                                  AVG = sum(H)/ sum(PA),
                                  PA = sum(PA))%>%
          filter(PA >= 300)%>%
          select(-G)%>%
          mutate(R_hat = predict(fit, newdata = .))

head(players)



qplot(R_hat, data = players , geom = "histogram", color = "firebrick")




head(Appearances)


### With our players data frame created we are going to add salaries 
## to those players 

head(Salaries)

players <- Salaries %>%
           filter(yearID == 2002)%>%
           select(playerID, salary)%>%
           right_join(players, by = "playerID")



head(players)

## we just performed a right Join so we added Salaries to the players 
## have in to account that is like writing right_join(Salaries, players, etc)

head(Appearances)


## remember hor to use the summarize_at thats specifing the specific 
### that we want to summarise at 


position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")

### we use that code to be able to know the most played posistion per player 



tmp_tab <- Appearances %>%
           filter(yearID == 2002)%>%
           group_by(playerID)%>%
           summarise_at(position_names, sum)%>%
           ungroup

tmp_tab



pos <- tmp_tab %>%
       select(all_of(position_names))%>%
       apply(., 1, which.max)

### In here we are creating an index of the position most played 
### to be able to use it to get the position played from our position_names
pos

position_names[pos]

## Right now we are going to have the most played position for each player 
## now lets get this new info in to a data frame 


help("str_to_upper")


dog <- "The quick brown dog"
str_to_upper(dog)


players <- data.frame(playerID = tmp_tab$playerID,
                        POS = position_names[pos])%>%
             mutate(POS = str_to_upper(str_remove(POS, "G_")))%>%
             filter(POS != "P")%>%
             right_join(players, by= "playerID")%>%
             filter(!is.na(POS) & !is.na(salary))


players        



## assesment 


bases <- c("BB", "singles", "doubles", "triples","HR")


coef_bases <- c(0.371,0.519,0.771,1.24,1.4)

coef <- data.frame(bases, coef_bases)

coef
## this are the average team performances 


team_1 <- c(2,4,1,0,1)

team_2 <- c(1,6,2,1,0)

sum(coef$coef_bases*team_1)

sum(coef$coef_bases*team_2)


### Assesment second part 



##Use the Teams data frame from the Lahman package. Fit a multivariate
##linear regression model to obtain the effects of BB and HR on Runs
##(R) in 1971. Use the tidy() function in the broom package to obtain 
##the results in a data frame.


library(Lahman)

data("Teams")

head(Teams)


asses <- Teams %>% filter( yearID == 1971)%>%
                  summarise(BB = BB/G, 
                            HR = HR/G,
                            R = R/G) %>%
                lm(R ~ BB + HR, data = .)



coef <- tidy(asses,conf.int = TRUE)

coef


coef %>% filter(p.value >= 0.05)



help("%in%")


## We are going to Repeat the above exercise to find the effects of
##BB and HR on runs (R) for every year from 1961 to 2018 using do() 
##and the broom package.

### Be carefull with start using the summarise statement on this kind of 
## cases beacuse in this cas you started using the summarise to create 
## statistics per game adn that was not what they asked you to do and 
## that caused the group by to work in the summarsed statistics but not 
# in the lm model 

res <- Teams %>% filter(yearID %in% 1961:2018)%>%
                     group_by(yearID)%>%
                     do(tidy(lm(R ~ BB+HR , data = . ),conf.int = TRUE))%>%
                     ungroup()
                     


asses_2 <- res %>% filter( term == "BB")%>%
                   ggplot(aes(yearID, estimate))+
                   geom_point()+
                   geom_smooth(method = "lm")


asses_2


## now we are going to see the effects of the year on the impact of the 
## BB on HR but this time we are going to see the effects of the year 
### on this estimate 

 asses_3 <- res %>% filter(term == "BB")%>%
            do(tidy(lm(estimate ~ yearID, data = .), conf.int = TRUE))


asses_3








