install.packages("titanic")
library(titanic)
2+7
2**7
sqrt(2)
sum(1:5)
pi
x <- sqrt(2)
print(x)
x
print(x,10)
x^2
x
x <- x^2
x
install.packages(titanic3)
titanic <- data.frame(
  pclass=rep(c("1st","2nd","3rd"),c(4,2,4)),
  survived=c(1,0,0,0,1,1,0,0,0,0),
  name=c("Allison, Master. Hudson Trevor","Dulles, Mr. William Crothers","McCarthy, Mr. Timothy J","Walker, Mr. William Anderson", "Duran y More, Miss. Asuncion", "Mellinger, Miss. Madeleine Viol","Abbott, Master. Eugene Joseph", "Calic, Mr. Petar","Flynn, Mr. James", "Johnston, Miss. Catherine Helen"),
  age= c(0.9167,39,54,47,27,13,13,17,NA,NA),
  fare=c(151.55,29.7,51.8625,34.0208,13.8583,19.5,20.25,8.6625,7.75,23.4))
titanic$age
titanic$age[6,10]
titanic$agec[c(6,10)]
titanic$age[c(6,7,8,9,10)]
6:10
seq(6,10,by=1) 
seq(1,10,by =2)
titanic$age
titanic[6,4]
titanic[2:3,c(3,1)]
titanic[c(2,3), c("name","pclav")
vec(11:30)
seq(11:30)
vec <- 11:30
vec
vec <- seq(11:30)
vec
vec[7]
vec(11:30)
vec[-15]
vec[-c(2,5)]
vec <- 11:30
vec
-2<2
c("Thinh","Ronald")
letters
titanic$fare
titanic$fare[c(TRUE,FALSE,TRUE,TRUE, FALSE, TRUE,FALSE,TRUE,TRUE, FALSE)] 
titanic$fare>40
titanic$pclass[titanic$fare>40]
"Thinh" > "Ronald"
as.character(seq(0,2,by=0.25))
as.numeric(c(FALSE,TRUE))
titanic$fare[titanic$fare>10 & titanic$fare<40]
titanic$pclass=="1st" 
!(titanic$fare < 10 | titanic$fare > 40)

x <- 12.3
class(x)
today <- as.Date("2025/05/03")
class(today)
df <- data.frame(name = c("Alice", "Bob"),
                 age = c(25, 30),
                 score = c(90, 85))
str(df)
df
vec <- 11:30
vec
vec[7]
vec[-15]
is.na(vec)
titanic$age
titanic
tydiverse
library(tidyverse)
library(tidyverse)
install.packages("tidyverse")
crime = crime %>% mutate(
  yr = year + 1900,
  pop = population / 1000
  