install.packages("dplyr")
install.packages("xlsx")
install.packages("plyr")
install.packages("ggplot2")
library("plyr")
library("dplyr")
library("xlsx")
library("ggplot2")
library("plotrix")
library("MASS")
getwd()
setwd("C:\\Users\\Yash Kumar\\Desktop\\R_language_tutorial")
#Read the first fle
df1<-read.csv("zomato.csv")
df1


#a.)Find  the no of observation and variable of first dataframe
m=dim(df1)
print(m)

##structure of the first file
str(df1)

##read the second file country code one

df2<-read.xlsx("Country-Code.xlsx",sheetIndex  = 1)
print(df2)
##merge the above two dataset
df3<-merge(df1, df2, by.x="Country.Code")
head(df3)
##dimension of merge dataframe
dim(df3)
##Check the NA value in dataframe
is.na(df3)
##Total no of NA value in data frame 
sum(is.na(df3))
##column_name of the dataframe
colnames(df3)
##Summary of the dataframe
summary(df3)
##To find the record with each country
country<-df3 %>%
  group_by(Country) %>%
  dplyr::summarize(count=n())

test<-arrange(country,country$count)
test
#Observations:- maximum record are from India and then from USA and then UK


#top 5 indian city with maximum number of zomato Rest.


india<-filter(df3,df3$Country=="India")
city_count<-india %>%
  group_by(City) %>%
 dplyr::summarize(n=n()) 

five_city<-tail(arrange(city_count,city_count$n),15)
ggplot(data=tail(five_city,5),aes(x=City,y=n))+
  geom_bar(stat = "identity",position=position_dodge(), width=0.5, color="blue",fill="steelblue")+
labs(title = "Top 5 Indian City with maximum number of Zomato Restaurant",x="City",y="Number of Restaurant")

#observations:-New Delhi has the maximum number of Restaurant


##Which currency is used in which country

ser<-df3 %>%
  group_by(Currency,Country) %>%
  dplyr::summarize(count=n())

ggplot(data=ser,aes(x=Country,y=Currency))+
  geom_bar(stat = "identity",position=position_dodge(), width=0.5, color="blue",fill="steelblue")+
  labs(title = "Which Currency is used by which Country",x="Country",y="Currency")



##which country offer online deliver
delivery<-df3 %>%
  group_by(Country,Has.Online.delivery) %>%
  summarise(count=n())
count1<-which(delivery$Has.Online.delivery=="Yes")
count<-delivery[count1,]
count
#pie Chart country offer online delivry
labs<-c("India","UAE")
pie(count,labels = labs,main = "Country which offer online Delivery")


#Observations
#online delivery is available only  in India and UAE .

##which offer  online deliver service in india
deliver<-df3 %>%
  filter(Country.Code==1) %>%
  unique() %>%
  group_by(City,Has.Online.delivery) %>%
  dplyr::summarize(count=n())



p<-ggplot(data=deliver,aes(x=City,y=Has.Online.delivery))+
  geom_bar(stat = "identity",position=position_dodge(), width=0.5, color="blue",fill="steelblue")+
  labs(title = "City which offer Online Deliver service in India",x="City",y="Has.Online.delivery")






p
p+coord_flip()
#19 city offer this service

 ## top 5 votes in india
top_vote<-df3 %>%
  filter(Country.Code==1) %>%
  unique() %>%
  group_by(City) %>%
  dplyr::summarize(n=n())
sort<-arrange(top_vote,n)
tail(sort)
p<-ggplot(data=tail(sort,5),aes(x=City,y=n))+
  geom_bar(stat = "identity")
p



##relation between aggregate rating, rating color,rating text,rating count
rating=df3 %>%
  group_by(Aggregate.rating,Rating.color,Rating.text) %>%
  dplyr::summarize(count=n())
##graph between rating count and aggregate.rating
p<-ggplot(data=rating,aes(x=Aggregate.rating,y=count))+
  geom_bar(stat = "identity")
p
##observation
##1. when rating are between 4.5 to 4.9 -> Excellent
##2. when rating are between 4.0 to 4.4 -> Very Good
##3. when rating are between 3.5 to 3.9 -> Good
##4. when rating are between 2.5 to 3.4 -> Average
##5. when rating are between 1.0 to 2.4 -> Poor


##graph between 
p<-ggplot(data=rating,aes(x=Rating.text,y=Aggregate.rating))+
  geom_bar(stat = "identity")
p
#Observation
#Note:- white is invisible



##Top 3 country which  has the cheaper food 
food<-df3 %>%
  group_by(Average.Cost.for.two,Country) %>%
  dplyr::summarize(n=n())
food
sort<-arrange(food)
p<-ggplot(data=head(sort,3),aes(x=Country,y=Average.Cost.for.two))+
  geom_bar(stat = "identity")+
  geom_bar(stat="identity", color="blue", fill="white")

p
#Observations:- In india and United States have the cheapest food

##which offer  booking service service in india
booking<-df3 %>%
  filter(Country.Code==1) %>%
  
  unique() %>%
  group_by(City,Has.Table.booking) %>%
  dplyr::summarize(count=n())


ggplot(booking,aes(x="City",y="",fill="blue"))+
  geom_col(color="black")+
  coord_polar(theta = "Has.Table.booking")

p<-ggplot(data=booking,aes(x=City,y=Has.Table.booking))+
  geom_bar(stat = "identity")+
  geom_bar(stat = "identity",position=position_dodge(), width=0.5, color="blue",fill="steelblue")+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

p+coord_flip()

#observations:-
 # Only 12 coutry offer Table booking service in india

a<-seq.int(10,100)
a
c<-sample(1,20,replace = TRUE)
c
