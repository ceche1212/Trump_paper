library(rjson)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(ggplot2)
library(zoo)
library(lmtest)
library(forecast)


#--------------------------2009 Trump Tweets----------------------------------------------------------------------------------------------------

trump2009<-fromJSON(file = "condensed_2009.json")

head(trump2009)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2009,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2009,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2009,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2009

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2009<-aaa

tweets2009$RT<-as.numeric(tweets2009$RT)

rm(aaa,bbb,ccc,trump2009)



#--------------------------2010 Trump Tweets----------------------------------------------------------------------------------------------------

trump2010<-fromJSON(file = "condensed_2010.json")

head(trump2010)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2010,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2010,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2010,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2010

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2010<-aaa

tweets2010$RT<-as.numeric(tweets2010$RT)

rm(aaa,bbb,ccc,trump2010)




#--------------------------2011 Trump Tweets----------------------------------------------------------------------------------------------------

trump2011<-fromJSON(file = "condensed_2011.json")

head(trump2011)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2011,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2011,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2011,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2011

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2011<-aaa

tweets2011$RT<-as.numeric(tweets2011$RT)

rm(aaa,bbb,ccc,trump2011)



#--------------------------2012 Trump Tweets----------------------------------------------------------------------------------------------------

trump2012<-fromJSON(file = "condensed_2012.json")

head(trump2012)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2012,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2012,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2012,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2012

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2012<-aaa

tweets2012$RT<-as.numeric(tweets2012$RT)

rm(aaa,bbb,ccc,trump2012)



#--------------------------2013 Trump Tweets----------------------------------------------------------------------------------------------------

trump2013<-fromJSON(file = "condensed_2013.json")

head(trump2013)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2013,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2013,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2013,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2013

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2013<-aaa

tweets2013$RT<-as.numeric(tweets2013$RT)

rm(aaa,bbb,ccc,trump2013)


#--------------------------2014 Trump Tweets----------------------------------------------------------------------------------------------------

trump2014<-fromJSON(file = "condensed_2014.json")

head(trump2014)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2014,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2014,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2014,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2014

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2014<-aaa

tweets2014$RT<-as.numeric(tweets2014$RT)

rm(aaa,bbb,ccc,trump2014)

#--------------------------2015 Trump Tweets----------------------------------------------------------------------------------------------------

trump2015<-fromJSON(file = "condensed_2015.json")

head(trump2015)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2015,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2015,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2015,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2015

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2015<-aaa

tweets2015$RT<-as.numeric(tweets2015$RT)

rm(aaa,bbb,ccc,trump2015)

----------------------#uploading 2016 data ---------------------------------------------------------------------------

trump2016<-fromJSON(file = "condensed_2016.json")

head(trump2016)

#getting tweets map contect of json$text and then transposing to avoid multiple variables

aaa<-data.frame(texto=map(trump2016,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2016,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2016,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2016

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2016<-aaa

tweets2016$RT<-as.numeric(tweets2016$RT)

rm(aaa,bbb,ccc,trump2016)

#-----------------------importing 2017 data-------------------------------------------------------------------------

trump2017<-fromJSON(file = "condensed_2017.json")

head(trump2017)

aaa<-data.frame(texto=map(trump2017,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2017,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2017,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2017

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2017<-aaa

tweets2017$RT<-as.numeric(tweets2017$RT)

rm(aaa,bbb,trump2017,ccc)

#-----------------------importing 2018 data-------------------------------------------------------------------------

trump2018<-fromJSON(file = "condensed_2018.json")

head(trump2018)

aaa<-data.frame(texto=map(trump2018,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2018,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2018,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-2018

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2018<-aaa

tweets2018$RT<-as.numeric(tweets2018$RT)

rm(aaa,bbb,trump2018,ccc)

#-----------------------importing 2019 data-------------------------------------------------------------------------

trump2019<-fromJSON(file = "condensed_2019.json")

head(trump2019)

aaa<-data.frame(texto=map(trump2019,~.x[["text"]][[1]])) %>% t()

#eliminating row.names

row.names(aaa)<-NULL

#transforming into data.frame

aaa<-as.data.frame(aaa)

#getting date data

bbb<-data.frame(texto=map(trump2019,~.x[["created_at"]][[1]])) %>% t()

#eliminating row.names

row.names(bbb)<-NULL

#transforming into data.frame

bbb<-as.data.frame(bbb)

#getting number of retweets data

ccc<-data.frame(texto=map(trump2019,~.x[["retweet_count"]][[1]])) %>% t()

#eliminating row.names

row.names(ccc)<-NULL

#transforming into data.frame

ccc<-as.data.frame(ccc)

ccc<-ccc %>% rename(RT=V1)

ccc<-ccc$RT

#joining both variables tweets and data

aaa$date<-bbb

aaa$RT<-ccc

#date data was imported as factor so we are unclassing it and transforming to character

aaa$date<-unclass(aaa$date)$`V1`

aaa$V1<-as.character(aaa$V1)

aaa$date<-as.character(aaa$date)

#extracting month info from data

aaa$Month<-str_sub(aaa$date,start = 5,end = 7)

#extracting day info from date

aaa$Day<-str_sub(aaa$date,start = 8,end = 10)

#creating the year variables

aaa$Year<-"2019"

#removing whitespace in all columns

aaa<-map_df(aaa,trimws)

#creating the new date column

aaa$date<-str_c(aaa$Year,"-",aaa$Month,"-",aaa$Day)

#parsing the text date column with lubridate to transform it into date

aaa$date<-ymd(aaa$date)

tweets2019<-aaa

tweets2019$RT<-as.numeric(tweets2019$RT)

rm(aaa,bbb,trump2019,ccc)

#------------------------------------joining all data---------------------------------------------------------------

trump_tweets<-bind_rows(tweets2009,tweets2010,tweets2011,tweets2012,tweets2013,tweets2014,tweets2015,tweets2016,tweets2017,tweets2018,tweets2019)

trump_tweets<-trump_tweets %>% arrange(.,date)

trump_tweets$V1<-str_to_lower(trump_tweets$V1)

rm(tweets2009,tweets2010,tweets2011,tweets2012,tweets2013,tweets2014,tweets2015,tweets2016,tweets2017,tweets2018,tweets2019)

save(trump_tweets,file = "trump_tweets_all.RData")

#variable looking for an oil word used

#"oil |gas |opec |pipeline |barrel |oil price|drilling |oil production|gas price|sanctions |sanctions |russia"

trump_tweets$oil_related_word<-grepl("oil|gas|opec|pipeline|barrel|oil price|drilling|oil production|gas price|sanctions|russia",
                                     trump_tweets$V1,ignore.case = TRUE)

trump_oil<-trump_tweets %>% group_by(date) %>% summarise(oil_word_count= sum(oil_related_word), Tweet_volume = n(),RT=sum(RT))

trump_oil$date<-ymd(trump_oil$date)

#----------------completing the dates since there are days without any tweet at all-------------------------------------

fechas<- seq(as.Date("2009/05/1"), as.Date("2019/06/01"), "days")

fechas <- as.data.frame(fechas)

fechas<-fechas %>% rename(date=fechas)

trump_oil<-left_join(fechas,trump_oil,by="date")

rm(fechas)

#---------------- handling missing values due to the completness of the dates -----------------------------------------

trump_oil$Tweet_volume[is.na(trump_oil$Tweet_volume)]<-0

trump_oil$RT[is.na(trump_oil$RT)]<-0

trump_oil$oil_word_count[is.na(trump_oil$oil_word_count)]<-0

#-------------importing Brent oil prices data------------------------------------------------------------------------

oil_price<-read.csv("brent-crude-oil-prices-10-year-daily-chart.csv")

#changin the date structure

oil_price$date<-mdy(oil_price$date)

#changing the name of the price variable

oil_price <- oil_price %>% rename(.,Brent = value)

#filtering for only values between 2016(trump became president) and 2019

oil_price<-oil_price %>% filter(.,date > ymd("2009/05/1") & date < ymd("2019-06-01"))


#-------------importing WTI oil prices data------------------------------------------------------------------------

WTI_oil_price<-read.csv("wti-crude-oil-prices-10-year-daily-chart.csv")

#changin the date structure

WTI_oil_price$date<-mdy(WTI_oil_price$date)

#changing the name of the price variable

WTI_oil_price <- WTI_oil_price %>% rename(.,WTI = value)

#filtering for only values between 2016(trump became president) and 2019

WTI_oil_price<-WTI_oil_price %>% filter(.,date > ymd("2009/05/1") & date < ymd("2019-06-01"))


#-------------importing Henry Hub Gas prices data------------------------------------------------------------------------


HH_Gas_price <- read.csv("HenryHubGasPrice.csv")

#renaming the date variable

HH_Gas_price <- HH_Gas_price %>% rename(date = ï..date)

#changin the date structure

HH_Gas_price$date<-mdy(HH_Gas_price$date)

#changing the name of the price variable

HH_Gas_price <- HH_Gas_price %>% rename(Gas = value)

#filtering for only values between 2016(trump became president) and 2019

HH_Gas_price<-HH_Gas_price %>% filter(.,date > ymd("2009/05/1") & date < ymd("2019-06-01"))

#------------------joining everything together-----------------------------------------------------------------

trump_oil<-left_join(trump_oil,oil_price,by="date")

trump_oil<-left_join(trump_oil,WTI_oil_price,by="date")

trump_oil<-left_join(trump_oil,HH_Gas_price,by="date")

trump_oil<-trump_oil %>% mutate(oil_keyword=ifelse(oil_word_count>0,TRUE,FALSE))

rm(HH_Gas_price,oil_price,WTI_oil_price)


#------------day after oil tweet variable creation-------------------------------------------------------------------------

# this line of code is just shifting one row ahead the oil keyword variable

day_after<-c(0,trump_oil$oil_keyword)

# eliminating the extra row but the last one

day_after<-day_after[-length(day_after)]

#joining the variable with the original data set

trump_oil$day_after<-day_after

trump_oil<-trump_oil %>% mutate(day_after=ifelse(day_after>0,TRUE,FALSE))

#2days after oil tweet variable creation

twoday_after<-c(0,0,trump_oil$oil_keyword)
twoday_after<-twoday_after[-length(twoday_after)]
twoday_after<-twoday_after[-length(twoday_after)]

trump_oil$two_day_after<-twoday_after

trump_oil<-trump_oil %>% mutate(two_day_after=ifelse(two_day_after>0,TRUE,FALSE))


#3days after oil tweet variable creation

threeday_after<-c(0,0,0,trump_oil$oil_keyword)
threeday_after<-threeday_after[-length(threeday_after)]
threeday_after<-threeday_after[-length(threeday_after)]
threeday_after<-threeday_after[-length(threeday_after)]

trump_oil$three_day_after<-threeday_after

trump_oil<-trump_oil %>% mutate(three_day_after=ifelse(three_day_after>0,TRUE,FALSE))

#4days after oil tweet variable creation

fouray_after<-c(0,0,0,0,trump_oil$oil_keyword)
fouray_after<-fouray_after[-length(fouray_after)] #1
fouray_after<-fouray_after[-length(fouray_after)] #2
fouray_after<-fouray_after[-length(fouray_after)] #3
fouray_after<-fouray_after[-length(fouray_after)] #4

trump_oil$four_day_after<-fouray_after

trump_oil<-trump_oil %>% mutate(four_day_after=ifelse(four_day_after>0,TRUE,FALSE))

#5days after oil tweet variable creation

fiveday_after<-c(0,0,0,0,0,trump_oil$oil_keyword)
fiveday_after<-fiveday_after[-length(fiveday_after)] #1
fiveday_after<-fiveday_after[-length(fiveday_after)] #2
fiveday_after<-fiveday_after[-length(fiveday_after)] #3
fiveday_after<-fiveday_after[-length(fiveday_after)] #4
fiveday_after<-fiveday_after[-length(fiveday_after)] #5

trump_oil$fiveday_after<-fiveday_after

trump_oil<-trump_oil %>% mutate(fiveday_after=ifelse(fiveday_after>0,TRUE,FALSE))

#6days after oil tweet variable creation

sixday_after<-c(0,0,0,0,0,0,trump_oil$oil_keyword)
sixday_after<-sixday_after[-length(sixday_after)] #1
sixday_after<-sixday_after[-length(sixday_after)] #2
sixday_after<-sixday_after[-length(sixday_after)] #3
sixday_after<-sixday_after[-length(sixday_after)] #4
sixday_after<-sixday_after[-length(sixday_after)] #5
sixday_after<-sixday_after[-length(sixday_after)] #6

trump_oil$sixday_after<-sixday_after

trump_oil<-trump_oil %>% mutate(sixday_after=ifelse(sixday_after>0,TRUE,FALSE))

#7days after oil tweet variable creation

sevenday_after<-c(0,0,0,0,0,0,0,trump_oil$oil_keyword)
sevenday_after<-sevenday_after[-length(sevenday_after)] #1
sevenday_after<-sevenday_after[-length(sevenday_after)] #2
sevenday_after<-sevenday_after[-length(sevenday_after)] #3
sevenday_after<-sevenday_after[-length(sevenday_after)] #4
sevenday_after<-sevenday_after[-length(sevenday_after)] #5
sevenday_after<-sevenday_after[-length(sevenday_after)] #6
sevenday_after<-sevenday_after[-length(sevenday_after)] #7

trump_oil$sevenday_after<-sevenday_after

trump_oil<-trump_oil %>% mutate(sevenday_after=ifelse(sevenday_after>0,TRUE,FALSE))

rm(day_after,twoday_after,threeday_after,fouray_after,fiveday_after,sixday_after,sevenday_after)

#------------day before oil tweet variable creation-------------------------------------------------------------------------

#one day before

day_before<-trump_oil$oil_keyword

day_before<-c(day_before[-1],FALSE)

trump_oil$day_before<-day_before

#Two days before

twoday_before<-trump_oil$oil_keyword

twoday_before<-twoday_before[-c(1,2)]

twoday_before<-c(twoday_before,FALSE,FALSE)

trump_oil$twoday_before<-twoday_before

#Three days before

threeday_before<-trump_oil$oil_keyword

threeday_before<-threeday_before[-c(1,2,3)]

threeday_before<-c(threeday_before,FALSE,FALSE,FALSE)

trump_oil$threeday_before<-threeday_before

#Four days before

fourday_before<-trump_oil$oil_keyword

fourday_before<-fourday_before[-c(1,2,3,4)]

fourday_before<-c(fourday_before,FALSE,FALSE,FALSE,FALSE)

trump_oil$fourday_before<-fourday_before

#Five days before

fiveday_before<-trump_oil$oil_keyword

fiveday_before<-fiveday_before[-c(1,2,3,4,5)]

fiveday_before<-c(fiveday_before,FALSE,FALSE,FALSE,FALSE,FALSE)

trump_oil$fiveday_before<-fiveday_before

#Six days before

sixday_before<-trump_oil$oil_keyword

sixday_before<-sixday_before[-c(1,2,3,4,5,6)]

sixday_before<-c(sixday_before,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)

trump_oil$sixday_before<-sixday_before

#seven days before

sevenday_before<-trump_oil$oil_keyword

sevenday_before<-sevenday_before[-c(1,2,3,4,5,6,7)]

sevenday_before<-c(sevenday_before,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE)

trump_oil$sevenday_before<-sevenday_before

rm(day_before,twoday_before,threeday_before,fourday_before,fiveday_before,sixday_before,sevenday_before)

trump_oil_backup<-trump_oil

#-------------joining all variables together-------------------------------------------------------------------------------

trump_oil <- trump_oil %>%
        mutate(LAG=case_when(
                oil_keyword==TRUE ~ "Zero",
                day_after==TRUE & 
                        oil_keyword==FALSE  ~ 
                        "One",
                two_day_after==TRUE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Two",
                three_day_after==TRUE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE  ~ 
                        "Three",
                four_day_after==TRUE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Four",
                fiveday_after==TRUE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~
                        "Five",
                sixday_after==TRUE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~
                        "Six",
                sevenday_after==TRUE & 
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~
                        "Seven",
                day_before==TRUE & 
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "One_before",
                twoday_before==TRUE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Two_before",
                threeday_before==TRUE &
                        twoday_before==FALSE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Three_before",
                fourday_before==TRUE &
                        threeday_before==FALSE &
                        twoday_before==FALSE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Four_before",
                fiveday_before==TRUE &
                        fourday_before==FALSE &
                        threeday_before==FALSE &
                        twoday_before==FALSE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Five_before",
                sixday_before==TRUE &
                        fiveday_before==FALSE &
                        fourday_before==FALSE &
                        threeday_before==FALSE &
                        twoday_before==FALSE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Six_before",
                sevenday_before==TRUE &
                        sixday_before==FALSE &
                        fiveday_before==FALSE &
                        fourday_before==FALSE &
                        threeday_before==FALSE &
                        twoday_before==FALSE &
                        day_before==FALSE &
                        sevenday_after==FALSE &
                        sixday_after==FALSE & 
                        fiveday_after==FALSE & 
                        four_day_after==FALSE & 
                        three_day_after==FALSE & 
                        two_day_after==FALSE & 
                        day_after==FALSE & 
                        oil_keyword==FALSE ~ 
                        "Seven_before",
                sevenday_before==FALSE ~ "No_oil_Tweet"))

trump_oil<-trump_oil %>% select(date,oil_word_count,oil_keyword,Tweet_volume,RT,Brent,WTI,Gas,LAG)

# variable that distingushes between before and after 2016

trump_oil<-trump_oil %>% mutate(Period = ifelse(year(date)<2016,"Before","After"))

save(trump_oil,file = "trump_oil_all.RData")


#eda

trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_Vol=sum(Tweet_volume,na.rm=TRUE),
                  Oil_Tweets=sum(oil_keyword==TRUE,na.rm=TRUE),
                  percentage_Oil_Tweets=(sum(oil_keyword==TRUE,na.rm=TRUE)/sum(Tweet_volume,na.rm = TRUE))*100) %>%
        knitr::kable()


trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_Vol=sum(Tweet_volume,na.rm=TRUE),
                  Oil_Tweets=sum(oil_keyword==TRUE,na.rm=TRUE),
                  percentage_Oil_Tweets=(sum(oil_keyword==TRUE,na.rm=TRUE)/sum(Tweet_volume,na.rm = TRUE))*100) %>%
        ggplot(.,aes(y=Oil_Tweets,x=as.factor(Year)))+
        geom_bar(stat = "identity",fill="red",alpha="0.4",color="blue")+
        labs(y="Oil related Tweets",
             title = "Trump Oil related Tweets over time",
             x="Year")+
        ggthemes::theme_tufte()


trump_oil %>% mutate(Year=year(date)) %>%
        group_by(Year) %>%
        summarise(Tweet_Vol=sum(Tweet_volume,na.rm=TRUE),
                  Oil_Tweets=sum(oil_keyword==TRUE,na.rm=TRUE),
                  percentage_Oil_Tweets=(sum(oil_keyword==TRUE,na.rm=TRUE)/sum(Tweet_volume,na.rm = TRUE))*100) %>%
        ggplot(.,aes(y=percentage_Oil_Tweets,x=as.factor(Year)))+
        geom_bar(stat = "identity",fill="blue",alpha="0.4",color="red")+
        labs(y="Oil related Tweets percentage",
             title = "Trump Oil related Tweets over time",
             x="Year")+
        ggthemes::theme_tufte()


#t-test oil word vs brent oil price Before presidency

trump_oil %>% filter(Period=="Before") %>% 
        t.test(Brent~oil_keyword,data = .)

#t-test oil word vs brent oil price after presidency

trump_oil %>% filter(Period=="After") %>% 
        t.test(Brent~oil_keyword,data = .)

#t-test oil word vs WTI oil price Before presidency

trump_oil %>% filter(Period=="Before") %>% 
        t.test(WTI~oil_keyword,data = .)

#t-test oil word vs WTI oil price After presidency

trump_oil %>% filter(Period=="After") %>% 
        t.test(WTI~oil_keyword,data = .)


#t-test oil word vs Henry Hub gas price  Before presidency

trump_oil %>% filter(Period=="Before") %>% 
        t.test(Gas~oil_keyword,data = .)

#t-test oil word vs Henry Hub gas price  After presidency

trump_oil %>% filter(Period=="After") %>% 
        t.test(Gas~oil_keyword,data = .)




trump_oil %>% ggplot(.,aes(y=Brent,x=oil_keyword,fill=oil_keyword))+geom_boxplot(alpha=0.5)+
        labs(title = "Brent Oil Prices vs Trump's Oil tweets",
             x= "Oil Tweet",
             y="Brent spot Oil Price (USD per barrel)")+
        facet_grid(. ~ Period)+
        ggthemes::theme_tufte()

trump_oil %>% ggplot(.,aes(y=WTI,x=oil_keyword,fill=oil_keyword))+geom_boxplot(alpha=0.5)+
        scale_fill_manual(values=c("blue","red"))+
        labs(title = "WTI Oil Prices vs Trump's Oil tweets",
             x= "Oil Tweet",
             y="WTI spot Oil Price (USD per barrel)")+
        facet_grid(. ~ Period)+
        ggthemes::theme_tufte()

trump_oil %>% ggplot(.,aes(y=Gas,x=oil_keyword,fill=oil_keyword))+geom_boxplot(alpha=0.5)+
        labs(title = "Henry Hub Gas Prices vs Trump's Oil tweets",
             x= "Oil Tweet",
             y="Henry Hub spot Gas Prices (USD per TCF)")+
        facet_grid(. ~ Period)+
        ggthemes::theme_tufte()




data.frame(table(cut(trump_oil$Brent,3),trump_oil$oil_keyword)) %>%
        filter(Var2==TRUE) %>%
        select(.,-Var2) %>%
        dplyr::rename(.,Price_range=Var1,Tweets=Freq) %>%
        knitr::kable()


data.frame(table(cut(trump_oil$Brent,3),trump_oil$oil_keyword)) %>%
        filter(Var2==TRUE) %>%
        select(.,-Var2) %>%
        dplyr::rename(.,Price_range=Var1,Tweets=Freq) %>% 
        ggplot(aes(x=Price_range,y=Tweets,fill=Price_range))+geom_bar(stat="identity")+
        labs(y="Trump Oil Tweets",
             title = "Trump Tweets distribution by price oil range",
             x="Oil price Ranges")+ggthemes::theme_tufte()


chisq.test(table(cut(trump_oil$Brent,3),trump_oil$oil_keyword))


data.frame(chisq.test(table(cut(trump_oil$Brent,3),trump_oil$oil_keyword))$expected) %>% 
        select(.,TRUE.) %>% mutate(Price_range=rownames(.)) %>% dplyr::rename(.,Chisq_Expected_Tweets=TRUE.) %>%
        select(.,Price_range,Chisq_Expected_Tweets) %>% knitr::kable()



data.frame(chisq.test(table(cut(trump_oil$Brent,3),trump_oil$oil_keyword))$expected) %>% 
        select(.,TRUE.) %>% mutate(Price_range=rownames(.)) %>% dplyr::rename(.,Tweets=TRUE.) %>%
        select(.,Price_range,Tweets) %>%
        ggplot(aes(x=Price_range,y=Tweets,fill=Price_range))+geom_bar(stat="identity")+
        labs(y="Expected Tweets by Chi-square Dist.",
             title = "Chi-square expected tweets distribution by price oil range",
             x="Oil price Ranges")+ggthemes::theme_tufte()



trump_oil %>% 
        filter(Period=="Before") %>%
        group_by(LAG) %>% 
        summarise(n(),BrentP=mean(Brent,na.rm=T),WTIP=mean(WTI,na.rm=T),GasP=mean(Gas,na.rm=T),
                  BrentSD=sd(Brent,na.rm=T),WTISD=sd(WTI,na.rm=T),GasSD=sd(Gas,na.rm=T)) %>% 
        arrange(.,desc(BrentP)) %>% knitr::kable()

trump_oil %>% 
        filter(Period=="After") %>%
        group_by(LAG) %>% 
        summarise(n(),BrentP=mean(Brent,na.rm=T),WTIP=mean(WTI,na.rm=T),GasP=mean(Gas,na.rm=T),
                  BrentSD=sd(Brent,na.rm=T),WTISD=sd(WTI,na.rm=T),GasSD=sd(Gas,na.rm=T)) %>% 
        arrange(.,desc(BrentP)) %>% knitr::kable()




trump_oil %>% group_by(Period,LAG) %>% 
        summarise(BrentP=mean(Brent,na.rm=T),WTIP=mean(WTI,na.rm=T),GasP=mean(Gas,na.rm=T),
                  BrentSD=sd(Brent,na.rm=T),WTISD=sd(WTI,na.rm=T),GasSD=sd(Gas,na.rm=T)) %>% 
        arrange(.,desc(BrentP)) %>%
        mutate(Lag_num=case_when(LAG=="Zero" ~ 0,
                                 LAG=="One" ~ 1,
                                 LAG=="Two" ~ 2,
                                 LAG=="Three" ~ 3,
                                 LAG=="Four" ~ 4,
                                 LAG=="Five" ~ 5,
                                 LAG=="Six" ~ 6,
                                 LAG=="Seven" ~ 7,
                                 LAG=="One_before" ~ -1,
                                 LAG=="Two_before" ~ -2,
                                 LAG=="Three_before" ~ -3,
                                 LAG=="Four_before" ~ -4,
                                 LAG=="Five_before" ~ -5,
                                 LAG=="Six_before" ~ -6,
                                 LAG=="Seven_before" ~ -7,
                                 LAG=="No_oil_Tweet" ~ -8,))%>%
        ggplot(.,aes(x=as.factor(Lag_num),y=BrentP,color=Period))+
        geom_point(size=7,alpha=0.5)+
        scale_color_manual(values=c("blue","red"))+
        facet_grid(.~Period)+
        geom_errorbar(aes(ymin=BrentP-BrentSD, ymax=BrentP+BrentSD), width=.2,
                      position=position_dodge(0.05),alpha=0.3)+
        labs(title = "Brent Oil Price vs Trump Oil tweets after he became president",
             x="Lag (days)",
             y="Brent Oil spot prices (USD per barrel)")+
        ggthemes::theme_igray()





trump_oil %>% group_by(Period,LAG) %>% 
        summarise(BrentP=mean(Brent,na.rm=T),WTIP=mean(WTI,na.rm=T),GasP=mean(Gas,na.rm=T),
                  BrentSD=sd(Brent,na.rm=T),WTISD=sd(WTI,na.rm=T),GasSD=sd(Gas,na.rm=T)) %>% 
        arrange(.,desc(BrentP)) %>%
        mutate(Lag_num=case_when(LAG=="Zero" ~ 0,
                                 LAG=="One" ~ 1,
                                 LAG=="Two" ~ 2,
                                 LAG=="Three" ~ 3,
                                 LAG=="Four" ~ 4,
                                 LAG=="Five" ~ 5,
                                 LAG=="Six" ~ 6,
                                 LAG=="Seven" ~ 7,
                                 LAG=="One_before" ~ -1,
                                 LAG=="Two_before" ~ -2,
                                 LAG=="Three_before" ~ -3,
                                 LAG=="Four_before" ~ -4,
                                 LAG=="Five_before" ~ -5,
                                 LAG=="Six_before" ~ -6,
                                 LAG=="Seven_before" ~ -7,
                                 LAG=="No_oil_Tweet" ~ -8,))%>%
        ggplot(.,aes(x=as.factor(Lag_num),y=WTIP,color=Period))+
        scale_color_manual(values=c("darkgreen","purple"))+
        geom_point(size=7,alpha=0.5)+
        facet_grid(.~Period)+
        geom_errorbar(aes(ymin=WTIP-WTISD, ymax=WTIP+WTISD), width=.2,
                      position=position_dodge(0.05),color="black")+
        labs(title = "WTI Oil Price vs Trump Oil tweets after he became president",
             x="Lag (days)",
             y="WTI Oil spot prices (USD per barrel)")+
        ggthemes::theme_igray()




trump_oil %>% group_by(Period,LAG) %>% 
        summarise(BrentP=mean(Brent,na.rm=T),WTIP=mean(WTI,na.rm=T),GasP=mean(Gas,na.rm=T),
                  BrentSD=sd(Brent,na.rm=T),WTISD=sd(WTI,na.rm=T),GasSD=sd(Gas,na.rm=T)) %>% 
        arrange(.,desc(BrentP)) %>%
        mutate(Lag_num=case_when(LAG=="Zero" ~ 0,
                                 LAG=="One" ~ 1,
                                 LAG=="Two" ~ 2,
                                 LAG=="Three" ~ 3,
                                 LAG=="Four" ~ 4,
                                 LAG=="Five" ~ 5,
                                 LAG=="Six" ~ 6,
                                 LAG=="Seven" ~ 7,
                                 LAG=="One_before" ~ -1,
                                 LAG=="Two_before" ~ -2,
                                 LAG=="Three_before" ~ -3,
                                 LAG=="Four_before" ~ -4,
                                 LAG=="Five_before" ~ -5,
                                 LAG=="Six_before" ~ -6,
                                 LAG=="Seven_before" ~ -7,
                                 LAG=="No_oil_Tweet" ~ -8,))%>%
        ggplot(.,aes(x=as.factor(Lag_num),y=GasP,color=Period))+
        scale_color_manual(values=c("thistle4","steelblue4"))+
        geom_point(size=7,alpha=0.5)+
        facet_grid(.~Period)+
        geom_errorbar(aes(ymin=GasP-GasSD, ymax=GasP+GasSD), width=.2,
                      position=position_dodge(0.05))+
        labs(title = "Henry Hub gas price vs Trump Oil tweets",
             x="Lag (days)",
             y="Henry Hub gas spot prices (USD per TCF)")+
        ggthemes::theme_igray()


trump_oil %>% filter(Period=="After") %>%
        aov(Brent~LAG,data = .) %>% summary()




a<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Seven") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

b<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Six") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

c<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Five") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

d<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Four") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

e<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Three") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

f<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="Two") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                  adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                  pval_WTI = t.test(WTI ~ LAG)$p.value,
                  adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                  pval_Gas = t.test(Gas ~ LAG)$p.value,
                  adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

g<-trump_oil %>% filter(Period=="After") %>%
        filter(LAG=="Zero" | LAG=="One") %>% 
        summarise(pval_Brent = t.test(Brent ~ LAG)$p.value,
                   adj_pval_Brent = ifelse(t.test(Brent ~ LAG)$p.value*15 > (1),1,t.test(Brent ~ LAG)$p.value*15),
                   pval_WTI = t.test(WTI ~ LAG)$p.value,
                   adj_pval_WTI = ifelse(t.test(WTI ~ LAG)$p.value*15 > (1),1,t.test(WTI ~ LAG)$p.value*15),
                   pval_Gas = t.test(Gas ~ LAG)$p.value,
                   adj_pval_Gas = ifelse(t.test(Gas ~ LAG)$p.value*15 > (1),1,t.test(Gas ~ LAG)$p.value*15))

bind_rows(g,f,e,d,c,b,a) %>% mutate(Lag=seq(1:7))


#granger causality test after


Brent_after <- trump_oil %>% filter(Period=="After") %>% select(Brent)

Brent_after <- Brent_after$Brent

WTI_after <- trump_oil %>% filter(Period=="After") %>% select(WTI)

WTI_after <- WTI_after$WTI

Gas_after<- trump_oil %>% filter(Period=="After") %>% select(Gas)

Gas_after<-Gas_after$Gas

Tweet_oil_after <- trump_oil %>% filter(Period=="After") %>% select(oil_word_count)

Tweet_oil_after <- Tweet_oil_after$oil_word_count


granger_causality_brent <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(Brent_after~Tweet_oil_after,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

(Granger_causal_test_after <- data.frame(
        Lag = c(1:10) ,
        pvalue_Brent = granger_causality_brent
        
))

granger_causality_wti <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(WTI_after~Tweet_oil_after,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

Granger_causal_test_after$pvalue_WTI <- granger_causality_wti

granger_causality_gas <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(Gas_after~Tweet_oil_after,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

Granger_causal_test_after$pvalue_Gas <- granger_causality_gas

Granger_causal_test_after %>% knitr::kable()


#granger causality test before

Brent_before <- trump_oil %>% filter(Period=="Before") %>% select(Brent)

Brent_before <- Brent_before$Brent

WTI_before <- trump_oil %>% filter(Period=="Before") %>% select(WTI)

WTI_before <- WTI_before$WTI

Gas_before<- trump_oil %>% filter(Period=="Before") %>% select(Gas)

Gas_before<-Gas_before$Gas

Tweet_oil_before <- trump_oil %>% filter(Period=="Before") %>% select(oil_word_count)

Tweet_oil_before <- Tweet_oil_before$oil_word_count


granger_causality_brent_before <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(Brent_before~Tweet_oil_before,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

(Granger_causal_test_before <- data.frame(
        Lag = c(1:10) ,
        pvalue_Brent = granger_causality_brent_before
        
))

granger_causality_wti_before <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(WTI_before~Tweet_oil_before,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

Granger_causal_test_before$pvalue_WTI <- granger_causality_wti_before

granger_causality_gas_before <- map_dbl(1:10,  function(k){
        model <- lmtest::grangertest(Gas_before~Tweet_oil_before,order=k,na.action=na.omit)
        model$`Pr(>F)`[2]
}) 

Granger_causal_test_before$pvalue_Gas <- granger_causality_gas_before

Granger_causal_test_before %>% knitr::kable()


bind_rows(Granger_causal_test_before,Granger_causal_test_after,.id = "Period") %>%
        mutate(Period=case_when(Period==1 ~ "Before", Period==2 ~ "After"))  %>% 
        rename(Gas=pvalue_Gas,Brent=pvalue_Brent,WTI=pvalue_WTI) %>%
        tidyr::gather(Brent,WTI,Gas,key="Commodity",value="Pvalue") %>%
        ggplot(.,aes(x=as.factor(Lag),y=Pvalue,color=Commodity,shape=Commodity))+
        scale_color_manual(values=c("Blue","Red","Black"))+
        geom_point(size=3,alpha=0.8)+
        geom_line(aes(x=Lag,y=Pvalue,color=Commodity),alpha=0.8)+
        ylim(0,1)+
        geom_hline(yintercept = 0.05,color="black",linetype = 'dashed')+
        geom_hline(yintercept = 0.1,color="red",linetype = 'dashed')+
        labs(title = "Granger causality test results commodity prices as function of trump oil tweets after 2016",
             x="Lag (Days)",
             y="p-value of Granger causality test")+
        facet_grid(.~Period)+
        ggthemes::theme_igray()
        
