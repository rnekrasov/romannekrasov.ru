#******************************************
library(twitteR)
library(ngram)
library(tm)
library(SnowballC) #stemming Porters
library(RSentiment)
library(Quandl)
library(quantmod)
setup_twitter_oauth("zngU250W8JuxNSXnAMn4bJdSH", "4ox4xnnIZuQanRZ2xkXrNgXIxwHuTJPv09i80NnV0ADv4umPQW","2669848952-NVwkw7wRLbTYXX7lgQe5GGXH8ZHOjhAGB457jp5","OH1isZMxNKcrQ93CNTXod3gMFIk2WeXMrH4nSGRf5ZLuF")
word1<-"#eurusd"
ticker1<-"EUR/USD"
word2<-"#usdrub"
ticker2<-"USD/RUB"
word3<-"#usdcad"
ticker3<-"USD/CAD"
word4<-"#gbpusd"
ticker4<-"GBP/USD"
word5<-"#oil"
ticker5<-"BRENT"
ticker6<-getSymbols("CL",src="google")
ticker6<-tail(CL,1)
ticker6<-as.numeric(ticker6$CL.Close)
#getSymbols(ticker1, src="oanda")
#getSymbols(ticker2, src="oanda")
#getSymbols(ticker3, src="oanda")
#getSymbols(ticker4, src="oanda")
#price1<-EURUSD[length(EURUSD)]
#price2<-USDRUB[length(USDRUB)]
#price3<-USDCAD[length(USDCAD)]
#price4<-GBPUSD[length(GBPUSD)]
price1<-0
price2<-0
price3<-0
price4<-0
price5<-0
twit1<-searchTwitter(word1, n=1500,lang="en",resultType="recent")
twit2<-searchTwitter(word2, n=1500,lang="en",resultType="recent")
twit3<-searchTwitter(word3, n=1500,lang="en",resultType="recent")
twit4<-searchTwitter(word4, n=1500,lang="en",resultType="recent")
twit5<-searchTwitter(word5, n=1500,lang="en",resultType="recent")
twit1<-twListToDF(twit1)
twit2<-twListToDF(twit2)
twit3<-twListToDF(twit3)
twit4<-twListToDF(twit4)
twit5<-twListToDF(twit5)
a1<-concatenate(twit1$text)
a2<-concatenate(twit2$text)
a3<-concatenate(twit3$text)
a4<-concatenate(twit4$text)
a5<-concatenate(twit5$text)
b1<-MC_tokenizer(twit1$text)
b2<-MC_tokenizer(twit2$text)
b3<-MC_tokenizer(twit3$text)
b4<-MC_tokenizer(twit4$text)
b5<-MC_tokenizer(twit5$text)
b1<-removeWords(b1, stopwords("english"))
b2<-removeWords(b2, stopwords("english"))
b3<-removeWords(b3, stopwords("english"))
b4<-removeWords(b4, stopwords("english"))
b5<-removeWords(b5, stopwords("english"))

#steming and n-gram

#c1<-stemDocument(a1)
#a1<-ngram(a1, n = 17, sep = "")
#b1<-get.phrasetable(a1)

#sentiment analisys

#calculate_sentiment(twit$text)
#calculate_score(twit$text)
sent1<-calculate_total_presence_sentiment(twit1$text)
sent2<-calculate_total_presence_sentiment(twit2$text)
sent3<-calculate_total_presence_sentiment(twit3$text)
sent4<-calculate_total_presence_sentiment(twit4$text)
sent5<-calculate_total_presence_sentiment(twit5$text)

#frequency analisys

#d1<-termFreq(b1)
#d1<-sort(d1,decreasing = TRUE)

#write to database

cur_date<-Sys.Date()
cur_time<-Sys.time()
#Quandl.api_key("7FYDFv-wKreb1rKbEzqB")
query1<-data.frame(word1,
              sent1[2],sent1[4],sent1[6],sent1[8],sent1[10],sent1[12],cur_time,
              price1,ticker1)
colnames(query1) <- c("word", "sarcasm","very negative",
                 "negative","neutral","positive","very positive",
                 "time","price","ticker")
query2<-data.frame(word2,
                   sent2[2],sent2[4],sent2[6],sent2[8],sent2[10],sent2[12],cur_time,
                   price2,ticker2)
colnames(query2) <- c("word", "sarcasm","very negative",
                      "negative","neutral","positive","very positive",
                      "time","price","ticker")
query3<-data.frame(word3,
                   sent3[2],sent3[4],sent3[6],sent3[8],sent3[10],sent3[12],cur_time,
                   price3,ticker3)
colnames(query3) <- c("word", "sarcasm","very negative",
                      "negative","neutral","positive","very positive",
                      "time","price","ticker")
query4<-data.frame(word4,
                   sent4[2],sent4[4],sent4[6],sent4[8],sent4[10],sent4[12],cur_time,
                   price4,ticker4)
colnames(query4) <- c("word", "sarcasm","very negative",
                      "negative","neutral","positive","very positive",
                      "time","price","ticker")
query5<-data.frame(word5,
                   sent5[2],sent5[4],sent5[6],sent5[8],sent5[10],sent5[12],cur_time,
                   ticker6,ticker5)
colnames(query5) <- c("word", "sarcasm","very negative",
                      "negative","neutral","positive","very positive",
                      "time","price","ticker")
#dbWriteTable(con, "nlp",query1,append=TRUE,row.names = FALSE)
#dbWriteTable(con, "nlp",query2,append=TRUE,row.names = FALSE)
#dbWriteTable(con, "nlp",query3,append=TRUE,row.names = FALSE)
#dbWriteTable(con, "nlp",query4,append=TRUE,row.names = FALSE)
dbWriteTable(con, "nlp",query5,append=TRUE,row.names = FALSE)

#read from database

data_NLP <- dbGetQuery(con, "SELECT * from nlp order by time")
data_NLP2<-data.frame(data_NLP)

#solve the sentiment
sentiment<-data_NLP$`positive`/data_NLP$`negative`
g<-data.frame(data_NLP$word,data_NLP$time,sentiment,data_NLP$`positive`,data_NLP$`negative`)
tail(g,20)

#******************************************
#******************************************           
#oil frequency analisys & n-gram

library(twitteR)
library(ngram)
library(tm)
library(SnowballC)
setup_twitter_oauth("zngU250W8JuxNSXnAMn4bJdSH", "4ox4xnnIZuQanRZ2xkXrNgXIxwHuTJPv09i80NnV0ADv4umPQW","2669848952-NVwkw7wRLbTYXX7lgQe5GGXH8ZHOjhAGB457jp5","OH1isZMxNKcrQ93CNTXod3gMFIk2WeXMrH4nSGRf5ZLuF")
twit=searchTwitter("#russia", n=1500,lang="en",resultType="recent")
#oil, usdrub, tax, russia, market, stock, forex, brent, wti, fed
twit=twListToDF(twit)
a=concatenate(twit$text)
a2=MC_tokenizer(twit$text)
a2=removeWords(a2, stopwords("english"))
c=stemDocument(a)
a1=ngram(c, n = 5, sep = "")
b1=get.phrasetable(a1)
d=termFreq(a2)
#sort(d,decreasing = TRUE)
par(mfrow = c(1, 1));plot(d)
g=weightTf(d)
#n-gram
head(b1,50)
#frequency words
head(sort(g,decreasing = TRUE),50)
#********************************************************

library(udpipe)
library(h2o)
library(tokenizers)
library(ngram)
library(tm)
library(tm.plugin.factiva)
library(tm.plugin.webmining)
library(NLP)
library(googleLanguageR)
library(twitteR)
library(Rfacebook)
#******************************************
x1 <- readLines ("c:/QRG/cbr_oct_17.txt")
y1 <- readLines ("c:/QRG/fed_sep_17.txt")
a=concatenate(x1)
b=concatenate(y1)
a1=ngram(a, n = 15, sep = "")
b1=ngram(b, n = 10, sep = "")
get.phrasetable(a1)
get.phrasetable(b1)
babble(a1, 15)
#******************************************
setup_twitter_oauth("zngU250W8JuxNSXnAMn4bJdSH", "4ox4xnnIZuQanRZ2xkXrNgXIxwHuTJPv09i80NnV0ADv4umPQW","2669848952-NVwkw7wRLbTYXX7lgQe5GGXH8ZHOjhAGB457jp5","OH1isZMxNKcrQ93CNTXod3gMFIk2WeXMrH4nSGRf5ZLuF")
fav1 = favorites("Amena__Bakr",n=10)
fav2=favorites("anasalhajji",n=10)
fav3=favorites("TankerTrackers",n=10)
fav4=favorites("JKempEnergy",n=10)
fav5=favorites("Lee_Saks",n=10)
fav6=favorites("JavierBlas2",n=10)
fav7=favorites("WSJenergy",n=10)
fav8=favorites("ErikSTownsend",n=10)
fav9=favorites("Statoil",n=10)
fav10=favorites("QuantOil",n=10)
fav11=favorites("IEABirol",n=10)
fav12=favorites("ArgusMediaOil",n=10)
fav13=favorites("Khalid_AlFalih",n=10)
fav14=favorites("FredericGonand",n=10)
fav15=favorites("Lisa_Ward1990",n=10)
fav16=favorites("AlexAndlauer",n=10)
fav17=favorites("WMALHittle",n=10)
fav18=favorites("Studebaker1963",n=10)
fav19=favorites("HamadeRiad",n=10)
fav20=favorites("StuartLWallace",n=10)
fav21=favorites("JLeeEnergy",n=10)
fav22=favorites("R_Zandi",n=10)
fav23=favorites("CollinEatonHC",n=10)
fav24=favorites("vsoldatkin",n=10)
fav25=favorites("ronbousso1",n=10)
fav26=favorites("ncitayim",n=10)
fav27=favorites("golnarM",n=10)
fav28=favorites("RowenaCaine",n=10)
fav29=favorites("EnergzdEconomy",n=10)
fav30=favorites("chris1reuters",n=10)
fav31=favorites("arascouet",n=10)
fav32=favorites("C_Barraud",n=10)
fav33=favorites("staunovo",n=10)
fav34=favorites("Ole_S_Hansen",n=10)
fav35=favorites("alexlongley1",n=10)
fav36=favorites("ValerieMarcel",n=10)
fav37=favorites("hgloystein",n=10)
fav38=favorites("summer_said",n=10)
fav39=favorites("ftcommodities",n=10)
fav40=favorites("Samir_Madani",n=10)
fav41=favorites("ja_herron",n=10)
fav42=favorites("wenkennedy",n=10)
fav43=favorites("WaelMahdi",n=10)
fav44=favorites("nayrazz",n=10)
fav45=favorites("T_Mason_H",n=10)
fav46=favorites("petromatrix",n=10)
fav47=favorites("AnjliRaval",n=10)
fav48=favorites("ErnestScheyder",n=10)
fav49=favorites("robinenergy",n=10)
fav50=favorites("Ed_Crooks",n=10)
fav51=favorites("Rory_Johnston",n=10)
fav52=favorites("OilandEnergy",n=10)
fav53=favorites("GBeleris",n=10)
fav54=favorites("IEA",n=10)
fav55=favorites("ReutersCommods",n=10)
fav56=favorites("humenm",n=10)
fav57=favorites("AlexLawler100",n=10)
fav58=favorites("OilSheppard",n=10)
fav59=favorites("mattpiotrowski",n=10)
fav60=favorites("EIAgov",n=10)
fav61=favorites("ArgusMedia",n=10)
fav62=favorites("GasBuddyGuy",n=10)
fav63=favorites("anasalhajji",n=10)
twit=rbind(twListToDF(fav1),twListToDF(fav2),twListToDF(fav3),twListToDF(fav4),twListToDF(fav5),twListToDF(fav6),twListToDF(fav7),twListToDF(fav8),twListToDF(fav9),twListToDF(fav10),twListToDF(fav11),twListToDF(fav12),twListToDF(fav13),twListToDF(fav14),twListToDF(fav15),twListToDF(fav16),twListToDF(fav17),twListToDF(fav18),twListToDF(fav19),twListToDF(fav20),twListToDF(fav21),twListToDF(fav22),twListToDF(fav23),twListToDF(fav24),twListToDF(fav25),twListToDF(fav26),twListToDF(fav27),twListToDF(fav28),
           twListToDF(fav29),twListToDF(fav30),twListToDF(fav31),twListToDF(fav32),twListToDF(fav33),twListToDF(fav34),twListToDF(fav35),twListToDF(fav36),twListToDF(fav37),twListToDF(fav38),twListToDF(fav39),twListToDF(fav40),twListToDF(fav41),twListToDF(fav42),twListToDF(fav43),twListToDF(fav44),twListToDF(fav45),twListToDF(fav46),twListToDF(fav47),twListToDF(fav48),twListToDF(fav49),twListToDF(fav50),twListToDF(fav51),twListToDF(fav52),twListToDF(fav53),twListToDF(fav54),twListToDF(fav55),twListToDF(fav56),
           twListToDF(fav57),twListToDF(fav58),twListToDF(fav59),twListToDF(fav60),twListToDF(fav61),twListToDF(fav62),twListToDF(fav63))
a=concatenate(twit$text)
#iconvlist();Encoding(a) <-"UTF-8"
a1=ngram(a, n = 17, sep = "")
b1=get.phrasetable(a1)
b1
