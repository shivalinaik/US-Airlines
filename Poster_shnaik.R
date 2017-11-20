#
#Final Poster: Sentiment Analysis US Airlines
#Author: Shivali Naik

#load dataset
tmp <- file.choose()
airlines <- read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)
str(airlines)
head(airlines)

airlines <- airlines[airlines$negativereason != '',]
American <- airlines[airlines$airline == 'American',]
USAirways <- airlines[airlines$airline == 'US Airways',]
Delta <- airlines[airlines$airline == 'Delta',]
Southwest <- airlines[airlines$airline =='Southwest',]
Virgin <- airlines[airlines$airline == "Virgin America",]
United <- airlines[airlines$airline == 'United',]
unique(airlines$airline)
airlinesColor <- c("#40E0D0","#00F5FF","#6CA6CD","#4876FF","#00868B","#36648B")
par(new = TRUE)
plot(table(American$negativereason), type = "l", col = "#40E0D0")
par(new = TRUE)
plot(table(USAirways$negativereason), type = "l", col = "#00F5FF")
par(new = TRUE)
plot(table(Delta$negativereason), type = "l", col = "#6CA6CD")
par(new = TRUE)
plot(table(Southwest$negativereason), type = "l", col = "#4876FF")
par(new = TRUE)
plot(table(Virgin$negativereason), type = "l", col = "#00868B")
par(new = TRUE)
plot(table(United$negativereason), type = "l", col = "#36648B")
legend('topleft', 'bottomleft', pch = 4, legend = rownames(m5), lwd = 4, lty = 1, cex = 0.8, bty = "n", col = airlinesColor, ncol = 2)

library(fmsb)

m5 <- as.data.frame.matrix(table(airlines$negativereason,airlines$airline))
m5=rbind(rep(1300,3) , rep(0,3) , m5)
radarchart(m5)

radarchart( m5, 
            #custom polygon
            pcol=airlinesColor , plwd=4 , plty=1
)
legend(x=1, y=0, legend = rownames(m5), bty = "n", pch=20 , col=airlinesColor , text.col = "grey", cex=1.2, pt.cex=3)


install.packages("lattice")
library(lattice)
m5 <- as.data.frame.matrix(table(airlines$negativereason,airlines$airline))
parallel(m5, col = airlinesColor)
par(new = TRUE)
legend('topleft', 'bottomleft', pch = 4, legend = rownames(m5), lwd = 4, lty = 1, cex = 0.8, bty = "n", col = airlinesColor, ncol = 2)



library("ggplot2")
g1 = ggplot(as.data.frame(prop.table(table(American$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g1 = g1 + ggtitle('American: Reasons for bad sentiment')
g1 = g1 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g2 = ggplot(as.data.frame(prop.table(table(United$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g2 = g2 + ggtitle('United: Reasons for bad sentiment')
g2 = g2 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g3 = ggplot(as.data.frame(prop.table(table(USAirways$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g3 = g3 + ggtitle('US Airways: Reasons for bad sentiment')
g3 = g3 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g4 = ggplot(as.data.frame(prop.table(table(Delta$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g4 = g4 + ggtitle('Delta: Reasons for bad sentiment')
g4 = g4 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g5 = ggplot(as.data.frame(prop.table(table(Southwest$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g5 = g5 + ggtitle('Southwest: Reasons for bad sentiment')
g5 = g5 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))

g6 = ggplot(as.data.frame(prop.table(table(Virgin$negativereason))), aes(x = Var1, y = Freq)) +
  geom_bar(stat = 'identity', fill = 'lightblue')
g6 = g6 + ggtitle('Virgin: Reasons for bad sentiment')
g6 = g6 + theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 30, size = 10, vjust = 1))
library("gridExtra")
g6
grid.arrange(g1, g2, g3, g4, g5, g6, ncol = 2, nrow = 3)



#colors
sentimentColors <- c("#FF3030","#C0FF3E","#00BFFF")
genderColors <- c("#FFC1C1", "#FFEC8B", "#00C5CD")
airlinesColor <- c("#40E0D0","#00F5FF","#6CA6CD","#4876FF","#00868B","#36648B")
ageColors <- c("#66CD00","#CAFF70","#BCEE68","#A2CD5A","#00CD00","#90EE90","#C0FF3E","#9ACD32","#4EEE94")
airlines <- read.csv(file = tmp, header = TRUE, stringsAsFactors = FALSE)

#Count of tweets per Airline
table(airlines$airline)
barplot(table(airlines$airline), beside = T, border = NA, col = airlinesColor, xlab = "Airlines", ylab = "Tweet Count", main = "Number of Tweets per Airline", ylim = c(0,3800))
mtext(text = "United Airlines reported more tweets than the other airlines", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Gender of the Tweeters
table(airlines$gender)
barplot(table(airlines$gender), beside = T, border = NA, col = genderColors, xlab = "Gender", ylab = "Tweet Count", main = "Gender of the Tweeters", ylim = c(0,7500))
mtext(text = "Women tweeted more about the service provided by Airlines", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Range Age
table(airlines$age)
barplot(table(airlines$age), beside = T, border = NA, col = ageColors, xlab = "Range of Ages", ylab = "Tweet count", main = "Age of the Travellers who tweet")
mtext(text = "The majority of the travellers are in tha age range 27-32 years old with 3509 tweets in the dataset", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Number of tweets per sentiment
table(airlines$airline_sentiment)
barplot(table(airlines$airline_sentiment), beside = T, border = NA, col = sentimentColors, xlab = "Sentiment", ylab = "Tweet count", main = "Distribution of the Tweets per sentiment")
mtext(text = "The majority of the tweets reflected a negative feeling", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Which Airline receive more negative, positive and neutral tweets
airlines$airline_sentiment
airlineAndSentiment <- tapply(airlines$count, list(airlines$airline_sentiment, airlines$airline), sum) 
barplot(airlineAndSentiment, beside = T, col = sentimentColors, main = "Tweet sentiment count per Airline", bty = "l", xlab = "Airlines", ylab = "Tweet count")
legend("topright", "(x,y)" , pch = 14, legend = c("Negative", "Neutral", "Positive"),  ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = sentimentColors)
mtext(text = "United Airlines received more negative tweets than other Airlines", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Females or Males, who tweet more 
airlineAndGender <- tapply(airlines$count, list(airlines$airline_sentiment, airlines$gender), sum) 
watercolor.asMatrix <- as.matrix(airlineAndGender)
barplot(watercolor.asMatrix, beside = FALSE, col = sentimentColors, main = "Tweet sentiment by Gender", bty = "l", xlab = "Gender", ylab = "Tweet count")

legend("topright", "(x,y)" , pch = 14, legend = c("Negative", "Neutral", "Positive"),  ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = sentimentColors)
mtext(text = "Men and women expressed more negative tweets about airline service", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Main reasons for negative complains
airlinesNegativeOpinions <- na.omit(airlines$negativereason)
#creating vector of negative colors
topReasons <- head(sort(table(airlines$negativereason), decreasing = FALSE), n = 10)
negativeColors <- rev(heat.colors(10))
barplot(topReasons, beside = T, col = negativeColors, main = "Main reason for negative Tweeets", bty = "l", xlab = "Reason", ylab = "Tweet count")
mtext(text = "Customer Service Issue is the principal reason because travellers complain", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

#Age
airlineAndAge <- tapply(airlines$count, list(airlines$airline_sentiment, airlines$age), sum) 
barplot(airlineAndAge, beside = T, col = airlines.color, main = "Tweet sentiment by Age Range", bty = "l", xlab = "Age Range", ylab = "Tweet count")
legend("topright", "(x,y)" , pch = 14, legend = c("Negative", "Neutral", "Positive"),  ncol = 1, lwd = 4, lty = 1, cex = .8, bty = "n", col = airlines.color)
mtext(text = "The age of range 27-32 years old  expressed more negative tweets about airline service", side = 1, line = 4, adj = 0, col = "#6C7B8B", cex = 1)

par(mfrow=c(2,5))
table(airlines$user_timezone)
class(table(airlines$user_timezone))
#Get the top 10 time zones
topTimeZones <- head(sort(table(airlines$user_timezone), decreasing = TRUE), n = 11)
class(topTimeZones)
barplot(topTimeZones)
tweeters <- airlines[airlines$user_timezone == "Eastern Time (US & Canada)",]
barplot(table(tweeters$airline_sentiment), main = "Eastern Time (US & Canada)", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Central Time (US & Canada)",]
barplot(table(tweeters$airline_sentiment), main = "Central Time (US & Canada)", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Pacific Time (US & Canada)",]
barplot(table(tweeters$airline_sentiment), main = "Pacific Time (US & Canada)", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Quito",]
barplot(table(tweeters$airline_sentiment), main = "Quito", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Atlantic Time (Canada)",]
barplot(table(tweeters$airline_sentiment), main = "Atlantic Time (Canada)", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Mountain Time (US & Canada)",]
barplot(table(tweeters$airline_sentiment), main = "Mountain Time (US & Canada)", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Arizona",]
barplot(table(tweeters$airline_sentiment), main = "Arizona", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "London",]
barplot(table(tweeters$airline_sentiment), main = "London", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Alaska",]
barplot(table(tweeters$airline_sentiment), main = "Alaska", col = sentimentColors)
tweeters <- airlines[airlines$user_timezone == "Sydney",]
barplot(table(tweeters$airline_sentiment), main = "Sydney", col = sentimentColors)

#TREE Map
library("treemap")
airlinesNegativeOpinions <- na.omit(airlines$negativereason)
#creating vector of negative colors
topReasons <- as.data.frame(head(sort(table(airlines$negativereason), decreasing = FALSE), n = 10))
colnames(topReasons) = c("Reason","total")
rownames(data)=paste("reason" , letters[1:3] , sep="-")

treemap(topReasons, index = c("Reason"), vSize = "total")

#Creating map
mp <- NULL
mapWorld <- borders("state", colour="white", fill="#45b2d6") # create a layer of borders
mp <- ggplot() +   mapWorld

sentimentLocation <- airlines[airlines$tweet_coord != "",]

positiveLocation <- sentimentLocation[sentimentLocation$airline_sentiment == "positive",]
neutralLocation <- sentimentLocation[sentimentLocation$airline_sentiment == "neutral",]
negativeLocation <- sentimentLocation[sentimentLocation$airline_sentiment == "negative",]

pos_lat = NULL
pos_long = NULL
cords = strsplit(positiveLocation$tweet_coord, ',')
for (i in 1:length(cords)) {
  pos_lat = c(pos_lat, substring(cords[[i]][1], 2)) # removes first character which is [
  pos_long = c(pos_long, cords[[i]][2]) 
}
pos_long = substr(pos_long, 1, nchar(pos_long)-1)
pos_lat = as.numeric(pos_lat)
pos_long = as.numeric(pos_long)
#Now Layer the cities on top
mp_1 <- mp+ geom_point(aes(x=pos_long, y=pos_lat) ,color="#00E800", size=4) 
mp_1 = mp_1 + xlim(-125, -65) + ylim(25, 50)

neu_lat = NULL
neu_long = NULL
cords = strsplit(neutralLocation$tweet_coord, ',')
for (i in 1:length(cords)) {
  neu_lat = c(neu_lat, substring(cords[[i]][1], 2)) # removes first character which is [
  neu_long = c(neu_long, cords[[i]][2]) 
}
neu_long = substr(neu_long, 1, nchar(neu_long)-1)

neu_lat = as.numeric(neu_lat)
neu_long = as.numeric(neu_long)

#Now Layer the cities on top
mp_2 <- mp_1+ geom_point(aes(x=neu_long, y=neu_lat) ,color="#FF8300", size=3) 
mp_2 = mp_2 + xlim(-125, -65) + ylim(25, 50)

neg_lat = NULL
neg_long = NULL
cords = strsplit(negativeLocation$tweet_coord, ',')
for (i in 1:length(cords)) {
  neg_lat = c(neg_lat, substring(cords[[i]][1], 2)) # removes first character which is [
  neg_long = c(neg_long, cords[[i]][2]) 
}

neg_long = substr(neg_long, 1, nchar(neg_long)-1)

neg_lat = as.numeric(neg_lat)
neg_long = as.numeric(neg_long)

#Now Layer the cities on top
mp_3 <- mp_2+ geom_point(aes(x=neg_long, y=neg_lat) ,color="#FF0000", size=2) 
mp_3
