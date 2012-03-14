library(ggplot2)
#library(zipcode) <- brauche ich wohl nicht
library(stringr) # lat und lon trennen
library(raster) # für Deutschlandkarte
library(twitteR) # für die Posts

setwd("C:\\Dokumente und Einstellungen\\ahschulz\\Eigene Dateien\\My Dropbox\\R\\twitternde_gemeinden")
setwd("C:\\Users\\Arne\\Dropbox\\R\\twitternde_gemeinden")

data <- read.delim("data/verwaltungen.txt", header = F)

lat_lon <- as.numeric(do.call("rbind", str_split(data$V3, ", ")))

data2 <- cbind(data[, 1:2], lat_lon[1:68], lat_lon[69:136])

names(data2) <- c("tw_name", "url", "lat", "lon")

# Anzahl der Tweets und Follower nachschlagen
for (i in data2$tw_name) {
  temp <- statusesCount(getUser(i))
  temp2 <- followersCount(getUser(i))
  if (i == "@ahlen_de") {
    count_tweets <- temp
    follower <- temp2
  } else {
    count_tweets <- append(count_tweets, temp)
    follower <- append(follower, temp2)
  }
}

data2 <- cbind(data2, count_tweets, follower)

# Daten speichern
save(data2, file = "data/gemeinden.rData")
load("data/gemeinden.rData")

# Map laden
de_map <- getData('GADM', country="DE", level=1)
de_map_2 <- fortify(de_map, region = "NAME_1")
gpclibPermit()
de_map_2 <- fortify(de_map, region = "NAME_1")



# Ländergrenzen nach hinten
tw2 <- ggplot(data= data2) + geom_path(data = de_map_2, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 2)
tw2 <- tw2 + geom_point(aes(x = lon, y = lat, colour = follower, size = count_tweets)) + scale_area("Anzahl Tweets")
tw2 <- tw2 + theme_bw() + labs(x = NULL, y = NULL) +  scale_colour_gradient(low = "#9D9D9D", high = "#242424", name = "Follower", limits = c(0, 12000))#breaks = c(2000,4000,6000,8000,10000,12000))
# + scale_colour_brewer(palette = "RdYlGn", name = "PLZ")
tw2 <- tw2 + scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA)
tw2 <- tw2 + opts(title = "twitternde Gemeinden")
tw2


# Plot fürs Blog
tw <- ggplot(data= data2) + geom_path(data = de_map_2, aes(x = long, y = lat, group = group), colour = "#8D8D8D", linetype = 2)
tw <- tw + geom_point(aes(x = lon, y = lat, colour = follower, size = count_tweets)) + scale_area("Tweet count")
tw <- tw + theme_bw() + labs(x = NULL, y = NULL) +  scale_colour_gradient(low = "#9D9D9D", high = "#242424", name = "Follower count", limits = c(0, 12000))
tw <- tw + scale_x_continuous(breaks = NA) + scale_y_continuous(breaks = NA)
tw <- tw + opts(title = "Municipalities using Twitter")
tw