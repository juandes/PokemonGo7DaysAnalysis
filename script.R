setwd("~/Development/R/PokemonGo")

require(ggplot2)
require(dplyr)
require(reshape2)

# Load data
pokemon <- read.csv("~/Development/R/PokemonGo/data/Pokemon.csv",
                    stringsAsFactors=TRUE)
items <- read.csv("~/Development/R/PokemonGo/data/Items.csv",
                  stringsAsFactors=FALSE)
pidgey <- read.csv("~/Development/R/PokemonGo/data/Pidgey.csv",
                   stringsAsFactors=FALSE)
drowzee <- read.csv("~/Development/R/PokemonGo/data/Drowzee.csv",
                    stringsAsFactors=FALSE)

# Converts NA's to 0
items[is.na(items)]<-0

## Pokemon
summary(pokemon)
ggplot(pokemon, aes(x=Date)) +
  geom_histogram(aes(fill = ..count..)) +
  ggtitle("Amount of Pokemon per day")

frequency <- as.data.frame(table(pokemon$Pokemon))
prop.table(frequency$Frequency) * 100
colnames(frequency) <- c('Pokemon', 'Frequency')

ggplot(pokemon, aes(x=reorder(Pokemon, Pokemon,
                                   function(x) - length(x)))) + 
  geom_bar() + theme(axis.text.x = element_text(angle = 90, vjust = 1)) +
  ggtitle("Pokemon counter") +
  xlab("Pokemon") +
  ylab("Count")

qplot(y=pokemon$CP, x= 1, geom = "boxplot", main = "Boxplot of CP",
      ylab = "Combat Power (CP)", xlab = NULL)

## CP over time
ggplot(pokemon, aes(Date, CP)) +
  geom_point(aes(alpha = 1/6)) +
  annotate("text", x = 3, y = 800, label = c("Snorlax")) +
  annotate("text", x = 9, y = 700, label = c("Kingler")) +
  ggtitle("CP over time")




# Mean and CP per day
cp <- data.frame(cp = pokemon$CP, index = 1:nrow(pokemon))
by_date <- group_by(pokemon, Date)
by_date <-summarise(by_date, mean(CP), sd(CP))
colnames(by_date) <- c("date", "mean", "sd")

ggplot(by_date, aes(date,mean)) +
  geom_bar(stat = "identity") +
  ggtitle("Mean of CP over day")

ggplot(by_date, aes(date,sd)) + 
  geom_bar(stat = "identity") +
  ggtitle("Standard deviation (sd) of CP over day")

p <- ggplot(cp, aes(index, cp))
p + geom_point() + geom_smooth(method=lm) 

# Items
summary(items)
sum(items[2:ncol(items)])
sum(items[2:ncol(items)]) / nrow(items)
items.frequency <- as.data.frame(colSums(items[2:ncol((items))]))
prop.table(items.frequency) * 100
items.frequency <- cbind(row.names(items.frequency), items.frequency)
colnames(items.frequency) <- c("Item", "Frequency")
items.frequency <- data.frame(Item = items.frequency$Item, Frequency = as.integer(items.frequency$Frequency))
ggplot(items.frequency,
       aes(x = reorder(Item, -Frequency),y = Frequency, fill = Item)) + geom_bar(stat = "identity") +
  ggtitle("Total of items obtained at Pokestops") +
  xlab("Item") +
  ylab("Total")

p <- ggplot(items, aes(Date, Pokeball))
p + geom_point()

## plot the sum of items
## Pokeballs
pokeballs <- data.frame(pokeballs = items$Pokeball, index = 1:nrow(items))
p <- ggplot(pokeballs, aes(index, pokeballs))
p + geom_point() + geom_smooth(method=lm) 

pokeballs <-data.frame(date = items$Date, pokeball = items$Pokeball)
pokeballs.by_date <- group_by(pokeballs, date)
pokeballs.by_date <- summarise(pokeballs.by_date, frequency = sum(pokeball))

#ggplot(pokeballs.by_date, aes(date, frequency)) + geom_point() + geom_smooth(method = lm)

g.pokeballs <- data.frame(pokeballs = items$Great.Ball, index = 1:nrow(items))
p <- ggplot(g.pokeballs, aes(index, pokeballs))
p + geom_point() + geom_smooth(method=lm) 

## Potion
potions <- data.frame(potions = items$Potion, index = 1:nrow(items))
p <- ggplot(potions, aes(index, potions))
p + geom_point() + geom_smooth(method=lm) 

## Super potion
s.potions <- data.frame(potions = items$Super.Potion, index = 1:nrow(items))
p <- ggplot(s.potions, aes(index, potions))
p + geom_point() + geom_smooth(method=lm) 

## Hyper potion

potions <-data.frame(date = items$Date, potion = items$Potion)
potions.by_date <- group_by(potions, date)
potions.by_date <- summarise(potions.by_date, Frequency = sum(potion))

s.potions <-data.frame(date = items$Date, potion = items$Super.Potion)
s.potions.by_date <- group_by(s.potions, date)
s.potions.by_date <- summarise(s.potions.by_date, Frequency = sum(potion))

h.potions <-data.frame(date = items$Date, potion = items$Hyper.Potion)
h.potions.by_date <- group_by(h.potions, date)
h.potions.by_date <- summarise(h.potions.by_date, Frequency = sum(potion))

all.potions <- data.frame(date = potions.by_date$date,
                          potion = potions.by_date$Frequency,
                          s.potion = s.potions.by_date$Frequency,
                          h.potion = h.potions.by_date$Frequency)

all.potions <- melt(all.potions)
ggplot(all.potions, aes(x = date, y = value, fill = variable)) +
  geom_bar(stat='identity') +
  xlab("Date") + ylab("Value") + ggtitle("Amount of potions by day")

## Amount of items per day
items.per.date <- data.frame(date = items$Date, frequency = rowSums(items[2:8]))
items.per.date <- group_by(items.per.date, date)
items.per.date <- summarize(items.per.date, frequency = sum(frequency))
ggplot(items.per.date, aes(x = date, y = frequency))  + geom_bar(stat = "identity")





## Drowzees
summary(drowzee)
ggplot(drowzee, aes(x = CP, y = HP)) + geom_point() + geom_smooth(method=lm) +
  ggtitle("Drowzee's HP and CP with regression line")
cor(drowzee$CP, drowzee$HP)
drowzee.lr <- lm(HP ~ CP, data = drowzee)
summary(drowzee.lr)
plot(drowzee$CP, drowzee$HP)
abline(lm(drowzee$CP~drowzee$HP), col="red")
hist(drowzee.lr$residuals, breaks = 100)


## Pidgey
summary(pidgey)
ggplot(pidgey, aes(x = HP, y = CP)) + geom_point() + geom_smooth(method=lm) +
  ggtitle("Pidgey's CP and HP with regression line")
cor(pidgey$CP, pidgey$HP)
pidgey.lr <- lm(HP ~ CP, data = pidgey)
summary(pidgey.lr)
plot(pidgey$HP, pidgey$CP)
abline(lm(pidgey$CP~pidgey$HP), col="red")
hist(pidgey$residuals, breaks = 100)

cor(drowzee$CP, drowzee$Weight)
cor(drowzee$CP, drowzee$Height)
cor(drowzee$HP, drowzee$Weight)
cor(drowzee$HP, drowzee$Height)
cor(drowzee$Height, drowzee$Weight)
cor(pidgey$Height, pidgey$Weight)
plot(drowzee$Height, drowzee$Weight)
