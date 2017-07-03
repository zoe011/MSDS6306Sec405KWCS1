beers <- read.csv("Beers.csv")
breweries <- read.csv("Breweries.csv")
head(breweries)
str(breweries)
summary(breweries)

# Q1 How many breweries are present in each state?
str(breweries)
library(plyr)
brewery.count.by.state <- count(breweries$State)
colnames(brewery.count.by.state) <- c("States", "Count")
p1 <- ggplot(data = brewery.count.by.state, aes(x=States, y=Count)) + geom_bar(stat="identity")


# Breweries exists in all 51 states.
# blah blah blah....



#Q2
# change breweries$brew_id name to be Brewery_id so we can merge on common id
colnames(breweries)[1] <- "Brewery_id"
colnames(breweries)[2] <- "Brewery_name"
colnames(beers)[1] <- "Beer_name"
# verified column name change
head(breweries) 
head(beers)
beersnbreweries <- merge.data.frame(beers, breweries, by="Brewery_id")
beersnbreweries[beersnbreweries$Brewery_id == 557,]
head(beersnbreweries)
tail(beersnbreweries)

# Q3
# test function
sum(is.na(beersnbreweries$IBU))
col.has.na <- sapply(beersnbreweries, function(y) sum(is.na(y)) ) # Obtain column that has na
# only ABV and IBU has 'na'
col.has.na[ c(4,5)]



# q4
# clean up State that has space in front ie " NY" instead of "NY"
# ...
beersnbreweries[]
beersnbreweries[beersnbreweries$State==" NY", c("ABV")] # NY has ABV values of 'na'
beersnbreweries[beersnbreweries$State==" SD", c("IBU")] # NY has IBU values of 'na'

# use deplyr pipe, remove 'na' when computing median 
median.ABV.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Median=median(ABV, na.rm=TRUE))
median.ABV.by.state[median.ABV.by.state$State==" NY",] # double checking NY has median and not 'na'
median.ABV.by.state
p.median.ABV.by.state <- ggplot(data = median.ABV.by.state, aes(x=State, y=Median)) + geom_bar(stat="identity") + ggtitle("Median ABV")
p.median.ABV.by.state

median.IBU.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Median=median(IBU, na.rm=TRUE))
median.IBU.by.state[median.IBU.by.state$State==" NY",] # double checking NY has median and not 'na'
median.IBU.by.state
p.median.IBU.by.state <- ggplot(data = median.IBU.by.state, aes(x=State, y=Median)) + geom_bar(stat="identity") + ggtitle("Median IBU")
p.median.IBU.by.state
# state SD has no valid entries for IBU


# Q5
max.ABV.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Max=max(ABV, na.rm=TRUE))
max.ABV.by.state
arrange(max.ABV.by.state, desc(Max)) # max ABV is in state CO, with value of 0.128

max.IBU.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Max=max(IBU, na.rm=TRUE))
max.IBU.by.state
arrange(max.IBU.by.state, desc(Max)) # max IBU is in state OR, with value of 138



# q6



# q6
beersnbreweries.abv <- beersnbreweries[,c(4)]
beersnbreweries.abv
summary(beersnbreweries.abv)



# q7
abvs <- beersnbreweries.abv
ibus <- beersnbreweries[,c(5)]
p.abv.vs.ibu <- plot(abvs, ibus, pch=".", cex=3, col="green")
p.abv.vs.ibu
abline(lm(ibus~abvs), col="darkgreen", lwd=2)
str(abvs)



