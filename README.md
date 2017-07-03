---
title: "Case Study 01 About Beers"
author: "Eng Kim Wong"
date: "7/2/2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Introduction

This is an analysis about US craft beers and Breweries. 
There are 2 datasets : Beers.csv and Breweries.csv. They are merged to facilitate further analysis. We look at the total number of breweries in each state and compute the median alcohol content and internatial bitterness for each state. 
A bar chart is plotted to graphically present the result.
I further present the state has the maximum alcoholic beer and state that has the most bitter beer.
Finally the analysis end with the summary statistic for ABV (Alcohol by Volume) and the relationship between ABV and IBU (International bitterness unit) on scatter plot.

##### Preparing beers.csv and breweries.csv data files for analysis:
```{r}
beers <- read.csv("Beers.csv") 
breweries <- read.csv("Breweries.csv")
```
```{r eval=FALSE, echo=FALSE}
head(breweries)
# Peek at the structure of breweries
str(breweries)
# Peek at the summary of breweries
summary(breweries)
```

#### How many breweries are present in each state?
```{r fig.width=12}
library(plyr) # import plyr and ggplot2 library
library(ggplot2)
brewery.count.by.state <- count(breweries$State)  # obtain count of breweries in each state
colnames(brewery.count.by.state) <- c("States", "Count") # changing column names 
```
##### *Breweries in each state*
```{r}
print(brewery.count.by.state) 
p1 <- ggplot(data = brewery.count.by.state, aes(x=States, y=Count)) + geom_bar(stat="identity") # bar plot
p1
``` 

#### Merge the two data sets:
```{r}
# change breweries$brew_id name to be Brewery_id so we can merge on common id
colnames(breweries)[1] <- "Brewery_id"
colnames(breweries)[2] <- "Brewery_name"
colnames(beers)[1] <- "Beer_name"
```
```{r echo=FALSE, eval=FALSE}
# verified column name change
head(breweries) 
head(beers)
```
```{r, message=FALSE, results="hide"}
beersnbreweries <- merge.data.frame(beers, breweries, by="Brewery_id")
```
```{r echo=FALSE, eval=FALSE}
beersnbreweries[beersnbreweries$Brewery_id == 557,] # verification
```
###### First 6 observations
```{r echo=FALSE}
head(beersnbreweries) # First 6 observations 
```

###### Last 6 observations
```{r echo=FALSE}
tail(beersnbreweries) # Last 6 observations
```

#### Number of invalid entries in each column:
```{r echo=FALSE, eval=FALSE}
# test function before apply in sapply below
sum(is.na(beersnbreweries$IBU))
```
```{r}
col.has.na <- sapply(beersnbreweries, function(y) sum(is.na(y)) ) # Obtain column that has na
```
##### *ABV column has 62 'na' and IBU has 1005*
```{r}
col.has.na[ c(4,5)] # only ABV and IBU has 'na'
```

#### Median of ABV (Alcohol by Volume) and IBU (International Bitterness Unit) by states:
###### ABV:
```{r echo=FALSE, eval=FALSE, message=FALSE}
beersnbreweries[]
beersnbreweries[beersnbreweries$State==" NY", c("ABV")] # NY has ABV values of 'na'
beersnbreweries[beersnbreweries$State==" SD", c("IBU")] # NY has IBU values of 'na'
```
```{r results="hide", echo=FALSE, message=FALSE}
library(dplyr)
```
```{r}
# use deplyr pipe, remove 'na' when computing median 
median.ABV.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Median=median(ABV, na.rm=TRUE))
```
```{r eval=FALSE, echo=FALSE, message=FALSE}
median.ABV.by.state[median.ABV.by.state$State==" NY",] # double checking NY has median and not 'na'
median.ABV.by.state
```
```{r fig.width=12}
# plotting median.ABV.by.state
p.median.ABV.by.state <- ggplot(data = median.ABV.by.state, aes(x=State, y=Median)) + geom_bar(stat="identity") + ggtitle("Median ABV")
p.median.ABV.by.state
```

###### IBU:
```{r}
median.IBU.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Median=median(IBU, na.rm=TRUE))
```
```{r eval=FALSE, echo=FALSE, message=FALSE}
median.IBU.by.state[median.IBU.by.state$State==" NY",] # double checking NY has median and not 'na'
median.IBU.by.state
```
```{r fig.width=12}
# plotting median.IBU.by.state
p.median.IBU.by.state <- ggplot(data = median.IBU.by.state, aes(x=State, y=Median)) + geom_bar(stat="identity") + ggtitle("Median IBU")
p.median.IBU.by.state
```

###### _State SD has no valid entries for IBU!_

#### Which state has the maximum alcoholic beer? Which state has the most bitter beer?
###### Maximum alcoholic beer: **max ABV is in state CO, with value of 0.128**
```{r}
max.ABV.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Max=max(ABV, na.rm=TRUE))
arrange(max.ABV.by.state, desc(Max)) 
```

###### Most bitter beer: **max IBU is in state OR, with value of 138**
```{r}
max.IBU.by.state <- beersnbreweries %>% group_by(State) %>% summarise(Max=max(IBU, na.rm=TRUE))
arrange(max.IBU.by.state, desc(Max)) 
```

#### Summary statistics for ABV (Alcohol by volume) variable
```{r}
beersnbreweries.abv <- beersnbreweries[,c(4)]
summary(beersnbreweries.abv)
```

#### Is there a relationship between the bitterness of the beer and its alcoholic content?
```{r fig.width=12}
abvs <- beersnbreweries.abv # a vector containing all abv
ibus <- beersnbreweries[,c(5)] # a vector containing all ibu
p.abv.vs.ibu <- plot(abvs, ibus, pch=".", cex=3, col="green", main = "ABV vs IBU") # pch is point shape, in this case a period character
abline(lm(ibus~abvs), col="darkgreen", lwd=2) # least squares regression, lwd is thickness of line
```

#####The scatter plot exhibits a positive correlation.
#####The more alcoholic content a beers has the higher the IBU value, that is the bitter the beer is.
>
>



