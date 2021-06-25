library(tidyverse)
library(readr)
library(lubridate)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/accidents.csv")
accidents <- read.csv(text = x)
x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/casualties.csv")
casualties <- read.csv(text = x)
x <- getURL("https://raw.githubusercontent.com/andybirch/UK-road-accidents/master/vehicles.csv")
vehicles <- read.csv(text = x)

view(accidents)
view(casualties)
view(vehicles)
