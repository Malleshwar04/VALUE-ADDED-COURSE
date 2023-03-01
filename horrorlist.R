library(rvest)
library(dplyr)
link <- 'https://www.imdb.com/list/ls005114080/'
#path = path_allowed(link)
web <- read_html(link)
name <- web %>% html_nodes(".lister-item-header a")%>% html_text()
View(name)
runtime <- web %>% html_nodes(".runtime")%>% html_text()
View(runtime)
rating <- web %>%html_nodes(".ipl-rating-star.small .ipl-rating-star__rating")%>% html_text()
View(rating)
horrormovies <- data.frame(name,runtime,rating)
View(horrormovies)
write.csv (horrormovies,"my horror movies.csv")


#Data pre processing

library(rvest)
library(dplyr)

imdb.ratings <- read.csv("my horror movies.csv")

View(imdb.ratings)

imdb.ratings$runtime <- gsub("min" ,"", imdb.ratings$runtime)
View(imdb.ratings)


str(imdb.ratings)
imdb.ratings$name <- as.character(imdb.ratings$name)
imdb.ratings$runtime <- as.integer(imdb.ratings$runtime)
imdb.ratings$rating <- as.numeric(imdb.ratings$rating)

View(imdb.ratings)
str(imdb.ratings)

set.seed(124)
imdb.ratings$Gross <- runif(20 ,min = 10000000 , max = 1000000000)
imdb.ratings$Gross_dollars <- runif(20 ,min = 10000000 , max = 1000000000)
View(imdb.ratings)

imdb.ratings <- subset(imdb.ratings, select = -(Gross))
View(imdb.ratings)       
imdb.ratings$rank <- c(1:20)
View(imdb.ratings)
imdb.ratings <- imdb.ratings[c("rank","name","runtime","rating","Gross_dollars")]
View(imdb.ratings)

imdb.ratings[1, 6] <- NA
imdb.ratings[8,6] <- NA
imdb.ratings[16, 6] <- NA
imdb.ratings[18, 6] <- NA
View(imdb.ratings)

imdb.ratings$gross_dollars = ifelse(is.na(imdb.ratings$gross_dollars),
                                    ave(imdb.ratings$gross_dollars,
                                        fun = function(x)
                                          mean(x,na.rm = true)),
                                    imdb.ratings$gross_dollars)
View(imdb.ratings)

imdb.ratings$watch_list <- imdb.ratings$rating > 8.6
View(imdb.ratings)
str(imdb.ratings)
imdb.ratings$watch_list = as.factor(imdb.ratings$watch_list)
str(imdb.ratings)
imdb.ratings$watch_list = as.factor(imdb.ratings$watch_list)
#Let deal with categorical data
imdb.ratings$watch_list = factor(imdb.ratings$watch_list,
                                 levels = c('true','false'),
                                 labels = c(1,0))
View(imdb.ratings)
#Training set and testing set
library(caTools)
split = sample.split(imdb.ratings$Gross_dollars,
                     SplitRatio = 0.8)
training_set = subset(imdb.ratings,split == TRUE)
testing_set = subset(imdb.ratings,split == FALSE)
View(training_set)
View(testing_set)

install.packages("outliers")
library(outliers)
test <- grubbs.test(imdb.ratings$Gross_dollars)
test

training_set$scaled_gross = scale(training_set[,6])
View(training_set)
testing_set$scaled_gross = scale(testing_set[,6])
View(testing_set)

