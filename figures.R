library(ggplot2)
library(ggpubr)
library(dplyr)

##months##
date <- watched %>%
  group_by(DATE) %>%
  summarise(count = n())

date <- as.data.frame(date)

colnames(date)[1] <- "month"
colnames(date)[2] <- "count"

x <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")

ggplot(date, aes(x = (factor(month, levels=x)), y = count)) +
  xlab("month") +
  geom_bar(fill = "black", stat = "identity") +
  theme_pubclean(base_size = 15)

sum(watched$LENGTH)

#rating##
rate <- watched %>%
  group_by(RATING) %>%
  summarise(count = n())

rate <- as.data.frame(rate)
colnames(rate)[1] <- "rating"
colnames(rate)[2] <- "count"

rate2 <- rate[-c(9), ]
mean2 <- mean(rate2$)

ggplot(rate2, aes(x = rating, y = count)) +
  geom_bar(fill = "black", stat = "identity") +
  theme_pubclean(base_size = 15)

sd(rate$count)

##new##
new <- watched %>%
  group_by(REWATCH) %>%
  summarise(count = n())

new <- as.data.frame(new)
colnames(new)[1] <- "rewatch"
colnames(new)[2] <- "count"

ggplot(new, aes(x = "", y=count, fill=rewatch)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void(base_size = 15)

##genre##
genre <- watched[, c("NAME", "GENRE")]
genre <- as.data.frame(genre)
list <- strsplit(genre$GENRE, ",")
all <- unlist(list)
counts <- table(all)
counts <- as.data.frame(counts)

colnames(counts)[1] <- "genre"
colnames(counts)[2] <- "count"

ggplot(counts, aes(x = "", y=count, fill=genre)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void(base_size = 15)

##directors##
write.table(director, "director.txt", sep="\t", row.names = F, quote=F)

##language
fore <- watched %>%
  group_by(FOREIGN) %>%
  summarise(count = n())

colnames(fore)[1] <- "foreign"
colnames(fore)[2] <- "count"

ggplot(fore, aes(x = "", y=count, fill=foreign)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void(base_size = 15)

lang <- watched %>%
  group_by(PRIMARY_LANGUAGE) %>%
  summarise(count = n())

colnames(lang)[1] <- "language"
colnames(lang)[2] <- "count"

ggplot(lang, aes(x = "", y=count, fill=language)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  theme_void(base_size = 15)
