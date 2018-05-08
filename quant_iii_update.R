# load packages -----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(ggplot2)

# read data ---------------------------------------------------------------
df <- read.csv("main_file.csv") %>%
  mutate(meats = deli + fish + meat) %>%
  select(date, store, hvalmean, single, retired, nwhite, poverty, housing, timing, meats, produce, bakery, dairy) %>%
  gather(dairy, bakery, meats, produce, key = "product", value = "sales")
glimpse(df)

# format data properly ----------------------------------------------------
df$date <- ymd(df$date)
df$store <- factor(df$store)
df$product <- factor(df$product)
df$timing <- relevel(df$timing, ref = "pre")
df <- df %>% 
  mutate(sales_log = log(sales + 1))
glimpse(df)

# create tables ------------------------------------------------------------
table_log <- df %>% 
  group_by(date = year(date), product) %>%
  summarize(sales = mean(log(sales + 1), na.rm = T)) %>%
  arrange(date, desc(product)) %>%
  filter(date > 1989 & date < 1997) %>%
  spread(key = product, value = sales)
table_pct <- df %>% 
  group_by(date = year(date), product) %>%
  summarize(sales = mean(sales, na.rm = T)) %>%
  arrange(date, desc(product)) %>%
  filter(date > 1989 & date < 1997) %>%
  spread(key = product, value = sales)
table_pct[2:5] <- apply(table_pct[2:5], 2, function(x) {((x-lag(x))/lag(x))*100})
table_log
table_pct

# roughly plot data ---------------------------------------------------------------
df %>%
  group_by(date, product) %>%
  summarize(sales = mean(sales, na.rm = T)) %>%
  select(date, sales, product) %>%
  filter(sales > 0) %>%
  arrange(date, desc(product)) %>%
  ggplot(aes(x = date, y = sales, color = product)) +
  geom_line(stat = "identity", lwd = .5) +
  ggtitle("Product Sales at Dominick's Grocery Stores in Chicago, 1989-1997") +
  xlab("Date") +
  ylab("Mean Weekly Sales") +
  theme_minimal()

# plot data ------------------------------------------------------------
df %>% 
  group_by(date = year(date), product) %>%
  summarize(sales = mean(log(sales + 1), na.rm = T)) %>%
  arrange(date, desc(product)) %>%
  filter(date > 1989 & date < 1997) %>%
  spread(key = product, value = sales) %>%
  ggplot(aes(x = date, y = Log)) +
  xlab("Year") +
  ylab("Log of Sales") +
  ggtitle("Grocery Sales by Product Type at Dominick's Grocery Stores in Chicago, \n1990-1996\n ") +
  geom_line(aes(y = bakery), color = "orange", lwd = .5) +
  geom_line(aes(y = dairy), color = "blue", lwd = .5) +
  geom_line(aes(y = meats), color = "red", lwd = .5) +
  geom_line(aes(y = produce), color = "darkgreen", lwd = .5) +
  annotate("rect", xmin = 1992.3, xmax = 1996, ymin = 9, ymax = 11.1, alpha = .1) +
  annotate("text", x = 1992.35,  y = 11, label = "Food Pyramid Introduced", hjust = 0) +
  annotate("text", 
           x = rep(1990,4),  y = c(11,10.6,10.4,9.3), 
           label = c("Meat","Produce","Dairy","Bakery"),
           color = c("red", "darkgreen", "blue", "orange"),
           hjust = 0) +
  theme_classic() +
  theme(plot.margin=unit(c(7,7,9,8),"mm")) +
  theme(axis.title.x = element_text(family="Arial",size=15,
                                    face="bold",colour = "Black",vjust=-1,hjust=0.5)) +
  theme(axis.title.y = element_text(family="Arial",size=15,
                                    face="bold",colour = "Black",hjust=0.5)) +
  theme(title = element_text(family="Arial",size=18,
                                    face="bold",colour = "Black",hjust=0.5))







