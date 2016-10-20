################################################################################
# Code for visualisation and modelling GWW fire history
#
# by Bart Huntley

wkdir <- "Z:\\DEC\\GreatWesternWoodland\\Working\\Fire_mapping_2016\\R_analysis"
library(dplyr)
library(tidyr)

setwd(wkdir)

files <- list.files(pattern = "*.csv")

#YSLB data
data7 <- read.csv(files[7], header = TRUE, stringsAsFactors = FALSE)
names(data7)[1] <- "ynames"

#b1
data1 <- read.csv(files[1], header = TRUE, stringsAsFactors = FALSE)
data1 <- data1[,-1]


df1 <- data1 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b1 = val) %>%
  select(year, sites, b1)

df1.1 <- left_join(df1, data7, by = "sites")

data2 <- read.csv(files[2], header = TRUE, stringsAsFactors = FALSE)
data2 <- data2[,-1]

df2 <- data2 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b2 = val) %>%
  select(year, sites, b2)

df2.1 <- left_join(df2, data7, by = "sites")

data3 <- read.csv(files[3], header = TRUE, stringsAsFactors = FALSE)
data3 <- data3[,-1]

df3 <- data3 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b3 = val) %>%
  select(year, sites, b3)

df3.1 <- left_join(df3, data7, by = "sites")

data4 <- read.csv(files[4], header = TRUE, stringsAsFactors = FALSE)
data4 <- data4[,-1]

df4 <- data4 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b4 = val) %>%
  select(year, sites, b4)

df4.1 <- left_join(df4, data7, by = "sites")

data5 <- read.csv(files[5], header = TRUE, stringsAsFactors = FALSE)
data5 <- data5[,-1]

df5 <- data5 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b5 = val) %>%
  select(year, sites, b5)

df5.1 <- left_join(df5, data7, by = "sites")


data6 <- read.csv(files[6], header = TRUE, stringsAsFactors = FALSE)
data6 <- data6[,-1]

df6 <- data6 %>%
  gather("sites", "val", 2:3210) %>%
  mutate(year = substr(ynames, 3, 6)) %>%
  rename(b6 = val) %>%
  select(year, sites, b6)

df6.1 <- left_join(df6, data7, by = "sites")

all <- bind_cols(df1.1, df2.1, df3.1, df4.1, df5.1, df6.1)
comdf <- all[,c(1, 2, 4, 3, 7, 11, 15, 19, 23)]


  