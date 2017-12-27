# R/3.4.3 kite-eating tree
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rlang)
setwd("~/Desktop/1706 and 1713 Col4a5xRfx3/Data/")
options(tibble.width = Inf)
wweights <- read_csv("1713_weekly_weights.csv")

# tidy dataframe
wweights$DOB <- myd(wweights$DOB)
wweights$Sex <- parse_factor(wweights$Sex, levels = unique(wweights$Sex))
wweights$Col4a5_geno <- parse_factor(wweights$Col4a5_geno, levels = unique(wweights$Col4a5_geno))
wweights$Rfx3_geno <- parse_factor(wweights$Rfx3_geno, levels = unique(wweights$Rfx3_geno))
wweights$Cat <- parse_factor(wweights$Cat, levels = unique(wweights$Cat))

# gather by weeks and prepare for plotting
wks <- colnames(wweights)[10:ncol(wweights)]
ggdata <- wweights %>%
              gather(wks, key = "Week", value = "Weight")

#plot
ggplot(ggdata, aes(x = Week, y = Weight, group = Animal_ID, colour = Cat, shape = Cat)) +
  geom_point() +
  geom_line() +
  labs( title = "1713 Col4a5xRfx3 Weekly Weights",
        subtitle = "15% drop indicator for euthanization",
        y = "Weights (g)",
        x = "Weeks",
        colour = "Cohorts",
        shape = "Cohorts")

# Check if any animals droped their weight by 15%
wweights <- wweights %>%
                  # Calculate by row
                  rowwise() %>%
                  # calculate the max per row of each
                  mutate( drop15= max(!!! syms(wks)) * 0.85 > !!! syms(wks[[length(wks)]]))

any(wweights$drop15, na.rm = TRUE)
