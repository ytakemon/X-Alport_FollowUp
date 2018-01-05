# R/3.4.3 kite-eating tree
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
wweights <- read_csv("~/Desktop/1717 and 1718 Col4a5xFmn1/Data/1718_weekly_weights.csv")

# tidy dataframe
wweights$DOB <- myd(wweights$DOB)
wweights$Sex <- parse_factor(wweights$Sex, levels = unique(wweights$Sex))
wweights$Col4a5_geno <- parse_factor(wweights$Col4a5_geno, levels = unique(wweights$Col4a5_geno))
wweights$Fmn1_geno <- parse_factor(wweights$Fmn1_geno, levels = unique(wweights$Fmn1_geno))
wweights$Cat <- parse_factor(wweights$Cat, levels = unique(wweights$Cat))

# gather by weeks and prepare for plotting
wks <- colnames(wweights)[10:ncol(wweights)]
ggdata <- wweights %>%
              gather(wks, key = "Week", value = "Weight")

#plot
plot <- ggplot(ggdata, aes(x = Week, y = Weight, group = Animal_ID, colour = Cat, shape = Cat)) +
              geom_point() +
              geom_line() +
              labs( title = "1718 Col4a5xFmn1 Weekly Weights",
                    subtitle = "15% drop indicator for euthanization",
                    y = "Weights (g)",
                    x = "Weeks",
                    colour = "Cohorts",
                    shape = "Cohorts") +
              scale_color_aaas()

pdf("~/Desktop/1706 and 1718 Col4a5xFmn1/Data/WeeklyWeights.pdf", width = 8, height = 6)
print(plot)
dev.off()

# Check if any animals droped their weight by 15%
wweights <- wweights %>%
                  # Calculate by row
                  rowwise() %>%
                  # calculate the max per row of each
                  mutate( drop15= max(!!! syms(wks)) * 0.85 > !!! syms(wks[[length(wks)]]))

if(any(wweights$drop15, na.rm = TRUE)){
  print("Following animals have dropped their weight by 15%!")
  wweights[wweights$drop15 == TRUE,]
} else if (!any(wweights$drop15, na.rm = TRUE)){
  print("Keep going, all animals have maintained a healthy weight")
} else {
  print("Something is wrong, check Rscript!")
}
