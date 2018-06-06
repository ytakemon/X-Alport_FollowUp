# R/3.4.3 kite-eating tree
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
indir <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data"
outfile <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data/1713_GFRplot.R"

# Get all files and gather them into one
files <- list.files(path = indir, pattern = "xlsx", full.names = TRUE)
data_all <- NULL
for(i in 1:length(files)){
  data <- read_excel(files[i])
  data$"Time into isoflurane" <- hms(str_sub(as.character(data$"Time into isoflurane"),12))
  data <- data[!is.na(data[,1]),] # remove empty rows
  data_all <- rbind(data_all, data) %>% arrange(Name)
}

# Need to convert dates and time accordingly






options(tibble.width = Inf)
infile <- "~/Desktop/Col4a5_FollowupStudies/1706 and 1713 Col4a5xRfx3/Weights/1713 Col4a5xRfx3 experimental cohort and schedule.xlsx"
outfile <- "~/Desktop/Col4a5_FollowupStudies/1706 and 1713 Col4a5xRfx3/Weights/WeeklyWeights.pdf"

# read file
wweights <- read_excel(infile, sheet = "1713_weekly_weights")

# tidy dataframe
wweights$DOB <- ymd(wweights$DOB)
wweights$Sex <- parse_factor(wweights$Sex, levels = unique(wweights$Sex))
wweights$Col4a5_geno <- parse_factor(wweights$Col4a5_geno, levels = unique(wweights$Col4a5_geno))
wweights$Rfx3_geno <- parse_factor(wweights$Rfx3_geno, levels = unique(wweights$Rfx3_geno))
wweights$Cat <- parse_factor(wweights$Cat, levels = c("A_Long", "B_Long", "C_Long", "D_Long","NA_Long")) # adding NA as temp place holder while genotyping is being sorted out
