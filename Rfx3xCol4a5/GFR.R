# R/3.4.3 kite-eating tree
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
indir <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data"
outfile <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data/1713_GFRplot.R"

# read file

wweights <- read_excel(infile, sheet = "1713_weekly_weights")
