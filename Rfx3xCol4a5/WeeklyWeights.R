# R/3.4.3 kite-eating tree
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
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

# Identify outliers and drop from plot and calculations
# Not outliers yet


# gather by weeks and prepare for plotting
wks <- colnames(wweights)[grep("Wk", colnames(wweights))]
ggdata <- wweights %>%
          gather(wks, key = "Week", value = "Weight") %>%
          mutate(Weight=suppressWarnings(as.numeric(replace(Weight,Weight=="NA", NA)))) %>%
          filter(!is.na(Weight))

# This will generate a warning, which should be fixed
#Warning message:
#In evalq(as.numeric(replace(Weight, Weight == "NA", NA)), <environment>) :
#  NAs introduced by coercion

#plot
count <- as.data.frame(table(wweights$Cat))
plot <- ggplot(ggdata, aes(x = Week, y = Weight, group = Animal_ID, colour = Cat, shape = Cat)) +
              geom_point(size= 2) +
              geom_line() +
              labs( title = "1713 Col4a5xRfx3 Weekly Weights",
                    subtitle =paste0("Cohort A Long Female Col4a5-Het/Rfx3-WT: ", count[count$Var1 %in% "A_Long",]$Freq, " of 30", "\n",
                                     "Cohort B Long Female Col4a5-Het/Rfx3-Het: ", count[count$Var1 %in% "B_Long",]$Freq, " of 30", "\n",
                                     "Cohort C Long Male Col4a5-Mut/Rfx3-WT: ", count[count$Var1 %in% "C_Long",]$Freq, " of 20", "\n",
                                     "Cohort D Long Male Col4a5-Mut/Rfx3-Het: ", count[count$Var1 %in% "D_Long",]$Freq, " of 20", "\n",
                            "Last Update: ", Sys.Date()),
                    y = "Weights (g)",
                    x = "Weeks",
                    colour = "Cohorts",
                    shape = "Cohorts") +
              theme(text = element_text(size = 20),
                    axis.text.x = element_text(angle =45,hjust=1))+
              scale_color_aaas()

pdf(outfile, width = 12, height = 8)
plot
dev.off()

# Check if any animals droped their weight by 15%
wweights$drop15 <- NA
for (i in 1:nrow(wweights)){
  row <- wweights[i, wks]
  row[which(row == "NA")] <- NA

  # if animal hasn't bee phenotyped yet (no week 6 data)
  if(is.na(row)[1]){
    wweights$drop15[i] <- FALSE
    next
  }

  # otherwise try to calculate weight drop
  row <- as.numeric(row[!is.na(row)[1,]])
  last <-row[length(row)]
  max <- max(row, na.rm = TRUE)
  wweights$drop15[i] <- (last / max) < 0.85
}

# Account for already euthanized animals
wweights <- mutate(wweights, drop15 = replace(drop15, Euth == TRUE, TRUE))

# Print message to indicate animal health
if(any(wweights$drop15, na.rm = TRUE)){
  print("Following animals have dropped their weight by 15%!")
  print(wweights[wweights$drop15 %in% TRUE,]$Animal_ID)
} else if (!any(wweights$drop15, na.rm = TRUE)){
  print("Keep going, all animals have maintained a healthy weight")
} else {
  print("Something is wrong, check Rscript!")
}
