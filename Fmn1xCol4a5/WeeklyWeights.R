# R/3.5.0 Joy in Playing
library(ggplot2)
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
<<<<<<< HEAD
#infile <- "~/Desktop/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/1718 Col4a5xFmn1 experimental cohort and schedule.xlsx"
#outfile <- "~/Desktop/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/WeeklyWeights.pdf"
infile <- "~/Desktop/1718 Col4a5xFmn1 experimental cohort and schedule.xlsx"
outfile <- "~/Desktop/WeeklyWeights.pdf"
=======
infile <- "~/Dropbox/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/1718 Col4a5xFmn1 experimental cohort and schedule.xlsx"
outfile <- "~/Dropbox/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/WeeklyWeights.pdf"
>>>>>>> 7bb61916ed5b70fbde479cfc50c6b7fe3fbf8ea1

# read file
wweights <- read_excel(infile, sheet = "1718_weekly_weights")

# tidy dataframe
wweights$DOB <- ymd(wweights$DOB)
wweights$StartDate <- ymd(wweights$StartDate)
wweights$Sex <- parse_factor(wweights$Sex, levels = unique(wweights$Sex))
wweights$Col4a5_geno <- parse_factor(wweights$Col4a5_geno, levels = unique(wweights$Col4a5_geno))
wweights$Fmn1_geno <- parse_factor(wweights$Fmn1_geno, levels = unique(wweights$Fmn1_geno))
wweights$Cat <- parse_factor(wweights$Cat, levels = c("A_Long", "B_Long", "C_Long", "D_Long","NA_Long")) # adding NA as temp place holder while genotyping is being sorted out

# Identify outliers and drop from plot and calculations
# Not outliers yet

# gather by weeks and prepare for plotting
wks <- colnames(wweights)[grep("Wk", colnames(wweights))]
ggdata <- wweights %>%
          gather(wks, key = "Week", value = "Weight") %>%
          mutate(Weight=as.numeric(replace(Weight,Weight=="NA", NA))) %>%
          filter(!is.na(Weight))

#plot
count <- as.data.frame(table(wweights$Cat))
plot <- ggplot(ggdata, aes(x = Week, y = Weight, group = Animal_ID, colour = Cat, shape = Cat)) +
              geom_point(size = 2) +
              geom_line() +
              labs( title = "1718 Col4a5xFmn1 Weekly Weights",
                    subtitle =paste0("Cohort A Long Female Col4a5-Het/Fmn1-WT: ", count[count$Var1 %in% "A_Long",]$Freq, " of 30", "\n",
                                     "Cohort B Long Female Col4a5-Het/Fmn1-Het: ", count[count$Var1 %in% "B_Long",]$Freq, " of 30", "\n",
                                     "Cohort C Long Male Col4a5-Mut/Fmn1-WT: ", count[count$Var1 %in% "C_Long",]$Freq, " of 20", "\n",
                                     "Cohort D Long Male Col4a5-Mut/Fmn1-Het: ", count[count$Var1 %in% "D_Long",]$Freq, " of 20", "\n",
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
  print(paste("Total of ",length(wweights[wweights$drop15 %in% TRUE,]$Animal_ID), "mice. Please confirm with spread sheet."))
} else if (!any(wweights$drop15, na.rm = TRUE)){
  print("Keep going, all animals have maintained a healthy weight")
} else {
  print("Something is wrong, check Rscript!")
}
