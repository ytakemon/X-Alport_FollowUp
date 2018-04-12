# R/3.4.3 kite-eating tree
library(ggplot2)
library(tidyverse)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
infile <- "~/Desktop/1706 and 1713 Col4a5xRfx3/Weights/Data/1713_weekly_weights.csv"
outfile <- "~/Desktop/1706 and 1713 Col4a5xRfx3/Weights/Data/WeeklyWeights.pdf"

# read file
wweights <- read_csv(infile)

# tidy dataframe
wweights$DOB <- mdy(wweights$DOB)
wweights$Sex <- parse_factor(wweights$Sex, levels = unique(wweights$Sex))
wweights$Col4a5_geno <- parse_factor(wweights$Col4a5_geno, levels = unique(wweights$Col4a5_geno))
wweights$Rfx3_geno <- parse_factor(wweights$Rfx3_geno, levels = unique(wweights$Rfx3_geno))
wweights$Cat <- parse_factor(wweights$Cat, levels = c("A_Long", "B_Long", "C_Long", "D_Long"))

# gather by weeks and prepare for plotting
wks <- colnames(wweights)[grep("Wk", colnames(wweights))]
ggdata <- wweights %>%
          gather(wks, key = "Week", value = "Weight") %>%
          filter(!is.na(Weight))

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
              scale_color_aaas()

pdf(outfile, width = 12, height = 8)
print(plot)
dev.off()

# Check if any animals droped their weight by 15%
wweights$drop15 <- NA
for (i in 1:nrow(wweights)){
  if(is.na(which(is.na(wweights[i, wks]))[1])){
    last <- ncol(wweights[i, wks])
    last <- wweights[i, wks][last]
    max <- max(wweights[i, wks], na.rm = TRUE)
    wweights$drop15[i] <- (last / max) < 0.85
  } else if(which(is.na(wweights[i, wks]))[1] - 1 == 0){
    wweights$drop15[i] <- NA
  } else {
    last <- which(is.na(wweights[i, wks]))[1] - 1
    last <- wweights[i, wks][last]
    max <- max(wweights[i, wks], na.rm = TRUE)
    wweights$drop15[i] <- (last / max) < 0.85
  }
}

# Print message to indicate animal health
if(any(wweights$drop15, na.rm = TRUE)){
  print("Following animals have dropped their weight by 15%!")
  wweights[wweights$drop15 == TRUE,]
} else if (!any(wweights$drop15, na.rm = TRUE)){
  print("Keep going, all animals have maintained a healthy weight")
} else {
  print("Something is wrong, check Rscript!")
}
