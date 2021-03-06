# R/3.4.3 kite-eating tree
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
indir <- "~/Dropbox/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data"
outfile <- "~/Dropbox/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data/1713_GFRplot.pdf"

# Get all files and gather them into one
files <- list.files(path = indir, pattern = "xlsx", full.names = TRUE)
data_all <- NULL
for(i in 1:length(files)){
  # Read in data
  data <- read_excel(files[i]) %>%
            select(Line, Name, Sex, Cohort, Age, "Weight (g)", "Injection volume (ul)", "T1/2","GFR ul/min", "CI Min", "CI Max", "CI Difference", "Peak", "Baseline","Peak to baseline ratio","R2") %>%
            rename(Weight = "Weight (g)",
                   Inj_vol = "Injection volume (ul)",
                   T_half = "T1/2",
                   GFR = "GFR ul/min",
                   CI_min = "CI Min",
                   CI_max = "CI Max",
                   CI_diff = "CI Difference",
                   Peak2BaseRatio = "Peak to baseline ratio") %>%
            filter(Name != is.na(Name) & Line == "1713") %>%
            mutate(
              Weight = as.numeric(Weight),
              Inj_vol = as.numeric(Inj_vol),
              T_half = as.numeric(T_half),
              GFR = as.numeric(GFR),
              CI_min = as.numeric(CI_min),
              CI_max = as.numeric(CI_max),
              CI_diff = as.numeric(CI_diff),
              Peak2BaseRatio = as.numeric(Peak2BaseRatio),
              R2 = as.numeric(R2)) %>%
            arrange(Name)

  # Add to the rest of the dataset
  data_all <- rbind(data_all, data) %>% arrange(Name)
}

# Run QC:
data_all$QC_pass <- NA
for( i in 1:nrow(data_all)){
  if(is.na(data_all$CI_diff[i]) | is.na(data_all$Peak2BaseRatio[i])){
    next
  } else if(data_all$CI_diff[i] > 2 | data_all$Peak2BaseRatio[i] < 3){
    data_all$QC_pass[i] <- FALSE
  } else {
    data_all$QC_pass[i] <- TRUE
  }
}
data_all <- data_all[data_all$QC_pass %in% TRUE,]

# Box plot and anova
plot <- ggplot(data_all, aes(x= Cohort, y= GFR, fill = Cohort))+
  geom_boxplot() +
  labs( title = "1713 Col4a5xRfx3 GFR",
        subtitle = paste0("Last Update: ", Sys.Date(), "\n",
                  "Cohort A Long Female Col4a5-Het/Rfx3-WT: ", table(data_all$Cohort)[[1]], " of 30", "\n",
                  "Cohort B Long Female Col4a5-Het/Rfx3-Het: ", table(data_all$Cohort)[[2]], " of 30", "\n",
                  "Cohort C Long Male Col4a5-Mut/Rfx3-WT: ", table(data_all$Cohort)[[3]], " of 20", "\n",
                  "Cohort D Long Male Col4a5-Mut/Rfx3-Het: ", table(data_all$Cohort)[[4]], " of 20", "\n"))+
  theme(text = element_text(size = 15))+
  scale_fill_aaas()

pdf(outfile, width = 12, height = 8)
plot
dev.off()

# ANOVA
data_all$Cohort <- as.factor(data_all$Cohort)
anov <- aov(GFR ~ Cohort, data = data_all)
print("ANOVA test ALL groups: ")
print(summary(anov))
TukeyHSD(anov,"Cohort")
