# R/3.4.3 kite-eating tree
library(tidyverse)
library(readxl)
library(lubridate)
library(rlang)
library(ggsci)
options(tibble.width = Inf)
indir <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data"
outfile <- "~/Desktop/Col4a5_FollowupStudies/FollowupStudies_GFR/cleaned_data/1713_GFRplot.pdf"

# Get all files and gather them into one
files <- list.files(path = indir, pattern = "xlsx", full.names = TRUE)
data_all <- NULL
for(i in 1:length(files)){
  # Read in data
  data <- read_excel(files[i]) %>%
            select(Line, Name, Sex, Cohort, Age, "Weight (g)", "Injection volume (ul)", "T1/2","GFR ul/min", "CI Min", "CI Max", "CI Difference", R2) %>%
            rename(Weight = "Weight (g)",
                   Inj_vol = "Injection volume (ul)",
                   T_half = "T1/2",
                   GFR = "GFR ul/min",
                   CI_min = "CI Min",
                   CI_max = "CI Max",
                   CI_diff = "CI Difference") %>%
            filter(Name != is.na(Name) & Line == "1713") %>%
            arrange(Name)

  # Add to the rest of the dataset
  data_all <- rbind(data_all, data) %>% arrange(Name)
}

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
print("ANOVA test: ")
print(summary(anov))
