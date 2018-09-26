# R/3.5.0 Joy in Playing
# reference:
# https://cran.r-project.org/web/packages/survminer/vignettes/Informative_Survival_Plots.html
library(tidyverse)
library(readxl)
library(lubridate)
library(survminer)
library(survival)
library(ggsci)
options(tibble.width = Inf)
infile <- "~/Dropbox/Col4a5_FollowupStudies/1706 and 1713 Col4a5xRfx3/Weights/1713 Col4a5xRfx3 experimental cohort and schedule.xlsx"
outfile <- "~/Dropbox/Col4a5_FollowupStudies/1706 and 1713 Col4a5xRfx3/Weights/1713_SurvivalCurve"

# read data and extract relevant columns
surv <- read_excel(infile, sheet = "Euthanized_date")
# Calculate number of days/weeks lived
# 0 = alive
# 1 = dead
surv <- surv[,colnames(surv)[1:13]] %>%
  mutate(StartDate = ymd(StartDate),
    EndDate = ymd(EndDate),
    Today = ymd(as.character(Sys.Date())),
    EndWeeks = as.numeric(round((as.numeric(EndDate - StartDate)/7)/ 0.5)* 0.5) + 6, # add 6 weeks as start of phenotyping
    TodayWeeks = as.numeric(round((as.numeric(Today - StartDate)/7)/ 0.5)* 0.5) + 6, # add 6 weeks as start of phenotyping
    LifeStat = as.numeric(0)
  ) # rounds weeks to the nearest half week

# some animals are not yet 6wks old so corece them to 0 weeks
if(any(surv$TodayWeeks < 0)){
  surv[surv$TodayWeeks < 0, ]$TodayWeeks <- 0
}

# Assign death date and change life stat
for (i in 1:nrow(surv)){
  wk <- surv$EndWeeks[i] # grab week of death
  ID <- surv$Animal_ID[i] # grab animal ID

  if (is.na(wk)){
    next # skip if animal is still alive?
  }

  # If end date is present change TodayWeeks and assign life stat
  surv[surv$Animal_ID == ID & surv$EndWeeks == wk,]$TodayWeeks <- wk
  surv[surv$Animal_ID == ID & surv$EndWeeks == wk,]$LifeStat <- 1
}

# Fit model
#fit <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv) # all
fit_M <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv[surv$Sex == "M",]) # males only
fit_F <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv[surv$Sex == "F",]) # females only

# Visualize
plotM <- surv %>% filter(Sex == "M") %>% ggsurvplot(fit_M,
  data = .,
  # Change legends: title & labels
  legend.title = "Genetype",
  legend.labs = c("Col4a5–Rfx3WT", "Col4a5–Rfx3KO"),
  # Add medians survival
  surv.median.line = "hv",
  # Add risk table
  risk.table = TRUE,
  tables.theme = theme_cleantable(),
  color = "strata",
  palette = pal_aaas("default")(5),
  pval = TRUE,
  xlim = c(0:max(.$TodayWeeks)),
  break.time.by = 2,
  title = "1713 Survival Curve (Males)",
  subtitle = paste("Last Update:", Sys.Date(), "Oldest mouse is", max(.$TodayWeeks), "wks old"),
  font.title = 10,
  font.subtitle = 10,
  fontsize = 4,
  ggtheme = theme_bw())

plotF <- surv %>% filter(Sex == "F") %>% ggsurvplot(fit_F,
  data = .,
  # Change legends: title & labels
  legend.title = "Genetype",
  legend.labs = c("Col4a5–Rfx3WT", "Col4a5–Rfx3KO"),
  # Add medians survival
  surv.median.line = "hv",
  # Add risk table
  risk.table = TRUE,
  tables.theme = theme_cleantable(),
  color = "strata",
  palette = pal_aaas("default")(5),
  pval = TRUE,
  xlim = c(0:max(.$TodayWeeks)),
  break.time.by = 2,
  title = "1713 Survival Curve (Females)",
  subtitle = paste("Last Update:", Sys.Date(), "Oldest mouse is", max(.$TodayWeeks), "wks old"),
  font.title = 10,
  font.subtitle = 10,
  fontsize = 4,
  ggtheme = theme_bw())

pdf(paste0(outfile,"_male.pdf"), width = 8, height = 8, onefile = FALSE) #force into single page to get ride of first blank page
plotM
dev.off()

pdf(paste0(outfile, "_female.pdf"), width = 8, height = 8, onefile = FALSE) #force into single page to get ride of first blank page
plotF
dev.off()
