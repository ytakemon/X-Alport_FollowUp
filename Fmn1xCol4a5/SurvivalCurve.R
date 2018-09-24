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
infile <- "~/Dropbox/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/1718 Col4a5xFmn1 experimental cohort and schedule.xlsx"
outfile <- "~/Dropbox/Col4a5_FollowupStudies/1717 and 1718 Col4a5xFmn1/Weights/SurvivalCurve_1718.pdf"

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
  # skip if animal is still alive
  if (is.na(wk)){
    next
  }
  # If end date is present change TodayWeeks and assign life stat
  surv[surv$Animal_ID == ID & surv$EndWeeks == wk,]$TodayWeeks <- wk
  surv[surv$Animal_ID == ID & surv$EndWeeks == wk,]$LifeStat <- 1
}

# Fit model
fit <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv) # all
fit_M <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv[surv$Sex == "M",]) # males only
fit_F <- survfit(Surv(TodayWeeks, LifeStat) ~ Cat, data = surv[surv$Sex == "F",]) # females only

# Visualize
plot <- ggsurvplot(fit,
  data = surv,
  # Change legends: title & labels
  legend.title = "Genetype",
  legend.labs = c("Female:Col4a5–Fmn1^{WT}", "Female:Col4a5–Fmn1KO","Male:Col4a5–Fmn1WT", "Male:Col4a5–Fmn1KO"),
  # Add medians survival
  surv.median.line = "hv",
  # Add risk table
  risk.table = TRUE,
  tables.theme = theme_cleantable(),
  color = "strata",
  palette = pal_aaas("default")(5),
  pval = TRUE,
  xlim = c(0:max(surv$TodayWeeks)),
  break.time.by = 2,
  title = "1718 Survival Curve",
  subtitle = paste("Last Update:", Sys.Date(), "Oldest mouse is", max(surv$TodayWeeks), "wks old"),
  font.title = 10,
  font.subtitle = 10,
  fontsize = 4,
  ggtheme = theme_bw())

# Visualize
plotM <- ggsurvplot(fit_M,
  data = surv,
  # Change legends: title & labels
  legend.title = "Genetype",
  legend.labs = c("Col4a5–Fmn1WT", "Col4a5–Fmn1KO"),
  # Add medians survival
  surv.median.line = "hv",
  # Add risk table
  risk.table = TRUE,
  tables.theme = theme_cleantable(),
  color = "strata",
  palette = pal_aaas("default")(5),
  pval = TRUE,
  xlim = c(0:max(surv$TodayWeeks)),
  break.time.by = 2,
  title = "1718 Survival Curve (Males)",
  subtitle = paste("Last Update:", Sys.Date(), "Oldest mouse is", max(surv$TodayWeeks), "wks old"),
  font.title = 10,
  font.subtitle = 10,
  fontsize = 4,
  ggtheme = theme_bw())

plotF <- ggsurvplot(fit_F,
  data = surv,
  # Change legends: title & labels
  legend.title = "Genetype",
  legend.labs = c("Col4a5–Fmn1WT", "Col4a5–Fmn1KO"),
  # Add medians survival
  surv.median.line = "hv",
  # Add risk table
  risk.table = TRUE,
  tables.theme = theme_cleantable(),
  color = "strata",
  palette = pal_aaas("default")(5),
  pval = TRUE,
  xlim = c(0:max(surv$TodayWeeks)),
  break.time.by = 2,
  title = "1718 Survival Curve (Females)",
  subtitle = paste("Last Update:", Sys.Date(), "Oldest mouse is", max(surv$TodayWeeks), "wks old"),
  font.title = 10,
  font.subtitle = 10,
  fontsize = 4,
  ggtheme = theme_bw())




pdf(outfile, width = 8, height = 8, onefile = FALSE) #force into single page to get ride of first blank page
plot
dev.off()
