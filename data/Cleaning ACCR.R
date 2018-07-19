# load packages
library(readr)
library(dplyr)

# read in data
accr <- read_delim("~/Kitu/College/Senior Year/Extracurriculars/Data Science Research Internship/Significant Regions Mapped/AllCaseReports-Mar_13_2017.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

# use only relevant variables
accr <- accr[ , c(2, 9, 11, 13, 43:58)]

# add decade
accr$decade <- accr$Year - accr$Year %% 10
accr$decade <- as.factor(accr$decade)

# reorder variables
accr <- accr[c(1:2, 23, 3:22)]

# load location file
load("~/Kitu/College/Senior Year/Extracurriculars/Data Science Research Internship/ACCR vs PMID/ACCR cleaned.Rdata")
accr_locs <- data
rm(data)

# remove duplicates
accr_locs <- accr_locs[-which(duplicated(accr_locs$PMID)), ]

# join location with data
accr <- left_join(x = accr, y = accr_locs[ , c(1, 3:4)], by = "PMID")

# save dataframe
save(accr, file = "cleaned_accr.Rdata")

###### cleaning all case reports
load("~/Kitu/College/Senior Year/Extracurriculars/Data Science Research Internship/David's Data/HFpEF vs HFrEF/Heart Failure vs All Case Reports/location.Rdata")

# create tables for country and state
country_all <- data.frame(table(location$country))
us_state_all <- data.frame(table(location$us_state))

# add proportions
country_all$prop <- country_all$Freq/sum(country_all$Freq)
us_state_all$prop <- us_state_all$Freq/sum(us_state_all$Freq)

# save files
save(country_all, file = "country_all.Rdata")
save(us_state_all, file = "us_state_all.Rdata")
