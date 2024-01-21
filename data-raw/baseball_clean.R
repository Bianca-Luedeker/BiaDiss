# This file cleans the Null baseball data and stores it in an Rda file.

# Packages

library(tidyverse)        ## For Data management.

## Reads in the data from the original file I received from Dr. Brad Null.

batters.data <- read.table("data-raw/batters_by_year.txt", header=FALSE, sep=",")



## Groups the data by batter and season year.
## Assigns descriptive titles to the columns.
## data_2: Intermediate data set with ID, birth year, and number of each type of
## outcome with no difference between handiness of pitcher.
## I had to combine rows where handiness of pitcher was distinguished.
## This is no longer in the data_2 data set.
## Note: KO is a strike-out.

data_2 <- batters.data %>%
  group_by(V1, V2) %>%
  dplyr::summarise( "BIRTH" = mean(V3), "INT" = sum(V7), "HBP"= sum(V8),
                    "BB" = sum(V9), "KO" = sum(V10), "FBHR" = sum(V11),
                    "FB3B"= sum(V12), "FB2B" = sum(V13), "FB1B" = sum(V14),
                    "FB0" = sum(V15), "GBHR" = sum(V16), "GB3B" = sum(V17),
                    "GB2B" = sum(V18), "GB1B" = sum(V19), "GB0" = sum(V20)) %>%
  as.data.frame()

#View(data_2)

## data_3: Intermediate data set with ID, birth year, season year
## and six outcome counts: Home Run, Third Base, Second Base, First Base,
## Out, and Other.

data_3 <- data_2 %>%
  dplyr::mutate("HR" = FBHR + GBHR, "TRIPLE" = FB3B + GB3B, "DOUBLE" = FB2B + GB2B,
                "SINGLE" = FB1B + GB1B, "OUT" = KO + FB0 + GB0,
                "OTHER" = INT + HBP + BB,
                "TOTAL" = INT+HBP+BB+KO+FBHR+FB3B+FB2B+FB1B+FB0+GBHR+GB3B+
                  GB2B+GB1B+GB0) %>%
  select(1:3, 18:24) %>%
  as.data.frame()

#View(data_3)

## data_4:  Intermediate data set with ID, Birth Year, Season Year, and
## proportions of six outcomes:  percent home run, percent third base,
## percent second base, percent first base, percent out, and percent other.
## Also includes a column total which is the total of all outcomes for
## an individual batter in that year.

data_4 <- data_3 %>%
  dplyr::mutate( "AGE" = V2 - BIRTH, "HR" = HR/TOTAL, "TRIPLE" = TRIPLE/TOTAL,
                 "DOUBLE" =DOUBLE/TOTAL, "SINGLE" = SINGLE/TOTAL, "OUT" = OUT/TOTAL,
                 "OTHER" = OTHER/TOTAL) %>%
  dplyr::rename("ID"= V1, "Year" = V2) %>%
  select(1, 11, 4:10) %>%
  as.data.frame()

#View(data_4)

## Classify each batter in each season year into one of three age groups.
## Age Group 1: Age is less than 25 years.
## Age Group 2: Age is greater than or equal to 25 but less than 35.
## Age Group 3: Age is greater than or equal to 35.
## This is stored in the column Group.

data_4$GROUP <- as.factor(
  ifelse(data_4$AGE < 25, 1, ifelse(data_4$AGE <35, 2, 3)))

#View(data_4)
#str(data_4)

## Move the Group column so that it is the third column.

BattersGrouped <- data_4 %>% relocate(GROUP, .before = HR)

#View(BattersGrouped)

# Save the data frame to the data/ directory as BattersGrouped.rda
usethis::use_data(BattersGrouped, overwrite=TRUE)

