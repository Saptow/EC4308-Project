## Quick setup of the fbi library
# install.packages("devtools") # install this so you can install github repos
## Other prerequisites to install
# install.packages("stats")
# install.packages("readr")
# install.packages("pracma")
# devtools::install_github("cykbennie/fbi") # install fbi from github

# Load fbi library
library(fbi)

# Read fredMD data and apply respective transformations
md=fredmd(
    file='./data/2025-09-MD.csv', 
    date_start=as.Date("1980-01-01"),
    date_end=as.Date("2019-12-01"),
    transform=TRUE
)

# Save the processed data as an R data file
save(md, file = "./data/fredmd.RData")

# To load the data in future sessions, use:
# load("./data/fredmd.RData")


# ---------------------------------------------
# Clean up FRED data 
# ---------------------------------------------
load('./data/fredmd.RData')
md=data.frame(md) # convert to data.frame
class(md) # check that this is data.frame
library(dplyr)
# drop New Orders for Consumer Goods (ACOGNO) due to insufficient data
md <- md %>% select(-ACOGNO)
# introduce dummy variable from dec 2010 onwards for structural break
md <- md %>%
  mutate(
    aft_break = ifelse(date >= as.Date("2010-12-01"), 1, 0),
    .keep="all"
  )
# save cleaned data
save(md, file = "./data/fredmd_cleaned.RData")
