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
    transform=FALSE
)

# Save the processed data as an R data file
save(md, file = "./data/raw_fredmd.RData")

# To load the data in future sessions, use:
# load("./data/fredmd.RData")