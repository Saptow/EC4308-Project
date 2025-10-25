library(fbi)
file_md <- "https://research.stlouisfed.org/econ/mccracken/fred-databases/Monthly/current.csv"

md=fredmd(
    file='C:/Users/rob-l/Documents/NUS/Y4S1/EC4308/EC4308-Project/data/2025-09-MD.csv', 
    date_start=as.Date("1980-01-01"),
    date_end=as.Date("2020-01-01"),
    transform=TRUE
)
md
