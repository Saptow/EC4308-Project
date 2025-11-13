# EC4308-Project
This is the repository for EC4308 Project for Group 9. Code structure is as follows:

## Code Structure for EC4308 Project
- `ADL/`: Contains all R scripts related to the ADL model implementation
- `AR(p)/`: Contains all R scripts related to the AR model implementation
- `data/`: Contains all data files used in the project
- `data_transformation/`: Contains all R scripts related to variable transformations (MAF & MARX)
- `dmtest/`: Contains all R scripts related to DM test for our results analysis
- `hybrid/`: Contains all R scripts related to the Hybrid model implementation
- `Lasso/`: Contains all R scripts related to the Lasso model implementation
- `Tree/`: Contains all R scripts related to the Random Forest model implementation
- `README.md`: This file, which provides an overview of the project

## General Instructions
1. Each model folder contains R scripts specific to that model.
2. Ensure you install any required packages before running the scripts. (e.g., `ranger` for Random Forest, check individual scripts for specific package requirements)
3. Each folder generally contains a main script to run the model and additional helper scripts (`func_/-*.R`) as needed, with the exception of LASSO which uses a single script.
4. All used data files are stored in the `data/` folder. The `fred.R` script is used to fetch data from the FRED database and do a bit of cleaning and the `structural_breaks.R` script is used to identify structural breaks in the data. Initial data can be found [here](https://www.stlouisfed.org/research/economists/mccracken/fred-databases). 
5. Variable transformation scripts are located in the `data_transformation/` folder. Implemented according to Coloumbe et al. (2021) Paper *Macroeconomic Data Transformations matter*.


