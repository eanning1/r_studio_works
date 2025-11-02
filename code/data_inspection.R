#Setting Working directory
setwd("D:\\Codes\\R_Codes\\r_studio_works\\raw_data")

# Read the RDS file
atm_data <- readRDS("ATM10_data.rds")
error_data <- readRDS("Errors_company01.rds")

# Check what the object contains in the atm data
class(atm_data)
str(atm_data)
head(atm_data)

# Check what the object contains in the atm data
class(error_data)
str(error_data)
head(error_data)


atm_data <- readRDS("ATM10_data.rds")
error_data <- readRDS("Errors_company01.rds")


# Understanding the data
summary(atm_data)
View(atm_data)  


summary(error_data)
View(error_data)  

