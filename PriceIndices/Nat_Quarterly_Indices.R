
#  R Script for Quarterly Price Indices
    # For national-level monthly average food prices

#  WFP Indonesia Country Office - Vulnerability Analysis and Mapping (VAM)

#  R Package needed
    #Install the following packages before running the script

    install.packages(c('lubridate', 'dplyr', 'zoo', 'xlsx', 'openxlsx'), dependencies = T)

    library(lubridate)
    library(dplyr)
    library(zoo)
    library(xlsx)
    library(openxlsx)

# Call the monthly price dataset

    data <- read.csv("monthly.csv", header = T)
    str(data) # Check the dataset structure

# Set the date format

    data <- mutate(data, date=as.Date(Date,format='%d/%m/%Y'))
    data <- data %>%
        filter(between(date, as.Date("2017-01-01"),as.Date("2020-06-01"))) # Adjust with the timeseries data
    
#----------------------Month-on-Month and Year-on-Year------------------------------------------------------#

# Generate the monthly average price
    
    data_monthly <- data %>%
        mutate(
            Monthly = format(date, "%Y-%m")
        )%>%
        group_by(Commodity, Monthly)%>%
        summarize(
            Monthly_Price = mean(Price)
        )%>%
        arrange(Commodity, Monthly)
    
# Calculate:
    # Month-on-Month (MoM) : Monthly change from the first month in the given quarter
    # Year-on-Year (YoY) : Monthly change from the same month last year
    
    MM <- data_monthly %>%
        mutate(
            MoM = (Monthly_Price - lag(Monthly_Price, 1)) / lag(Monthly_Price, 1)*100,
            YoY = (Monthly_Price - lag(Monthly_Price, 12)) / lag(Monthly_Price, 12)*100
        ) %>%
        arrange(Commodity, Monthly)
    
#------------------Quarterly Change from Previous Quarter and Quarterly Change from Last Year----------------#
    
# Generate the quarterly average price
    
    data_quarter <- data %>%
        mutate(
            Quarter = as.yearqtr(date, "%Y-Q%q")
        )%>%
        group_by(Commodity, Quarter)%>%
        summarize(
            Quarterly_Price = mean(Price)
        )%>%
        
        arrange(Commodity, Quarter)
    
# Calculate:
    # Quarterly Change from Previous Quarter (QCPQ)
    # Qurterly Change from Last Year (QCLY)
    
    QQ <- data_quarter %>%
        mutate(
            QCPQ = (Quarterly_Price - lag(Quarterly_Price)) / lag(Quarterly_Price)*100,
            QCLY = (Quarterly_Price - lag(Quarterly_Price, 4)) / lag(Quarterly_Price, 4)*100
            ) %>%
        arrange(Commodity, Quarter)

#-----------------Seasonally Adjusted Quarterly Change and Quarterly Change from Baseline---------------------#
    
# Generate the seasonally adjusted price
    
    # Generate the long-term average by Month (Baseline)
    
    lt_data <- data %>%
        filter(between(date, as.Date("2017-01-01"),as.Date("2019-12-01")))
    
    lt_average <- lt_data %>%
        mutate(
            Month = format(date, "%m")
        )%>%
        group_by(Commodity, Month)%>%
        summarize(
            Baseline = mean(Price)
        )%>%
        arrange(Commodity, Month)
    
    # Convert column "Month" to integer type (Optional) = Ad Hoc adjustment
        #Purpose: To join using the column containing the same value
    
    lt_average <- lt_average %>%
       mutate(Month = as.integer(Month))
    
    # Join dataframe

    lt_join <- left_join(data, lt_average, by = c("Commodity", "Month"))
    
    # Calculate the real price
    
    real <- lt_join %>%
        mutate(real_price = (Price/Baseline))
    
    real_quarter <- real %>%
        mutate(
            Quarter = as.yearqtr(date, "%Y-Q%q")
        )%>%
        group_by(Commodity, Quarter)%>%
        summarize(
            Q_real_price = mean(real_price)
        )%>%
        arrange(Commodity, Quarter)
    
# Calculate: Seasonally Adjusted Quarterly Change (SAQC)
    
    SAQC_data <- real_quarter %>%
            mutate(SAQC = (Q_real_price - lag(Q_real_price)) / lag(Q_real_price)*100)
    
# Calculate: Quarterly Change from Baseline (QCB)
    
    QCB_data <- lt_join %>%
        mutate(QCB_diff = (Price - Baseline) / Baseline*100) %>%
        mutate(
            Quarter = as.yearqtr(date, "%Y-Q%q")
        )%>%
        group_by(Commodity, Quarter)%>%
        summarize(
            QCB = mean(QCB_diff)
        )%>%
        arrange(Commodity, Quarter)
    
#---------------Convert the dataframes to an Excel with multiple sheets (.xlsx)-----------------------#
    
    list_dataframe <- list(MM, QQ, QCB_data, SAQC_data)
    names(list_dataframe) <- c("MM", "QQ", "QCB", "SAQC")
    write.xlsx(list_dataframe, file = 'IDN Q2 2020.xlsx', )
    
    
