-- Getting the URL handle
con = url("http://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for")
-- Reading the file in a data frame using "read.fwf", skipping the 1st four rows.
-- Defining columns with "widths" arrtibute
--  03JAN1990     23.4-0.4     25.1-0.3     26.6 0.0     28.6 0.3
-- *---------*****----####*****----####*****----####*****----####
-- 1   9       5   4    4   5   4    4   5    4   4   5    4  4 (The stars (values in -ve) are skipped)  
df <- read.fwf(con, widths = c(-1,9,-5,4,4,-5,4,4,-5,4,4,-5,4,4), skip=4)
-- checking the class
class(df)
-- [1] "data.frame"
-- checking the names
names(df)
-- [1] "V1" "V2" "V3" "V4" "V5" "V6" "V7" "V8" "V9"
-- printing the result
sum(df[,4]) 
