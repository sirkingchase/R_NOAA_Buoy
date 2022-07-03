#works with meteorological data available from https://www.ndbc.noaa.gov/
#ex. https://www.ndbc.noaa.gov/download_data.php?filename=42020h2021.txt.gz&dir=data/historical/stdmet/
library(units)
library(purrr)
library(dplyr)
library(measurements)

#first comment are the headers
headers = read.table("42020h2021.txt", skip = 0, header = FALSE, nrows = 1, as.is = T, comment.char = "")
x<-read.table("42020h2021.txt", header=FALSE)
colnames(x)= headers
names(x)[1] <- "YY" #remove that prepended comment

#remove incomplete data
x <- filter(x, WVHT < 99 & DPD < 99)

headerMeta = read.table("42020h2021.txt", skip = 1, header = FALSE, nrows = 1, as.is = T, comment.char = "")
#https://www.unidata.ucar.edu/software/udunits/udunits-2.2.28/udunits2-common.xml
#the second comment in these files are the units of measure, some need to be renamed for r units

headerMeta[1, c("V1")] <- "yr"
headerMeta[1, c("V2")] <- "month"
headerMeta[1, c("V3")] <- "day"
headerMeta[1, c("V5")] <- "minute"
headerMeta[1, c("V6")] <- "degreeW"
headerMeta[1, c("V12")] <- "degreeW"
units = headerMeta 

#pair the meta data with the column headers
data_frame = x %>% map2_dfc(units,  ~set_units(.x, .y, mode = "standard"))

#by(data_sorted[!(WVHT %in% c(99))],

#https://r-quantities.github.io/units/articles/measurement_units_in_R.html
#https://www.rdocumentation.org/packages/units/versions/0.8-0/topics/units
#lets do some converstion from metric to imperial

units(data_frame$WVHT) <- make_units(ft)
units(data_frame$WSPD) <- make_units(kts) #knots AKA mph
units(data_frame$GST) <- make_units(kts)

units(data_frame$ATMP) <- make_units(degrees_F)
units(data_frame$WTMP) <- make_units(degrees_F)
units(data_frame$DEWP) <- make_units(degrees_F)

data_frame <- round(data_frame, digits=1) #round columns to nearest tenth ex. 1.1

#create a string from the dates
date <- sprintf("%s-%s-%s %s:%s", 
                data_frame$YY,
                data_frame$MM,
                data_frame$DD,
                data_frame$hh,
                data_frame$mm
                )

#make datetime object from string
DATETIME = strptime(date, format="%Y-%m-%d %H:%M")

#add as first column in dataframe
data_frame <-cbind(DATETIME,data_frame)



#TODO: Analyze trends in dominate wave periods & wave heights
#TODO: Marry this data with over sources ex. historical weather reports like was it raining that day?


# order by wave height (ex. biggest waves)
data_sorted <- data_frame[
    order(
      data_frame$WVHT, 
      #data_frame$DPD,
      decreasing = TRUE), 
  ]

# for each wave group select the top 3 with the longest dominate wave period
data_mod <- Reduce(rbind,                               
                   by(data_sorted,
                      data_sorted["DPD"],
                      #data_sorted["WVHT"],
                      head,
                      n = 3))

#order this data by DPD
# data_month <- data_mod[
#   order(
#     data_mod$DPD, 
#     decreasing = TRUE), 
# ]

#for each month select the top 3 DPD? not sure what its actualing doing here
#this orders it by month somehow...
data_plot <- Reduce(rbind,                               
                   by(data_mod,
                      data_mod["DATETIME"],
                      #data_sorted["WVHT"],
                      head,
                      n = 3))

#remove the old columns before printing/export
data_plot <- data_plot %>% select(-YY,-MM,-DD,-hh,-mm)



#print (data_mod)
write.csv(data_plot, "dominateWavePeroid.csv", row.names=TRUE)


with(data_plot,matplot(x=DATETIME,y=cbind(WVHT,DPD, WSPD),type="l"))

