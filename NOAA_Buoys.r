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
names(x)[1] <- "YY" #remove that comment

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

#https://r-quantities.github.io/units/articles/measurement_units_in_R.html
#https://www.rdocumentation.org/packages/units/versions/0.8-0/topics/units
#lets do some converstion from metric to imperial
units(data_frame$WVHT) <- make_units(ft)

print(data_frame)



#TODO: Analyze trends in dominate wave peroids & wave heights
#TODO: Marry this data with over sources ex. historical weather reports like was it raining that day?
# sorting the data by the column
data_sorted <- x[order(x$WVHT, decreasing = TRUE), ]

# select top 3 values from each group
data_mod <- Reduce(rbind,                               
                   by(data_sorted,
                      data_sorted["DPD"],
                      head,
                      n = 3))

#https://r-graphics.org/recipe-dataprep-calculate#:~:text=With%20base%20R%2C%20calculating%20a%20new%20colum%20can,heightweight%24heightCm%20%3C-%20heightweight%24heightIn%20%2A%202.54%2015.15.4%20See%20Also
# print(data_mod %>%
#         mutate(
#           wave_height_feet =  conv_unit(data_mod["WVHT"], "m", "ft"),
#           water_temp =  conv_unit(data_mod["WTMP"], "C", "F"),
#           air_temp =  conv_unit(data_mod["ATMP"], "C", "F")
#         )
# )

#print ("Modified DataFrame")
#print (data_mod)

write.csv(data_frame, "dominateWavePeroid.csv", row.names=TRUE)
