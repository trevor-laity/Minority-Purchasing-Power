#read data 
##downloaded from US Census
download.file(url = "http://www.census.gov/housing/hvs/data/histtab16.xlsx",
              destfile = "rawdata/homeownership_rates.xlsx")
#coerce .xlsx into .csv
#read csv
home_rates <- read.csv("rawdata/homeownership_rates_csv.csv", header=FALSE)

#re-name columns 
library("stringr")
library(plyr)
names(home_rates)[names(home_rates)=="V1"] <- "Year and Quarter"
names(home_rates)[names(home_rates)=="V2"] <- "US Total"
names(home_rates)[names(home_rates)=="V3"] <- "White"
names(home_rates)[names(home_rates)=="V4"] <- "Black"
names(home_rates)[names(home_rates)=="V5"] <- "Other"
names(home_rates)[names(home_rates)=="V6"] <- "Hispanic"


#remove unnecessary columns after "Hispanic" with "NA"s
home_rates$V7 <- NULL
home_rates$V8 <- NULL
home_rates$V9 <- NULL
home_rates$V10 <- NULL
home_rates$V11 <- NULL
home_rates$V12 <- NULL


#remove unnecessary rows in header and footer along 
# with data for 2015 due to incomplete data
#(missing data for fourth quarter)
home_rates <- home_rates[-c(134:155),]


home_rates <- home_rates[-c(1:8),]


#remove empty rows in between data for each year
home_rates <- home_rates[!apply(is.na(home_rates) | 
                                  home_rates == "", 1, all), ]



#make new column for year to have separate columns for year and quarter
home_rates$Year <- rep(1994:2014, each = 5)

#remove 'nth' rows that contain the years, under "Year and Quarter" 
index <- seq(1, nrow(home_rates), by=5)
home_rates <- home_rates[-index,]


#remove "Year and Quarter" column and make "Quarter" column
home_rates$Quarter <- str_extract(home_rates$`Year and Quarter`, 
                                  pattern = "^[1-4]")
home_rates$`Year and Quarter` <- NULL            


#re-order columns to move "Year" and "Quarter" and "
#Other" columns to conclude cleaning
home_rates <- home_rates[,c("Year", "Quarter", "US Total", "White", 
                            "Black", "Hispanic", "Other")]
save(home_rates, file = "data/home_rates.RData")

#create average of homeownership rates of all 
#quarters to obtain 1 value for each year for all
#population groups

average_black_rate <- calc_black_rate()

#average White homeownership rate by year
average_white_rate <- calc_white_rate()

#average Hispanic homeownership rate by year
average_hispanic_rate <- calc_hispanic_rate()

#average Other homeownership rate by year
average_other_rate <- calc_other_rate()

#average total US homeownership rate by year
average_US_rate <- calc_US_rate()

#compare minority group averages with US Total average 
pdf("plots/rates_comparison.pdf")
png("plots/rates_comparison.png")
plot.new()
plot(unique(home_rates$Year), average_US_rate, xlab = "Year", 
     main = "Average Homeownership Rates: Minority Groups vs. US Total", 
     ylab = "Homeownership Rates", xaxt = 'n', lwd = 3, 
     yaxt = 'n', type = "l", col = "#FF4040", 
     axes = FALSE, ylim= c(40,80), xlim = c(1994, 2014))
lines(unique(home_rates$Year), average_hispanic_rate, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#7FFFD4")
lines(unique(home_rates$Year), average_black_rate, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#FF7F50")
axis(side = 2, at = c(seq(from = 40, to = 80, by = 5)), las = 1)
axis(side = 1, at = c(seq(from = 1994, to = 2014, by = 2)), las = 2)
abline(h = c(seq(from=40,to=80,by=5)), 
       v = c(seq(from = 1994, to = 2014, by = 2)))
legend("topright", title = "Homeownership Rates", 
       col = c("#FF4040", "#7FFFD4", "#FF7F50"), 
       legend = c("Total US", "Hispanic", "Black"), 
       lwd = c(2.5, 2.5), lty = c(1,1), cex = 0.7)
dev.off()

#create plot demonstrating change in differences in 
#homeownership rates for hispanic vs US total
diff_hisp <- c()
for (i in 1994:2014) {
  result = calc_diff(average_US_rate, average_hispanic_rate)
}
diff_hisp <- c(diff_hisp, result)
pdf("plots/differences_hispanic.pdf")
png("plots/differences_hispanic.png")

plot.new()
plot(unique(home_rates$Year), diff_hisp, 
     main = "Difference in Avg US and Hispanic Homeownership Rates", 
     ylim = c(10,30), type = 'l', lwd = 4, col = "#8B0000",
     xlab = "Year", ylab = "Homeownership Rates")
abline(h = c(seq(from=10,to=30,by=5)), 
       v = c(seq(from = 1994, to = 2014, by = 2)))
dev.off()

#create vector of differences in homeownership rates for US total vs Black
diff_black <- c()
for (i in 1994:2014) {
  result = calc_diff(average_US_rate, average_black_rate)
}
diff_black <- c(diff_black, result)

#create plot demonstrating change in differences in homeownership rates 
#for black vs US total
pdf("plots/differences_black.pdf")
png("plots/differences_black.png")
plot.new()
plot(unique(home_rates$Year), diff_black, 
     main = "Difference in Avg US and Black Homeownership Rates", 
     ylim = c(10,30), type = 'l', lwd = 4, col = "#0000EE",
     xlab = "Year", ylab = "Homeownership Rates")
abline(h = c(seq(from=10,to=30,by=5)), 
       v = c(seq(from = 1994, to = 2014, by = 2)))
dev.off()

#create vector of differences in homeownership rates for US total vs other
diff_other <- c()
for (i in 1994:2014) {
  result = calc_diff(average_US_rate, average_other_rate)
}
diff_other <- c(diff_other, result)

#create plot demonstrating change in differences in 
#homeownership rates for other vs US total
pdf("plots/differences_other.pdf")
png("plots/differences_other.png")
plot.new()
plot(unique(home_rates$Year), diff_other, 
     main = "Difference in Avg US and 'Other' Homeownership Rates", 
     ylim = c(0,30), type = 'l', lwd = 4, col = "#76EEC6",
     xlab = "Year", ylab = "Homeownership Rates", las = 1)
abline(h = c(seq(from=0,to=30,by=5)), 
       v = c(seq(from = 1994, to = 2014, by = 2)))
dev.off()

