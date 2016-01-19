###LINE GRAPHS - INCOMES
#load relevant df: 1994:2002
for(i in 1994:2002){
  prefix <- "race_df_"
  file_name <- paste0("data/",prefix, i, "_cleaned.Rdata", sep = "")
  df_name <- paste0(prefix, i, sep = "")
  load(file_name)
  assign(df_name, df)
}

####Sort data by individual item: rows = years, columns = races
races <- c("White", "Black", "Hispanic", "NonHispanic")
row_indices <- c(Income = 1, Expenditure = 2, 
                 Food = 3, Alcohol = 4, Apparel = 5,
                 Housing = 6, Transportation = 7, Health = 8, 
                 Entertainment = 9, Reading = 11, Miscellaneous = 14,
                 PersonalInsurance = 16)

#make dfs, then split by race

for(i in 1:length(row_indices)){
  df <- as.data.frame(matrix(data = 0, nrow = 9, ncol = 4))
  df <- get_info(row_index = row_indices[i], df)
  #make individual vectors: spending data for years by race
  for(j in 1:length(races)){
    vec <- df[j]
    #make & assign name to vec
    name <- paste0(races[j], "_", names(row_indices[i]), sep = "")
    assign(name, vec)
  }
}
#now vectors of spending each year, by race: "White_Food", 
#                                            "Black_Entertainment", 
#                                            "Hispanic_Health", etc.
pdf("plots/income_after_taxes.pdf")
png("plots/income_after_taxes.png")
plot.new()
income_years <- c(1994:2002)
plot(income_years, t(White_Income), xlab = "Year", main = "Income After Taxes", 
     ylab = "Income ($)", xaxt = 'n', lwd = 3, yaxt = 'n', 
     type = "l", col = "#FF7256", axes = FALSE, ylim= c(15000,60000), 
     xlim = c(1994, 2002))
lines(income_years, t(Black_Income), xaxt = 'n', yaxt = 'n', 
      lwd = 3, col = "#76EEC6")
lines(income_years, t(Hispanic_Income), xaxt = 'n', yaxt = 'n', 
      lwd = 3, col = "#0000EE")
lines(income_years, t(`NonHispanic_Income`), xaxt = 'n', yaxt = 'n', 
      lwd = 3, col = "#BCEE68")
axis(side = 2, at = c(seq(from = 15000, to = 60000, by = 5000)), las = 1)
axis(side = 1, at = c(seq(from = 1994, to = 2002, by = 2)), las = 1,
     cex.axis = .75)
abline(h = c(seq(from=15000,to=60000,by=5000)), 
       v = c(seq(from = 1994, to = 2002, by = 2)))
legend("topleft", title = "Subgroups", 
       col = c("#FF7256", "#BCEE68", "#0000EE", "#76EEC6"), 
       legend = c("White", "Non-Hispanic", "Hispanic", "Black"), 
       lwd = c(2.5, 2.5), lty = c(1,1), cex = 0.7)
dev.off()

#plot expenditures together:
pdf("plots/expenditures.pdf")
png("plots/expenditures.png")
plot.new()
years_expenditure <- c(1994:2002)
plot(years_expenditure, t(White_Expenditure), 
     xlab = "Year", main = "Average Annual Expenditure", 
     ylab = "Expenditure ($)", xaxt = 'n', lwd = 3, 
     yaxt = 'n', type = "l", col = "#FF7256", axes = FALSE, 
     ylim= c(15000,60000), xlim = c(1994, 2002))
lines(years_expenditure, t(Black_Expenditure), 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#76EEC6")
lines(years_expenditure, t(Hispanic_Expenditure), 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#0000EE")
lines(years_expenditure, t(`NonHispanic_Expenditure`), 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#BCEE68")
axis(side = 2, at = c(seq(from = 15000, to = 60000, by = 5000)), las = 1)
axis(side = 1, at = c(seq(from = 1994, to = 2002, by = 2)), las = 1,
     cex.axis = .75)
legend("topleft", title = "Subgroups", 
       col = c("#FF7256", "#BCEE68", "#0000EE", "#76EEC6"), 
       legend = c("White", "Non-Hispanic", "Hispanic", "Black"), 
       lwd = c(2.5, 2.5), lty = c(1,1), cex = 0.7)


###########################
###########################
###EXPENDITURE PIE CHARTS
# sum of food, apparel, transportation, health care, entertainment, 
# personal care products for 1994 (all races)
pdf("plots/1994piechart_black.pdf")
png("plots/1994piechart_black.png")
food_black_1994 <- race_df_1994[3,2]
apparel_black_1994 <- race_df_1994[6,2]
transportation_black_1994 <- race_df_1994[7,2]
healthcare_black_1994 <- race_df_1994[8,2]
entertainment_black_1994 <- race_df_1994[9,2]
personalcare_black_1994 <- race_df_1994[10,2]

slices_black_1994 <- c(food_black_1994, apparel_black_1994,
                       transportation_black_1994, healthcare_black_1994, 
                       entertainment_black_1994, personalcare_black_1994)
labels_black_1994 <- c("Food","Apparel","Transportation","Healthcare",
                       "Entertainment","Personal Care")
pct_black_1994 <- round(slices_black_1994/sum(slices_black_1994)*100)

#add percents to labels
labels_black_1994 <- paste(labels_black_1994, pct_black_1994)  
labels_black_1994 <- paste(labels_black_1994,"%",sep="")  
pie_black_1994 <- pie(slices_black_1994, labels = labels_black_1994, 
                      main = "Black - Categorical Spending for 1994")

dev.off()

pdf("plots/1994piechart_hispanic.pdf")
png("plots/1994piechart_hispanic.png")
slices_hispanic_1994 <- c(race_df_1994$Hispanic[3],race_df_1995$Hispanic[6],
                          race_df_1995$Hispanic[7],race_df_1995$Hispanic[8],
                          race_df_1995$Hispanic[9],race_df_1995$Hispanic[10])
labels_hispanic_1994 <- c("Food","Apparel","Transportation",
                          "Healthcare","Entertainment","Personal Care")
pct_hispanic_1994 <- round(slices_hispanic_1994/sum(slices_hispanic_1994)*100)
labels_hispanic_1994 <- paste(labels_hispanic_1994, pct_hispanic_1994)
labels_hispanic_1994 <- paste(labels_hispanic_1994, "%", sep = "")
pie_hispanic_1994 <- pie(slices_hispanic_1994, labels = labels_hispanic_1994, 
                         main = "Hispanic - Categorical Spending for 1994")
dev.off()

#pie charts for black and hispanic categorical spending for 2002
pdf("plots/2002piechart_black.pdf")
png("plots/2002piechart_black.png")
slices_black_2002 <- c(race_df_2002$Black[3],race_df_2002$Black[6],
                       race_df_2002$Black[7],race_df_2002$Black[8],
                       race_df_2002$Black[9],race_df_2002$Black[10])
labels_black_2002 <- c("Food","Apparel","Transportation","Healthcare",
                       "Entertainment","Personal Care")
pct_black_2002 <- round(slices_black_2002/sum(slices_black_2002)*100)
labels_black_2002 <- paste(labels_black_2002, pct_black_2002)
labels_black_2002 <- paste(labels_black_2002, "%", sep = "")
pie_black_2002 <- pie(slices_black_2002, labels = labels_black_2002, 
                      main = "Black - Categorical Spending for 2002")
dev.off()

pdf("plots/2002piechart_hispanic.pdf")
png("plots/2002piechart_hispanic.png")
slices_hispanic_2002 <- c(race_df_2002$Hispanic[3], race_df_2002$Hispanic[6], 
                          race_df_2002$Hispanic[7], race_df_2002$Hispanic[8], 
                          race_df_2002$Hispanic[9], race_df_2002$Hispanic[10])
labels_hispanic_2002 <- c("Food","Apparel","Transportation","Healthcare",
                          "Entertainment","Personal Care")
pct_hispanic_2002 <- round(slices_hispanic_2002/sum(slices_hispanic_2002)*100)
labels_hispanic_2002 <- paste(labels_hispanic_2002, pct_hispanic_2002)
labels_hispanic_2002 <- paste(labels_hispanic_2002, "%", sep = "")
pie_hispanic_2002 <- pie(slices_hispanic_2002, labels = labels_hispanic_2002, 
                         main = "Hispanic - Categorical Spending for 2002")
dev.off()

#create vectors of essential spending 
#(Food, Transportation, Apparel, Health Care, Personal Insurance) by race
total_essential_white <- White_Food + White_Transportation + 
  White_Apparel + White_Health + 
  White_PersonalInsurance
total_essential_black <- Black_Food + Black_Transportation + 
  Black_Apparel + Black_Health + 
  Black_PersonalInsurance
total_essential_hispanic <- Hispanic_Food + Hispanic_Transportation + 
  Hispanic_Apparel + Hispanic_Health + 
  Hispanic_PersonalInsurance
total_essential_nonhispanic <- NonHispanic_Food + NonHispanic_Transportation +
  NonHispanic_Apparel + NonHispanic_Health + 
  NonHispanic_PersonalInsurance

#create vectors of non essential spending 
#(Alcoholic Beverages, Reading, Entertainment, Miscellaneous Foods) by race
total_non_essential_white <- White_Alcohol + White_Reading + 
  White_Entertainment + White_Miscellaneous
total_non_essential_black <- Black_Alcohol + Black_Reading + 
  Black_Entertainment + Black_Miscellaneous
total_non_essential_hispanic <- Hispanic_Alcohol + Hispanic_Reading + 
  Hispanic_Entertainment + Hispanic_Miscellaneous
total_non_essential_nonhispanic <- NonHispanic_Alcohol + NonHispanic_Reading + 
  NonHispanic_Entertainment + 
  NonHispanic_Miscellaneous

#plot essential graph for all groups 
pdf("plots/essential_expenditure.pdf")
png("plots/essential_expenditure.png")
plot.new()
years_expenditure <- c(1994:2002)
plot(years_expenditure, total_essential_white$White, 
     xlab = "Year", main = "Essential Goods", 
     ylab = "Expenditure ($)", xaxt = 'n', lwd = 3, yaxt = 'n', 
     type = "l", col = "#FF7256", axes = FALSE, ylim= c(10000,25000), 
     xlim = c(1994, 2002))
lines(years_expenditure, total_essential_black$Black, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#76EEC6")
total_essential_black
lines(years_expenditure, total_essential_hispanic$Hispanic, xaxt = 'n', 
      yaxt = 'n', lwd = 3, col = "#0000EE")
lines(years_expenditure, total_essential_nonhispanic$NonHispanic, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#BCEE68")
axis(side = 2, at = c(seq(from = 10000, to = 25000, by = 2500)), 
     las = 1, cex.axis = 0.75)
axis(side = 1, at = c(seq(from = 1994, to = 2002, by = 2)), las = 1)
abline(h = c(seq(from=10000,to=25000,by=2500)), 
       v = c(seq(from = 1994, to = 2002, by = 2)))
legend("topleft", title = "Subgroups", 
       col = c("#FF7256", "#BCEE68", "#0000EE", "#76EEC6"), 
       legend = c("White", "Non-Hispanic", "Hispanic", "Black"), 
       lwd = c(2.5, 2.5), lty = c(1,1), cex = 0.7)
dev.off()

#plot nonessential graph for all groups
pdf("plots/nonessential_expenditure.pdf")
png("plots/nonessential_expenditure.png")
plot.new()
years_expenditure <- c(1994:2002)
plot(years_expenditure, total_non_essential_white$White, 
     xlab = "Year", main = "Nonessential Goods", 
     ylab = "Expenditure ($)", xaxt = 'n', lwd = 3, yaxt = 'n', 
     type = "l", col = "#FF7256", axes = FALSE,
     ylim= c(1000,5000), xlim = c(1994, 2002))
lines(years_expenditure, total_non_essential_black$Black, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#76EEC6")
lines(years_expenditure, total_non_essential_hispanic$Hispanic, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#0000EE")
lines(years_expenditure, total_non_essential_nonhispanic$NonHispanic, 
      xaxt = 'n', yaxt = 'n', lwd = 3, col = "#BCEE68")
axis(side = 2, at = c(seq(from = 1000, to = 5000, by = 500)), 
     las = 1, cex.axis = 0.75)
axis(side = 1, at = c(seq(from = 1994, to = 2002, by = 2)), 
     las = 1)
abline(h = c(seq(from=0,to=5000,by=500)), 
       v = c(seq(from = 1994, to = 2002, by = 2)))
legend("topleft", title = "Subgroups", 
       col = c("#FF7256", "#BCEE68", "#0000EE", "#76EEC6"), 
       legend = c("White", "Non-Hispanic", "Hispanic", "Black"), 
       lwd = c(2.5, 2.5), lty = c(1,1), cex = 0.7)
dev.off()
