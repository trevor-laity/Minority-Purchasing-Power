### Following script pulls txt data from the BLS regarding 
### expenditure based on housing tenure (own/rent) & race
### input   : BLS fixed-width text file (via url)
### output  : dfs of cleaned, selected data with named rows and columns
###           - number of dfs produced changes with loop length
###
### FUNCTION: complete_pull_data(year, widths, header, footer)
###     parameters:
###     - year = year in question
###     - widths = vector of column widths (characters per column)
###     - header = how many lines FROM TOP until data starts
###     - footer = how many lines FROM TOP until data ends
### NOTE: function does not & store df in environment under df_name
###     fix:  function now returns dataframe; when calling function, use helper
###           pull_data_1994_2002(year)[1] to get df name, and assign yourself
###
### breakdown
### 1. get data for current df      [f = pull_data(year)]
###     a. make df name
###     b. get file url
### 2. make df                      [f= make_df(source_name, header_length,
###     a. read file url, store as df           widths_vec)]
### 3. clean df                     [f = clean_df(helper_df,
###     a. remove bottom rows                     footer_length)]
###     b. clean first column (remove ...)
###     c. convert first column to rownames, remove
###     d. select rows by predetermined relevance
###     d. recode digits: remove $,(); add -; coerce
### 4. assign data to df            [f = assign_df(df_name, df)]

library(stringr)

### PULLING DATA: 
#dimensions for txt files for years 1994-2002
vec_1994 <- list(widths = c(42, 9, 9, 9, 9, 9, 9, 10, 9, 9),
                 header = 8,
                 footer = 168)
vec_1995 <- list(widths = c(42, 9, 9, 9, 9, 9, 9, 10, 9, 9),
                 header = 8,
                 footer = 168)
vec_1996 <- list(widths = c(60, 16, 15, 15, 15, 15, 15, 15, 15, 14),
                 header = 11,
                 footer = 169)
vec_1997 <- list(widths = c(36, 10, 10, 10, 10, 10, 10, 10, 10, 9),
                 header = 13,
                 footer = 195)
vec_1998 <- list(widths = c(45, 10, 10, 10, 10, 10, 10, 10, 10, 10),
                 header = 11, 
                 footer = 183)
vec_1999 <- list(widths = c(46,9,10,10,10,10,10,10,10,10),
                 header = 11,
                 footer = 184)
vec_2000 <- list(widths = c(46,9,10,10,10,10,10,10,10,10),
                 header = 12,
                 footer = 187)
vec_2001 <- list(widths = c(46,9,10,10,10,10,10,10,10,10),
                 header = 12,
                 footer = 187)
vec_2002 <- list(widths = c(46,9,10,10,10,10,10,10,10,10),
                 header = 12,
                 footer = 187)
vec_2003 <- list(widths = c(45,10,10,10,10,10),
                 header = 13,
                 footer = 187)
vec_2004 <- list(year = 2004,
                 widths = c(50,11,12,12,12,12),
                 header = 16,
                 footer = 191)

####DOWNLOAD & CLEAN ALL DATA:
for(y in 1994:2004){
  
  name <- get(paste0("vec_", y, sep = ""))
  df <- complete_pull_data(year = y,
                           widths = name$widths,
                           header = name$header,
                           footer = name$footer)
  helper <- paste0("race_df_", y, sep = "")
  assign(helper, df)
  #save cleaned data in data directory
  save(df, file = paste0("data/", helper, "_cleaned.Rdata", sep = ""))
}