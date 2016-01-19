##Race Functions
#need str_trim() for removing whitespace
library(stringr)

### 1. get data for current df
pull_data <- function(year){
  df_name <- paste0("tenure_race_", year, sep = "")
  #file name/url changes w year -- account for switch in year 2003
  if (year < 2003){
    source_name <- paste0("http://www.bls.gov/cex/standard/",
                          year,
                          "/tenracar.txt", sep = "")
  }
  else{
    source_name <- paste0("http://www.bls.gov/cex/standard/",
                          year,
                          "/race.txt", sep = "")
  }
  c(df_name, source_name)
}

### 2. make df
make_df <- function(source_name,header_length, widths_vec){
  helper_df <- read.fwf(source_name,
                        #skip = how many lines to skip before reading table
                        skip = header_length,
                        #widths = vectore of character lengths of column widths
                        widths = widths_vec,
                        #we add our own later
                        row.names = NULL)
  helper_df
}

###3. clean df
clean_df <- function(helper_df, footer_start){
  #remove footnotes in bottom rows
  helper_df <- helper_df[-c(footer_start:nrow(helper_df)),]
  
  #remove "..." in first column
  helper_df[,1] <- gsub("\\.", "", helper_df[,1])
  
  #set row names as first column
  rownames(helper_df) <- make.names(helper_df[,1], unique = T)
  
  #remove first column
  helper_df <- helper_df[,-1]
  
  #select indices of relevant rows
  rows_wanted <- c("Income After Taxes", "Average Annual Expenditures", 
                   "Food", "Alcoholic Beverages", "Housing$", 
                   "Apparel and Services", "Transportation", "Health care", 
                   "Entertainment", "Personal Care Products and", 
                   "Reading", "Education$", "Tobacco Products and", 
                   "Miscellaneous", "Cash Contributions", 
                   "Personal Insurance and Pensions")
  #format used in all data frames
  rows_wanted <- gsub(" ", ".", rows_wanted)
  index_count <- c()
  for(i in 1:length(rows_wanted)){
    if (i == 4)
    {
      number <- 2
    }
    else
      number <- 1
    index_count <- c(index_count, 
                     grep(rows_wanted[i], row.names(helper_df), 
                          ignore.case = T)[number])
  }
  helper_df <- helper_df[index_count,]
  #recode (digits) to -digits
  #traverse columns
  for(i in 1:ncol(helper_df)){
    #remove whitespace
    helper_df[,i] <- str_trim(helper_df[,i])
    #remove commas & dollar signs (and pipes where needed)
    helper_df[,i] <- gsub("[\\|,\\$]", "", helper_df[,i])
    #find indices of columns with parentheses values
    parentheses_index <- grep("[\\(\\)]", helper_df[,i])
    if (length(parentheses_index > 0)){
      for(j in 1:length(parentheses_index)){
        #remove ()
        helper_df[parentheses_index[j],i] <- gsub("[\\(\\)]", "", 
                                                  helper_df[parentheses_index[j],
                                                            i])
        #add -
        helper_df[parentheses_index[j],i] <- paste0("-", 
                                                    helper_df[parentheses_index[j],
                                                              i],
                                                    sep = "")
      }
    }
    #coerce string as numeric and overwrite
    helper_df[,i] <- as.numeric(helper_df[,i])
  }
  #rename in cleaner fashion
  rownames(helper_df) <- c("Income After Taxes", 
                           "Average Annual Expenditures", 
                           "Food", "Alcoholic Beverages", 
                           "Housing", "Apparel & Services", 
                           "Transportation", "Healthcare", 
                           "Entertainment", 
                           "Personal Care Products & Services", 
                           "Reading", "Education", 
                           "Tobacco Products & Smoking Supplies", 
                           "Miscellaneous Foods", "Cash Contributions", 
                           "Personal Insurance and Pensions")
  helper_df
}

### 4. assign -- need to do outside function
assign_df <- function(df_name, df){
  #assign character string to df as obj name
  assign(df_name, df)
}

##MASTER FUNCTION
complete_pull_data <- function(year, widths, header, footer){
  df_name <- pull_data(year)[1]
  source_name <- pull_data(year)[2]
  #download rawt txt to rawdata directory
  download.file(destfile = paste0("rawdata/", df_name, "text.txt", sep = ""),
                url = source_name)
  ?download.file
  #make df
  df <- make_df(source_name = source_name,
                header_length = header,
                widths_vec = widths)
  #save RAW df in rawdata directory
  save(df, file = paste0("rawdata/", df_name, "raw.Rdata", sep = ""))
  
  #clean df, return all numbers
  df_cleaned <- clean_df(helper_df = df,
                         footer_start = footer)
  
  #remove unnecessary categories: only need 
  #white, black, hispanic, nonhispanic
  if(year < 2003){
    df_cleaned <- df_cleaned[-c(1:5)]
    names(df_cleaned) <- c("White", 
                           "Black", 
                           "Hispanic", 
                           "NonHispanic")
  }
  else{
    df_cleaned <- df_cleaned[-1]
    names(df_cleaned) <- c("Total",
                           "White",
                           "Asian",
                           "Black")
  }
  
  #NA removal
  #traverse columns
  for(x in ncol(df_cleaned)){
    #then go by row
    for(y in nrow(df_cleaned)){
      if (is.na(df_cleaned[y,x]))
        df_cleaned[y,x] <- 0
    }
  }
  #assign name to cleaned object
  assign(df_name, df_cleaned)
  #return object
  get(df_name)
  
}

#transpose dataframes
#go through dataframes by year, then by race (nested)
#pull data, put in new dataframe
#abstract to function: specify datatype by index of row wanted
get_info <- function(row_index, new_df){
  year_count <- 1
  #go by year
  for(y in 1994:2002){
    #year = by dataframe in old sort
    #get df for specified year
    currYear <- get(paste0("race_df_", y, sep = ""))
    #year = by row in new sort; use year count as row count in new sort
    #traverse by columns = races in both 
    for(z in 1:length(races)){
      new_df[year_count, z] <- currYear[row_index, z]
    }
    year_count <- year_count + 1
  }
  rownames(new_df) <- c(1994:2002)
  colnames(new_df) <- races
  new_df
}
