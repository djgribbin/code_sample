#Thank you Robert Sams for providing the body of this function
# http://r.789695.n4.nabble.com/IMM-Dates-td926077.html
dcIMMDate = function(year_int, month_abbr, week_int, day_str){ 
  months = 1:12
  names(months) = c("Jan", "Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  month_int = months[[month_abbr]]
  days = 1:7
  names(days) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  day_int = days[[day_str]]
  
  
  dcTuple = function(date){ 
    lt = as.POSIXlt(date) 
    year = lt$year + 1900 
    month = lt$mon + 1 
    mday = lt$mday 
    wday = lt$wday 
    if(length(date)!=1){  
      return(matrix(c(year,month,mday,wday),ncol=4)) 
    } 
    else{ 
      return(c(year,month,mday,wday)) 
    } 
  } 
  dcDatePart = function(date, part){ 
    parts = c("year", "month", "mday", "wday") 
    if(!class(date)[1] %in% c("Date", "chron", "POSIXt")){ 
      return(date) 
    } 
    if(is.character(part)){ 
      part = which(parts == part) 
    } 
    if(length(date) == 1){ 
      return(dcTuple(date)[part]) 
    } 
    else{ 
      return(dcTuple(date)[, part]) 
    } 
  } 
  dcNthDayOfWeek = function(first_day_of_month, dow, n){ 
    m = dcDatePart(first_day_of_month, "month") 
    y = dcDatePart(first_day_of_month, "year") 
    dow1 = dcDatePart(as.Date(ISOdate(y, m, 1)), "wday") 
    return((n -  1) * 7 + (dow - dow1) %% 7 + first_day_of_month) 
  } 
  first_day_of_month = as.Date(ISOdate(year_int, month_int, 1)) 
  return(dcNthDayOfWeek(first_day_of_month, n = week_int, dow = day_int))
} 


# Convert from a text to a boolean column
convertFraudColumn = function(fraud){
  if(fraud == "Yes"){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

# Returns date one year prior
oneYearBack = function(date){
  d = as.POSIXlt(date)
  d$year = d$year -1
  return(as.Date(d))
}

# Calculate feature: Number of fraud cases in the past year
fraudCountPastYear = function(col_name, group, lookup_table, datetime){
  group = toString(group)
  return(sum(lookup_table[[col_name]][[group]][DateTime >= oneYearBack(datetime) & DateTime <= datetime]$FraudFound))
}

# Calculate feature: Number of legitimate cases in the past year
legitimateCountPastYear = function(col_name, group, lookup_table, datetime){
  group = toString(group)
  return(sum(!lookup_table[[col_name]][[group]][DateTime >= oneYearBack(datetime) & DateTime <= datetime]$FraudFound))
}

# Calculate feature: Number of months that have a fraud case in the past year
fraudMonthsPastYear = function(col_name, group, lookup_table, datetime){
  group = toString(group)
  times = lookup_table[[col_name]][[group]][DateTime >= oneYearBack(datetime) & DateTime <= datetime & FraudFound == TRUE]$DateTime
  if(length(times) != 0) {
    return(length(unique(lapply(times, function(x) months(as.POSIXlt(x))))))
  }
  else{
    return(as.vector(c(0), mode = "integer"))
  }
}

# Calculate feature: Number of legitimate months in the past year
legitimateMonthsPastYear = function(col_name, group, lookup_table, datetime){
  group = toString(group)
  times = lookup_table[[col_name]][[group]][DateTime >= oneYearBack(datetime) & DateTime <= datetime & FraudFound == FALSE]$DateTime
  if(length(times) != 0) {
    return(length(unique(lapply(times, function(x) months(as.POSIXlt(x))))))
  }
  else{
    return(as.vector(c(0), mode = "integer"))
  }
}

# Compute features
calculateFeature = function(feature_df, subset_df, col_name, lookup_table, features){
  print(col_name)
  for(feature in features){

    if(feature == "FraudCount"){
      feature_column = subset_df %>% rowwise() %>% mutate_(Feature = interp(~fraudCountPastYear(c, cn, lt, dt), c = col_name, cn = as.name(col_name), lt = lookup_table, dt = as.name("DateTime")))
      feature_df[paste(name, feature, sep = "")] = feature_column$Feature
    }
    
    if(feature == "FraudMonths"){
      feature_column = subset_df %>% rowwise() %>% mutate_(Feature = interp(~fraudMonthsPastYear(c, cn, lt, dt), c = col_name, cn = as.name(col_name), lt = lookup_table, dt = as.name("DateTime")))
      feature_df[paste(name, feature, sep = "")] = feature_column$Feature
    }
    
    if(feature == "LegitimateCount"){
      feature_column = subset_df %>% rowwise() %>% mutate_(Feature = interp(~legitimateCountPastYear(c, cn, lt, dt), c = col_name, cn = as.name(col_name), lt = lookup_table, dt = as.name("DateTime")))
      feature_df[paste(name, feature, sep = "")] = feature_column$Feature
    }
    
    if(feature == "LegitimateMonths"){
      feature_column = subset_df %>% rowwise() %>% mutate_(Feature = interp(~legitimateMonthsPastYear(c, cn, lt, dt), c = col_name, cn = as.name(col_name), lt = lookup_table, dt = as.name("DateTime")))
      feature_df[paste(name, feature, sep = "")] = feature_column$Feature
    }
    
    if(feature == "FraudRate"){
      feature_df[paste(name, feature, sep = "")] = feature_df[paste(name, "FraudCount", sep = "")] / (feature_df[paste(name, "FraudCount", sep = "")] + feature_df[paste(name, "LegitimateCount", sep = "")])
    }
    
  }
  return(feature_df)
}


# Unit Testing
dcIMMDate(2014, "Jun", 4, "Thursday") == "2014-06-26"
dcIMMDate(2013, "Jun", 4, "Thursday") == "2013-06-27"
