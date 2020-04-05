pull_raw <- function(save = FALSE){
`%>%` <- magrittr::`%>%`

# Confirmed cases
us_conf_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
raw_us_conf <- read.csv(file = us_conf_url,
                        stringsAsFactors = FALSE)


# Checking if there is a missing data
lapply(1:ncol(raw_us_conf), function(i){
  if(all(is.na(raw_us_conf[, i]))){
    raw_us_conf <<- raw_us_conf[, -i]
    return(print(paste("Column", names(raw_us_conf)[i], "is missing", sep = " ")))
  } else {
    return(NULL)
  }
})


df_us_conf <- raw_us_conf[, 1:11]
head(df_us_conf)
head(df_us_conf)

for(i in 12:ncol(raw_us_conf)){

  raw_us_conf[,i] <- as.integer(raw_us_conf[,i])
  # raw_us_conf[,i] <- ifelse(is.na(raw_us_conf[, i]), 0 , raw_us_conf[, i])
  print(names(raw_us_conf)[i])

  if(i == 12){
    df_us_conf[[names(raw_us_conf)[i]]] <- raw_us_conf[, i]
  } else {
    df_us_conf[[names(raw_us_conf)[i]]] <- raw_us_conf[, i] - raw_us_conf[, i - 1]
  }

}

df_us_conf1 <-  df_us_conf %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                                   names_to = "date_temp",
                                                   values_to = "cases_temp")

# Parsing the date
df_us_conf1$month <- sub("X", "",
                         strsplit(df_us_conf1$date_temp, split = "\\.") %>%
                           purrr::map_chr(~.x[1]) )

df_us_conf1$day <- strsplit(df_us_conf1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_us_conf1$date <- as.Date(paste("2020", df_us_conf1$month, df_us_conf1$day, sep = "-"))

# Aggregate the data to daily
df_us_conf2 <- df_us_conf1 %>%
  dplyr::group_by(date, Province_State, UID, iso2, iso3, FIPS, Admin2, Lat, Long_, Combined_Key) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "confirmed")

head(df_us_conf2)
tail(df_us_conf2)


# Death cases
us_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
raw_us_death <- read.csv(file = us_death_url,
                         stringsAsFactors = FALSE)


# Checking if there is a missing data
lapply(1:ncol(raw_us_death), function(i){
  if(all(is.na(raw_us_death[, i]))){
    raw_us_death <<- raw_us_death[, -i]
    return(print(paste("Column", names(raw_us_death)[i], "is missing", sep = " ")))
  } else {
    return(NULL)
  }
})


df_us_death <- raw_us_death[, 1:12]
head(df_us_death)
head(df_us_death)

for(i in 13:ncol(raw_us_death)){

  raw_us_death[,i] <- as.integer(raw_us_death[,i])
  # raw_us_death[,i] <- ifelse(is.na(raw_us_death[, i]), 0 , raw_us_death[, i])
  print(names(raw_us_death)[i])

  if(i == 13){
    df_us_death[[names(raw_us_death)[i]]] <- raw_us_death[, i]
  } else {
    df_us_death[[names(raw_us_death)[i]]] <- raw_us_death[, i] - raw_us_death[, i - 1]
  }

}

df_us_death1 <-  df_us_death %>% tidyr::pivot_longer(cols = dplyr::starts_with("X"),
                                                     names_to = "date_temp",
                                                     values_to = "cases_temp")

# Parsing the date
df_us_death1$month <- sub("X", "",
                          strsplit(df_us_death1$date_temp, split = "\\.") %>%
                            purrr::map_chr(~.x[1]) )

df_us_death1$day <- strsplit(df_us_death1$date_temp, split = "\\.") %>%
  purrr::map_chr(~.x[2])


df_us_death1$date <- as.Date(paste("2020", df_us_death1$month, df_us_death1$day, sep = "-"))

# Aggregate the data to daily
df_us_death2 <- df_us_death1 %>%
  dplyr::group_by(date, Province_State, UID, iso2, iso3, FIPS, Admin2, Lat, Long_, Combined_Key) %>%
  dplyr::summarise(cases = sum(cases_temp)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(type = "death")

head(df_us_death2)
tail(df_us_death2)

covid19county <- dplyr::bind_rows(df_us_death2, df_us_conf2)
write.csv(covid19county, "csv/covid19county.csv", row.names = FALSE)

system(command = "git add *; git commit -m 'data refresh'; git push origin master")

}
