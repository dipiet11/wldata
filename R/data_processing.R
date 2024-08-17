##### Exploring Missing Data #####

if (!exists("combined_data")) {
  warning("Run the 'web_scraping_and_variable_matching.R' script before proceeding")
}

# Rearrange the variables in the data so that similar sounding variables are closer to
# one another for comparison.
names(combined_data)
combined_data <- combined_data[,c(14,15,13,1,2,20,3,4,5,6,7,8,9,10,11,12,19,18,16,17,21)]

# Run the missing_values() and missing_rows() functions
missing_values(combined_data)
# Looks like the competitions that are missing nat have nation, and the same pattern for name and surname variables

# Run the missing_values() and missing_rows() functions
missing_values(combined_data)
# missing_rows(combined_data)

# Since there are two name variables, lets see if we can match 
# names to get one name variable
name1_list <- levels(factor(combined_data$`Surname and name`))
name2_list <- levels(factor(combined_data$`Name and surname`))

# Since there are only 1254 names in the 2nd list compared to 21747 in the first,
# its easier to work with name2_list

# Reversing the names here of the string if there are two names. If there are more than
# two names in a name, then "MATCH" is added on to the end of these peoples name.
name2_list_reverse <- lapply(name2_list, function(x) {
  space_count <- length(str_split(x, " ")[[1]])
  if (space_count == 2) {
    return(paste0(str_split(x, " ")[[1]][2], " ", str_split(x, " ")[[1]][1]))
  } else {
    return(paste0(x,"MATCH"))
  }
})

### How many names have MATCH tagged on to the end?
count_this <- 0
for (i in 1:length(name2_list_reverse)) {
  if (grepl("MATCH", name2_list_reverse[[i]])) {
    count_this <- count_this + 1
  }
}
count_this
# 67 names have more than one first name or last name, thats not so many, and likely
# not to have much of an impact on the quality of the data. Maybe they can be removed.

# Ok lets add in names
# Need to get the positions in the main data frame where these names are
positions_in_data <- sapply(name2_list, function(x) {
  return(which(combined_data$`Name and surname` == x))
})

# Need to build a vector to store the name data as a new variable
Name2 <- 1:nrow(combined_data)
for (i in 1:length(positions_in_data)) {
  for (j in 1:length(positions_in_data[[i]])){
    Name2[[positions_in_data[[i]][[j]]]] <- names(positions_in_data[i])
  }
}

# Bind the new names into the dataset and remove the other name variables that were
# there from before. Also, removing the "MATCH" from the 67 individuals names.
df7 <- cbind(combined_data, Name2)
df7 <- df7 %>%
  mutate(Competitor_Name = .data$`Surname and name`) %>%
  mutate(Competitor_Name = if_else(is.na(.data$`Surname and name`), .data$Name2, .data$`Surname and name`)) %>%
  select(-c("Name and surname", "Surname and name", "Name2")) %>%
  mutate(Competitor_Name = if_else(grepl("MATCH", .data$Competitor_Name),
                                   sub("MATCH", "", .data$Competitor_Name),
                                   .data$Competitor_Name))

# Lets quickly check is all of nat levels are in nation so we can assume they are the same.
levels(factor(combined_data$nat)) %in% levels(factor(combined_data$Nation))

# Everything is TRUE, so we can make the assumption that Nation is the same as nat
df8 <- df7 %>%
  mutate(Nation = if_else(is.na(.data$Nation), .data$nat,
                          if_else(is.na(.data$nat), .data$Nation, as.character(NA))))

# There is a Gender called "Females and Males" with a lot of data,
# to solve this we need to look at their total and compare it to their 
# sinclair as males and females have a different sinclair coefficient 
# this will be done later as totals are unknown and need to be calculated.
summary(factor(df8$Gender))

# Check the range of dates
as.integer(diff(range(as.Date(df8$Date))))
range(as.Date(df8$Date))

# Earliest date is 2014-05-24, latest date is 2022-11-04. Anything after 2018-10-31 follows 
# new weightclasses. 
# Also max weight is 1305, and minimum is -120, which doesnt make sense. Let's see how many
# weight values are between 35 and 200
check_BW <- subset(df8, select = c("B.W", "Age_Category")) %>%
  mutate(B.W = as.numeric(.data$B.W)) %>%
  filter(.data$B.W <= 35 | .data$B.W >= 200)
summary(factor(check_BW$Age_Category))
# Only 181 values between these bodyweights, lets remove them as its unrealistic and the
# lowest youth weightclass is 40kg. Many of these are also very young, so lets remove them here.

# converting lift values to numeric and setting anything listed as '---' to 0 as this represents a skipped lift
# 
df8 <- df8 %>%
  mutate(weightclass_rule = ifelse(as.Date(.data$Date) >= as.Date("2018-10-31"), "New", "Old")) %>%
  mutate(B.W = as.numeric(.data$B.W)) %>%
  filter(.data$B.W >= 35 & .data$B.W <= 200) %>%
  mutate(Snatch_1 = if_else(.data$Snatch_1 == "---", as.numeric(0), as.numeric(.data$Snatch_1))) %>%
  mutate(Snatch_2 = if_else(.data$Snatch_2 == "---", as.numeric(0),as.numeric(.data$Snatch_2))) %>%
  mutate(Snatch_3 = if_else(.data$Snatch_3 == "---", as.numeric(0),as.numeric(.data$Snatch_3))) %>%
  mutate(`Cl&Jerk_1` = if_else(.data$`Cl&Jerk_1` == "---", as.numeric(0),as.numeric(.data$`Cl&Jerk_1`))) %>%
  mutate(`Cl&Jerk_2` = if_else(.data$`Cl&Jerk_2` == "---", as.numeric(0),as.numeric(.data$`Cl&Jerk_2`))) %>%
  mutate(`Cl&Jerk_3` = if_else(.data$`Cl&Jerk_3` == "---", as.numeric(0),as.numeric(.data$`Cl&Jerk_3`))) %>%
  mutate(Sincler = as.numeric(.data$Sincler)) %>%
  mutate(Sex = case_when(.data$Gender == "Males" ~ "M",
                         .data$Gender == "Females" ~ "F",
                         TRUE ~ "Females and males"))

# Running a complex set of functions to back calculate the total based on sinclair and also
# what the lifters best snatch and clean and jerk were as well as the sex of the lifter if it
# was left unknown based on sinclair values and their respective lifts. df9, df10
df9 <- subset(df8, df8$Gender == "Females and males") %>%
  rowwise() %>%
  mutate(Best_Data = best_lifts(
    snatch1 = .data$Snatch_1,
    snatch2 = .data$Snatch_2,
    snatch3 = .data$Snatch_3,
    cj1 = .data$`Cl&Jerk_1`,
    cj2 = .data$`Cl&Jerk_2`,
    cj3 = .data$`Cl&Jerk_3`,
    sinclair = .data$Sincler,
    sex = .data$Sex,
    BW = .data$B.W,
    year = case_when(as.Date(.data$Date) >= as.Date("2017-01-01") ~ 2022,
                     TRUE ~ 2015)))

# Make two new variables to get the lifters best snatch and clean and jerk attempts as well as gender
# if it was left unknown
df10 <- df9 %>%
  rowwise() %>%
  mutate(Best_Snatch = if_else(.data$Best_Data == "DNF", "DNF", 
                               str_split(.data$Best_Data, " ")[[1]][1])) %>%
  mutate(`Best_Cl&Jerk` = if_else(.data$Best_Data == "DNF", "DNF",
                                  str_split(.data$Best_Data, " ")[[1]][2])) %>%
  mutate(Gender = if_else(.data$Best_Data == "DNF", .data$Sex, 
                          str_split(.data$Best_Data, " ")[[1]][3]))

# create a proxy variable to assess lifters who either completed one lift or the other but not both
# in competition
df11 <- df10 %>%
  mutate(NA_check = if_else(.data$`Best_Cl&Jerk` == "NA" | .data$Best_Snatch == "NA", "NA", "Y")) %>%
  mutate(NA_check = case_when(.data$NA_check == "NA" & .data$Best_Snatch == "NA" & .data$`Best_Cl&Jerk` == "NA" ~ "NA",
                              .data$NA_check == "NA" & .data$Best_Snatch == "NA" & .data$`Best_Cl&Jerk` != "NA" ~ "Y-S",
                              .data$NA_check == "NA" & .data$Best_Snatch != "NA" & .data$`Best_Cl&Jerk` == "NA" ~ "Y-C",
                              TRUE ~ "Y")) %>%
  mutate(Best_Snatch_proxy = case_when(.data$NA_check == "NA" ~ "NA",
                                       .data$NA_check == "Y-S" & .data$Best_Snatch == "NA" ~ .data$`Best_Cl&Jerk`,
                                       .data$NA_check == "Y-C" & .data$Best_Snatch != "NA" ~ "DNF",
                                       .data$Best_Snatch == as.character(NA) ~ "DNF",
                                       TRUE ~ .data$Best_Snatch)) %>%
  mutate(`Best_Cl&Jerk_proxy` = case_when(.data$NA_check == "NA" ~ "NA",
                                          .data$NA_check == "Y-C" & .data$`Best_Cl&Jerk` == "NA" ~ .data$Best_Snatch,
                                          .data$NA_check == "Y-S" & .data$`Best_Cl&Jerk` != "NA" ~ "DNF",
                                          .data$`Best_Cl&Jerk` == as.character(NA) ~ "DNF",
                                          TRUE ~ .data$`Best_Cl&Jerk`)) %>%
  mutate(Best_Snatch = .data$Best_Snatch_proxy) %>%
  mutate(`Best_Cl&Jerk` = .data$`Best_Cl&Jerk_proxy`)

# Weird extra processing step
df12 <- df11 %>%
  mutate(Best_Snatch = if_else(grepl("Snatch", .data$Gender), .data$Gender, .data$Best_Snatch)) %>%
  mutate(`Best_Cl&Jerk` = if_else(grepl("Cl&Jerk", .data$Gender), .data$Gender, .data$`Best_Cl&Jerk`)) %>%
  mutate(Gender = ifelse(grepl("3", .data$Gender), .data$Sex, .data$Gender))


# Still with "Females and males" or "Females" which is still doesnt know their reported gender
# Would have to guess based on sinclair and bodyweight... 
# for simplicity... remove them here..
# there are also those that did not finish and competed only in C+J, remove these 22
df13 <- subset(df12, !(df12$Gender %in% c("Females and males")))
df13 <- df13 %>%
  filter(.data$Best_Snatch_proxy %in% c("DNF", "Snatch_1", "Snatch_2", "Snatch_3"))

# Find out if the competition is an international one by seeing if there are more than 3
# nationalities competing in the competition.
df14 <- df13 %>%
  group_by(.data$Competition_Name, .data$Date, .data$Place) %>%
  mutate(count = n_distinct(.data$Nation)) %>%
  mutate(competition_type = ifelse(count > 3, "International", "Local")) %>%
  ungroup() 

# Add in the snatch varience and clean and jerk varience between attempts.
df15 <- df14 %>%
  mutate(Snatch_varience = case_when(.data$Snatch_3 == 0 & .data$Snatch_2 == 0 ~ 0,
                                     .data$Snatch_3 == 0 & .data$Snatch_2 != 0 ~ .data$Snatch_2 - .data$Snatch_1,
                                     TRUE ~ .data$Snatch_3 - .data$Snatch_1)) %>%
  mutate(CJ_varience = case_when(.data$`Cl&Jerk_3` == 0 & .data$`Cl&Jerk_2` == 0 ~ 0,
                                 .data$`Cl&Jerk_3` == 0 & .data$`Cl&Jerk_2` != 0 ~ .data$`Cl&Jerk_2` - .data$`Cl&Jerk_1`,
                                 TRUE ~ .data$`Cl&Jerk_3` - .data$`Cl&Jerk_1`))

# Adding in the best snatch and the best clean and jerk for the lifter.
Snatch <- c()
`Cl&Jerk` <- c()
for (i in 1:nrow(df15)) {
  if (df15$Best_Snatch[i] != "DNF") {
    Snatch[[i]] <- df15[[as.name(df15$Best_Snatch[i])]][i]
  } else {Snatch[[i]] <- "DNF"}
  
  if (df15$`Best_Cl&Jerk`[i] != "DNF") {
    `Cl&Jerk`[[i]] <- df15[[as.name(df15$`Best_Cl&Jerk`[i])]][i]
  } else { `Cl&Jerk`[[i]] <- "DNF" }
}

# calculating the total, and seeing if the lifter missed their first snatch attempt by 
# adding First_attempt_flag == "Y" as a variable. This is an assumption but a pretty good one,
# as I still get over 10000 data points with a Y flag. It only cant accurately know if a lifter
# missed the first attempt and decided to move up on their second. This is extremely rare in lifting,
# and i've only seen it happen a few times.
df15 <- cbind(df15, Snatch = unlist(Snatch), `Cl&Jerk` = unlist(`Cl&Jerk`))
df16 <- df15 %>%
  mutate(Snatch = as.numeric(.data$Snatch)) %>%
  mutate(`Cl&Jerk` = as.numeric(.data$`Cl&Jerk`)) %>%
  mutate(Total = .data$Snatch + .data$`Cl&Jerk`) %>%
  select(-c("Sex", "Points", "weightclass_rule", "Best_Data", "NA_check", "count", "Best_Cl&Jerk_proxy", "Best_Snatch_proxy")) %>%
  mutate(First_attempt_flag = case_when(
    .data$Snatch_1 == .data$Snatch_3 ~ "Y",
    .data$Snatch_1 == .data$Snatch_2 ~ "Y",
    TRUE ~ "N"
  ))

# Calculate the age of the lifter if possible.
library(lubridate)
Age <- c()
for(i in 1:nrow(df16)) {
  if (is.na(df16$Birthyear[[i]])) {
    Age[[i]] <- as.numeric(0)
  } else if (!is.na(df16$Date[[i]])) {
    c_year <- as.numeric(year(as.Date(df16$Date[[i]])))
    Age[[i]] <- c_year - as.numeric(paste0("19", df16$Birthyear[[i]]))
  }
}

df17 <- cbind(df16, Age = unlist(Age)) %>%
  mutate(Age = case_when(.data$Age > 100 ~ as.numeric(NA), 
                         .data$Age == 0 ~ as.numeric(NA),
                         TRUE ~ .data$Age))

df17 <- df17[,c("Competition_Name", "Place", "Date", "competition_type", "Age_Category",
                "Competitor_Name", "Club", "Gender", "Age", "Nation", "B.W", "Snatch_1",
                "Snatch_2", "Snatch_3", "Cl&Jerk_1", "Cl&Jerk_2", "Cl&Jerk_3", "Snatch",
                "Cl&Jerk", "Total", "Sincler", "First_attempt_flag", "Snatch_varience", "CJ_varience",
                "Best_Snatch", "Best_Cl&Jerk")] %>%
  filter(.data$Gender %in% c("F","M")) 

# Save the processed data
# write.csv(df17,"C:/Users/chris/Documents/R/wl_data3.csv", row.names = FALSE)
# df17<- read.csv("C:/Users/chris/Documents/R/wl_data3.csv") %>%
#    rename(`Cl&Jerk_1` = Cl.Jerk_1, `Cl&Jerk_2` = Cl.Jerk_2, `Cl&Jerk_3` = Cl.Jerk_3, `Cl&Jerk` = Cl.Jerk,
#           `Best_Cl&Jerk` = Best_Cl.Jerk)

# df9<- read.csv("C:/Users/chris/Documents/R/wl_data2.csv") %>%
#   rename(`Cl&Jerk_1` = Cl.Jerk_1, `Cl&Jerk_2` = Cl.Jerk_2, `Cl&Jerk_3` = Cl.Jerk_3)
