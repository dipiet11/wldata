##### Using RSelenium to scrape the dynamic table data from the weightlifting website https://iwrp.net/ #####

# Set up our driver object, in this case it's chrome using the closest version to what I have in my browser.
# To check chrome version type in chrome://version to your browser. To check possible versions on your system
# binman::list_versions("chromedriver")

# Pull docker image for firefox:
# docker pull selenium/standalone-firefox:latest
# docker pull selenium/standalone-chrome
# docker run -d -p 4445:4444 --shm-size="2g" selenium/standalone-chrome
# docker run -d -p 4455:4444 -p 7902:7900 --shm-size="2g" selenium/standalone-chrome
# docker run -d -p 4445:4444 -p 7901:7900 --shm-size="2g" selenium/standalone-firefox:latest
# docker ps (listing of containers running)
# docker ps --format 'table {{.Names}}\t{{.Image}}\t{{.ID}}' for better visibility..
# docker inspect <containerNameorId> | grep '"IPAddress"' | head -n 1
# Assign docker_ip address based on above command.

# We can run the above comments in R, need to set the shell path to bash.exe
# Define the path to the shell executable
# This would be the only path specified by the user... Maybe put it into .Renviron file...
shell_path <- "C:\\Program Files\\Git\\bin\\bash.exe"

# Construct the command(s) to run
command_dockerps <- sprintf('"%s" -c "docker ps --format \'table {{.Names}}\\t{{.Image}}\\t{{.ID}}\'"', shell_path)
# Use system() to run the command
output <- system(command_dockerps, intern = TRUE)

# Get the outputs into another format.
output_matrix <- unname(sapply(output, function(items) {
  stringr::str_split(items, "\\s{2,}", simplify = TRUE)[1,]
}))
colnames(output_matrix) <- paste0("V", 1:ncol(output_matrix))

# Simple function to get IP address of a docker container
get_container_ip <- function(container_id, port = F) {
  # Construct the command
  command <- sprintf('docker inspect %s | grep \'"IPAddress"\' | head -n 1', container_id)
  command2 <- sprintf('docker inspect %s | grep \'"HostPort"\' | head -n 1', container_id)
  # Execute the command using shell_path (wrap in single quotes)
  shell_path <- '"C:\\Program Files\\Git\\bin\\bash.exe"'
  raw_output <- system(paste(shell_path, "-c", shQuote(command)), intern = TRUE)
  raw_output2 <- system(paste(shell_path, "-c", shQuote(command2)), intern = TRUE)
  # Extract IPAddress out of output using Common Regular Expression for IPv4
  output <- stringr::str_extract(raw_output, '[0-9]{1,3}(?:\\.[0-9]{1,3}){3}')
  output2 <- stringr::str_extract(raw_output2, '[0-9]{1,8}')
  # Return the output
  if (isTRUE(port)) {
    return(output2)
  } else {
    return(output)
  }
}

# Create output of active docker containers and obtain whichever row is of interest.
outputs_df <- as.data.frame(output_matrix, stringsAsFactors = FALSE) |>
  tidyr:::pivot_longer(cols = -V1,
                       names_to = c(".value", NA),
                       names_sep = "[0-9]"
                       ) |>
  tidyr::pivot_wider(
    names_from = V1,
    values_from = V,
    values_fn = list
    ) |>
  tidyr::unnest(cols = everything()) |>
  # Filter for the docker image we are intested in
  dplyr::filter(IMAGE == "selenium/standalone-chrome") |>
  # get the IP address
  dplyr::rowwise() |>
  dplyr::mutate(IP_address = get_container_ip(NAMES)) |>
  dplyr::mutate(HostPort = as.numeric(get_container_ip(NAMES, port = TRUE))) |>
  dplyr::ungroup()

# if (nrow(outputs_df) <= 1) {
  # Choose any port that is available on docker... in this case 4445L has been set up as above.
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = outputs_df$IP_address[[1]],
    port = outputs_df$HostPort[[1]],
    browserName = "chrome"
    )
# } else {
#   # Open multiple containers
# }

library(httr)
library(RSelenium)
library(wdman)
library(netstat)
# Load selenium dependencies
selenium()
# Check where chromedriver.exe is located and delete the license files within binman chromedriver files.
selenium_object <- selenium(retcommand = T, check = F)

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
remDr$open()

#google chrome
chrome_versions <- unlist(binman::list_versions("chromedriver"))
remote_driver <- RSelenium::rsDriver(browser = "chrome",
                                     chromever = chrome_versions[[length(chrome_versions)]],
                                     verbose = F,
                                     port = netstat::free_port(),
                                     iedrver = NULL,
                                     check = FALSE)

# Set a longer implicit wait
# remDr$open()

# Set remDr to the client
remDr <- remote_driver$client

# run to make sure it's connected to R
# RSelenium::remDr$open()

# Navigate to the page of interest, in my case it is this website with a lot of weightlifting results data.
# We can also use the wayback machine snapshot so not to overwhelm the original server
# https://web.archive.org/web/20240701085947/https://iwrp.net/
# Original: "http://iwrp.net/"
# But let's use a combination of both to get the most up-to-date latest data.

URL_nav <- "http://iwrp.net/"
remDr$navigate(URL_nav)
# Rselenium::remDr$navigate("http://iwrp.net/")

# Switch the entries tab on the webpage to list "all" of the data on one page. I found the xpath to
# this by using devtools in chrome and copying the xpath associated with the entries tab = all.
# next we can actually tell our browser to click on the tab option we want by calling the element$clickElement()

data_tab <- remDr$findElement(using = 'xpath', '//*[@id="stat_tab_length"]/label/select/option[3]')
data_tab$clickElement()

# Getting the main page data by using the xpath of table and running it through my get_table_data function
# located in the functions.R script.
main_df <- get_table_data(
  xpath = '//*[@id="stat_tab"]/tbody/tr/td'
)
main_df <- main_df[[2]]

# Now the main table is dynamic with links to each competition results on a different webpage. In order
# to scrape through all of the links, we need to set up a loop.

# cond  is a condition on which there are still entries to click on the table below, once there are non left
# cond will be set to false and break the loop
# w is a counter for loop iterition
# table_list is where all my competition data will be stored in a list.
cond <- TRUE
w <- 0
table_list <- list()

while (cond == TRUE) {

  # Every time the loop iterates, navigate back to the homepage and set the entries to all again.
  tryCatch(
    {
      remDr$navigate(URL_nav)
      Sys.sleep(0.5)
      data_tab <- remDr$findElement(using = 'xpath', '//*[@id="stat_tab_length"]/label/select/option[3]')
      data_tab$clickElement()
    },
    # If there is no link, append NA to the list and go to the next item in the loop
    error = function(e) {
      table_list <- append(table_list, list(NA))
      i <- w + 1
      next
    }
  )

  # iteration counter
  i <- w + 1

  # clicks on the loop iteration file for competiton data, this is based on location of the table in the xpath
  tryCatch(
    {
      clicknewtab <- remDr$findElement(using = 'xpath', paste0('//*[@id="stat_tab"]/tbody/tr[', i, "]/td[2]/a"))
      Sys.sleep(0.5)
      clicknewtab$clickElement()
    },
    # If there are no more links to click, condition is set to FALSE, and the loop ends.
    error = function(e) {
      print("Script Complete!")
      cond <<- FALSE
    }
  )

  # Run our get_table_data function to scrape the data of each competition within its respective link.
  tryCatch(
    {
      df_x <- get_table_data(
        xpath = paste0('//*[@id="wrapper-pattern"]/div[3]/div[4]/table[1]/tbody'),
        multipleTables = T)
    },
    error = function(e) {
      break
    }
  )

  # append the competition data to the list of competition data
  table_list <- append(table_list, list(df_x))

  if (cond == FALSE) {
    break
  }

  w <- w + 1
}

# renaming the list items to the name of the respective competition
table_list_rename <- table_list
comp_names <- main_df$Name[1:length(table_list_rename)]
names(table_list_rename) <- comp_names

# Need to standardize the column data, need to check variable names for every single table,
# I will store the unique values in the variable_list object
variable_list <- list()

# The first row of every table has all of the column name data that wasn't picked up initially by scraping.
for (q in 1:length(table_list_rename)) {
  # print(q)
  for (i in 1:dim(table_list_rename[[q]])[[2]]) {
    if (!(table_list_rename[[q]][1][[i]] %in% variable_list)) {
      variable_list <- append(variable_list, table_list_rename[[q]][1][[i]])
      variable_list <- variable_list[!variable_list %in% c(NA, "NA", "<NA>")]
    }
  }
}

variable_list <- unlist(variable_list)

# It seems that there these unique variables in all of the datasets
# Surname and name
# Name and surname
# Sincler
# Club
# B.W.
# Birthyear
# Snatch (3 values)
# Cl&Jerk (3 values)
# Nation
# Total
# Points
# Gr. == Group (Group A, B, C)
# Pl == placing
# ""

# Let's make sure that all of the column names match up for merging.
standardize_col_names <- lapply(table_list_rename, function(x) {
  names(x) <- as.character(x[1])
  x <- subset(x, select = names(x) %in% variable_list)
  return(x)
})

# Drop Group data (var=Gr.) this is not important as this is only for international competitions
# where the analysis would be focused on elite level athletes.
# Also dropping "" and "Total" variables as these will have to be recalculated after for tables without these.
drop_grp_var <- lapply(standardize_col_names, function(x) {
  x <- subset(x, select = !(names(x) %in% c("Gr.","Total", "")))
  return(x)
})

#### Now add a club variable to each if not present and make the value NA
add_club_var <- lapply(drop_grp_var, function(x) {
  if ("Club" %in% names(x)) {
    return(x)
  } else {
    Club <- rep(NA, dim(x)[[1]])
    x <- cbind(x, Club)
    return(x)
  }
})

# Now add in nation if the nation is missing from the original main_df_table, also merge all the information
# from this table here based on the name of the list.

main_df_new <- main_df[c(1:length(drop_grp_var)),]
df5 <- add_club_var

for (i in 1:nrow(main_df_new)) {
  nat <- main_df_new$Nation[[i]]
  if ("Nation" %in% names(df5[[i]])) {
    df5[[i]] <- df5[[i]]
  } else {
    Nation <- rep(nat, dim(df5[[i]])[[1]])
    df5[[i]] <- cbind(df5[[i]], nat)
  }
  Date <- rep(main_df_new$Date[[i]], dim(df5[[i]])[[1]])
  Competition_Name <- rep(main_df_new$Name[[i]], dim(df5[[i]])[[1]])
  Place <- rep(main_df_new$Place[[i]], dim(df5[[i]])[[1]])
  Gender <- rep(main_df_new$Gender[[i]], dim(df5[[i]])[[1]])
  Age_Category <- rep(main_df_new$Age.category[[i]], dim(df5[[i]])[[1]])
  df5[[i]] <- cbind(df5[[i]], Date, Competition_Name, Place, Gender, Age_Category)
}

#### Now add a "Birthyear" variable to each if not present and make the value NA
add_birth_year <- lapply(df5, function(x) {
  if ("Birthyear" %in% names(x)) {
    return(x)
  } else {
    Birthyear <- rep(NA, dim(x)[[1]])
    x <- cbind(x, Birthyear)
    return(x)
  }
})

#### Check to see if all the datatables have 18 rows
for (i in 1:length(add_birth_year)) {
  if (dim(add_birth_year[[i]])[2] != 18) {
    print(i)
  }
}
table_list_rename_test
# They do!

# Change the name of Snatch and Cl&Jerk to Snatch_1, Snatch_2, Snatch_3 & Cl&Jerk_1...etc
df6 <- lapply(add_birth_year, function(x) {
  loc_snatch <- unlist(list(which(names(x) == "Snatch")))
  loc_cj <- unlist(list(which(names(x) == "Cl&Jerk")))
  colnames(x)[loc_snatch[1]] <- "Snatch_1"
  colnames(x)[loc_snatch[2]] <- "Snatch_2"
  colnames(x)[loc_snatch[3]] <- "Snatch_3"
  colnames(x)[loc_cj[1]] <- "Cl&Jerk_1"
  colnames(x)[loc_cj[2]] <- "Cl&Jerk_2"
  colnames(x)[loc_cj[3]] <- "Cl&Jerk_3"
  return(x)
})

# Let's bind all the data together.
combined_data <- bind_rows(df6)
# Seeing here there are a lot of rows that were previously variable names in the original tables
summary(as.factor(combined_data$B.W))
# 14218 rows where B.W. is plugged in as name, and 1624 unreported rows. These are all up for deletion
combined_data <- combined_data %>%
  filter(.,!(.data$B.W %in% c("", "B.W")))

# Now we have our starting dataset!
# write.csv(combined_data,"C:/Users/chris/Documents/R/wl_data.csv", row.names = FALSE)
combined_data <- read.csv("C:/Users/chris/Documents/R/wl_data.csv") %>%
  rename(`Cl&Jerk_1` = Cl.Jerk_1, `Cl&Jerk_2` = Cl.Jerk_2, `Cl&Jerk_3` = Cl.Jerk_3,
         `Surname and name` = Surname.and.name, `Name and surname` = Name.and.surname)
