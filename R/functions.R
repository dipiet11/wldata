
##### get_table_data #####
# Uses an xpath for a table on a webpage and gathers the information into a list
# multiple tables option returns all of the tables on a webpage if there is more than one.
get_table_data <- function(xpath, multipleTables = F) {
  
  data_table <- remDr$findElement(using = 'xpath', xpath)
  
  if (multipleTables == F) {
    data_table_html <- data_table$getPageSource()
    page <- read_html(data_table_html %>% unlist())
    return(html_table(page))
    
  } else {
    data_table_html <- data_table$getPageSource()
    page <- read_html(data_table_html %>% unlist())
    dt <- html_table(page)
    dt <- lapply(dt, function(x) {
      if (dim(x)[[1]] == 0) {
        x <- NA
      } else {x <- x}
    })
    dt <- dt[!is.na(dt)]
    list_dt <- lapply(dt, function(x) {
      x <- as.data.frame(x)
      colnames(x) <- c(1:dim(x)[2])
      return(x)
    })
    
    new_list <- rbindlist(list_dt)
    return(new_list)
  }
}

#####boxplot_nice#####
# returns a nice boxplot given the data, a categorical, and a numeric variable.
boxplot_nice <- function(data, cat_var, num_var) {
  p <- data %>%
    ggplot( aes(x=cat_var, y=num_var, fill=cat_var)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6) +
    geom_jitter(color="black", size=0.2, alpha=0.1) +
    theme_ipsum() +
    theme(
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("A boxplot with jitter") +
    xlab("")
  return(p)
}


##### missing_values #####
# Generates plot of missing values in a dataframe as a percentage
missing_values <- function(df) {
  
  missing.values <- df %>%
    gather(key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    group_by(key) %>%
    mutate(total = n()) %>%
    group_by(key, total, isna) %>%
    summarise(num.isna = n()) %>%
    mutate(pct = num.isna / total * 100)
  
  levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key
  
  percentage.plot <- missing.values %>%
    ggplot() +
    geom_bar(aes(x = reorder(key, desc(pct)), 
                 y = pct, fill=isna), 
             stat = 'identity', alpha=0.8) +
    scale_x_discrete(limits = levels) +
    scale_fill_manual(name = "", 
                      values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
    coord_flip() +
    labs(title = "Percentage of missing values", x =
           'Variable', y = "% of missing values")
  
  return(percentage.plot)
  
}

##### missing_rows #####
# Same as missing_values() but for rows
missing_rows <- function(df) {
  
  row.plot <- df %>%
    mutate(id = row_number()) %>%
    gather(-id, key = "key", value = "val") %>%
    mutate(isna = is.na(val)) %>%
    ggplot(aes(key, id, fill = isna)) +
    geom_raster(alpha=0.8) +
    scale_fill_manual(name = "",
                      values = c('steelblue', 'tomato3'),
                      labels = c("Present", "Missing")) +
    scale_x_discrete(limits = levels) +
    labs(x = "Variable",
         y = "Row Number", title = "Missing values in rows") +
    coord_flip()
  
  return(row.plot)
}



##### sinclair_calc #####
# Returns a vector of two possible total results based on sinclair. One is rounded up the other down.
sinclair_calc <- function(year = 2022, sex = "M", BW = 100, sinclair = 300, totalcalc = F) {
  if (sex == "M") {
    alpha <- case_when(year %in% c(2017,2018,2019,2020,2021,2022) ~ 0.75194503,
                       year %in% c(2013, 2014, 2015, 2016) ~ 0.794358141,
                       year %in% c(2009, 2010, 2011, 2012) ~ 0.784780654,
                       year %in% c(2004, 2005, 2006, 2007, 2008) ~ 0.845716976,
                       TRUE ~ 0.845716976
    )
    beta <- case_when(year %in% c(2017,2018,2019,2020,2021,2022) ~ 175.508,
                      year %in% c(2013, 2014, 2015, 2016) ~ 174.393,
                      year %in% c(2009, 2010, 2011, 2012) ~ 173.9,
                      year %in% c(2004, 2005, 2006, 2007, 2008) ~ 168.091,
                      TRUE ~ 168.091
    )
  } else {
    alpha <- case_when(year %in% c(2017,2018,2019,2020,2021,2022) ~ 0.783497476,
                       year %in% c(2013, 2014, 2015, 2016) ~ 0.89726074,
                       year %in% c(2009, 2010, 2011, 2012) ~ 1.056683941,
                       year %in% c(2004, 2005, 2006, 2007, 2008) ~ 1.316081431,
                       TRUE ~ 1.316081431
    )
    beta <- case_when(year %in% c(2017,2018,2019,2020,2021,2022) ~ 153.655,
                      year %in% c(2013, 2014, 2015, 2016) ~ 148.026,
                      year %in% c(2009, 2010, 2011, 2012) ~ 125.4,
                      year %in% c(2004, 2005, 2006, 2007, 2008) ~ 107.844,
                      TRUE ~ 107.844
    )
  }
  
  if (totalcalc == T) {
    sinclair_a <- log10(BW/beta)^2
    sinclair_a <- sinclair_a*alpha
    sinclair_a <- 10^sinclair_a
    return(sinclair_a)
  } else {
    sinclair_a <- log10(BW/beta)^2
    sinclair_a <- sinclair_a*alpha
    sinclair_a <- 10^sinclair_a
    total <- sinclair/sinclair_a
    total <- c(floor(total), ceiling(total))
    return(total)
  }
}

##### Make tribble #####
# Makes a tribble for sinclair ranking classification
make_tribble <- function(total_vector) {
  ranking_classification <- tribble(
    ~level, ~sinclair,
    "Beginner1", as.numeric(total_vector[[1]]),
    "Beginner2", as.numeric(total_vector[[2]]),
    "Intermediate1", as.numeric(total_vector[[3]]),
    "Intermediate2", as.numeric(total_vector[[4]]),
    "Advanced1", as.numeric(total_vector[[5]]),
    "Advanced2", as.numeric(total_vector[[6]]),
    "Elite", as.numeric(total_vector[[7]]),
  )
  return(ranking_classification)
}

##### find_sinclair #####
# function for finding the sinclair value of a given total
find_sinclair <- function(BW, sex, year, total) {
  for (i in c(1:500)) {
    a <- sinclair_calc(year = year, sex=sex, BW=BW, sinclair = i, totalcalc=F)
    if (total %in% a) {
      return(i)
    }
  }
}

##### get_sinclair_classes #####
get_sinclair_classes <- function(total_vector, BW, sex, year) {
  return(sapply(total_vector, function(x) {
    find_sinclair(BW=BW, sex=sex, year=year, total=x)
  }))
}

##### unknown_s #####
# Helper function to work within best_lifts when the gender is unknown ("Females and males")
unknown_s <- function(year, BW, sinclair) {
  m <- sinclair_calc(year=year, sex="M", BW=BW, sinclair = sinclair)
  f <- sinclair_calc(year=year, sex="F", BW=BW, sinclair = sinclair)
  return(c(m,f))
}

##### best_lifts #####
best_lifts <- function(snatch1, snatch2, snatch3, cj1, cj2, cj3, sinclair,
                       BW, sex, year) {
  # if the person didn"t complete a lift, they have a sinclair score of 0
  if (sinclair == 0) {
    return("DNF")
  }
  
  if (sex ==  "Females and males") {
    total_range <- unknown_s(year = year, BW = BW, sinclair = sinclair)
    sc1 <- round(sinclair_calc(year = year, BW = BW, sex = "M", sinclair = sinclair, totalcalc = T)*total_range[[1]], digits = 1)
    sc2 <- round(sinclair_calc(year = year, BW = BW, sex = "M", sinclair = sinclair, totalcalc = T)*total_range[[2]], digits = 1)
  } else {
    total_range <- sinclair_calc(year = year, BW = BW, sex = sex, sinclair = sinclair)
  }
  
  #placeholder for opt, returned down below
  opt <- 999
  # If they bombed snatch and were able to do clean and jerk or vise versa, then their reported 
  # sinclair will be artifically low. We need to account for this by seeing if it is lower than the 
  # minimum total assuming they make the first lifts in both the snatch and clean and jerk.
  
  if (sex ==  "Females and males") {
    min_total_f <- min(snatch1, snatch2, snatch3) + min(cj1, cj2, cj3)
    min_sinclair_coef_f <- sinclair_calc(year = year, BW = BW, sex = "F", sinclair = sinclair, totalcalc = T)
    min_sinclair_f <- min_total_f*min_sinclair_coef_f
    min_total_m <- min(snatch1, snatch2, snatch3) + min(cj1, cj2, cj3)
    min_sinclair_coef_m <- sinclair_calc(year = year, BW = BW, sex = "M", sinclair = sinclair, totalcalc = T)
    min_sinclair_m <- min_total_m*min_sinclair_coef_m
  } else if (sex ==  "M"){
    min_total_m <- min(snatch1, snatch2, snatch3) + min(cj1, cj2, cj3)
    min_sinclair_coef_m <- sinclair_calc(year = year, BW = BW, sex = "M", sinclair = sinclair, totalcalc = T)
    min_sinclair_m <- min_total_m*min_sinclair_coef_m
  } else {
    min_total_f <- min(snatch1, snatch2, snatch3) + min(cj1, cj2, cj3)
    min_sinclair_coef_f <- sinclair_calc(year = year, BW = BW, sex = "F", sinclair = sinclair, totalcalc = T)
    min_sinclair_f <- min_total_f*min_sinclair_coef_f
  }
  
  if (sex ==  "Females and males") {
    if (sinclair < (min_sinclair_m*.8) | sinclair < (min_sinclair_f*.8)) {
      total_range <- sinclair_calc(year = year, BW = BW, sex = sex, sinclair = min_sinclair_m)
      opt_m <- which.min(abs(c(snatch1, snatch2, snatch3, cj1,cj2,cj3) - total_range))
      if (length(opt == 0)) {
        total_range <- sinclair_calc(year = year, BW = BW, sex = sex, sinclair = min_sinclair_f)
        opt_f <- which.min(abs(c(snatch1, snatch2, snatch3, cj1,cj2,cj3) - total_range))
        opt <- max(opt_f)
        marker <- "F"
      }
    } else if (between(round(sinclair, digits = 1), sc1, sc2)){
      opt <- 999
    } else {
      opt <- max(opt_m)
      marker <- "M"
    }
  } else if (sex ==  "M") {
    if (sinclair < (min_sinclair_m*.8)) { 
      total_range <- sinclair_calc(year = year, BW = BW, sex = sex, sinclair = min_sinclair_m)
      opt <- which.min(abs(c(snatch1, snatch2, snatch3, cj1,cj2,cj3) - total_range))
      opt <- max(opt)
      marker <- "M"
    }
  } else if (sex ==  "F") {
    if (sinclair < (min_sinclair_f*.8)) {
      total_range <- sinclair_calc(year = year, BW = BW, sex = sex, sinclair = min_sinclair_f)
      opt <- which.min(abs(c(snatch1, snatch2, snatch3, cj1,cj2,cj3) - total_range))
      opt <- max(opt)
      marker <- "F"
    }
  }
  
  if (opt == 999) {
    
    all_snatch <- c(snatch1, snatch2, snatch3)
    all_cj <- c(cj1, cj2, cj3)
    
    perms <- sapply(all_snatch, function(x) {
      sapply(all_cj, function(y) {
        return(sum(x,y))
      })
    })
    
    colnames(perms) <- c("Snatch_1", "Snatch_2", "Snatch_3")
    row.names(perms) <- c("Cl&Jerk_1", "Cl&Jerk_2", "Cl&Jerk_3")
    
    marker <- "M"
    
    if ((total_range[[1]] %in% perms)) {
      opt <- which(perms == total_range[[1]], arr.ind=TRUE)
      marker <- sex
    } else if (total_range[[2]] %in% perms) {
      opt <- which(perms == total_range[[2]], arr.ind=TRUE)
      marker <- sex
    } else if (sex == "Females and males" & length(total_range) > 2) {
      if (total_range[[3]] %in% perms) {
        opt <- which(perms == total_range[[3]], arr.ind=TRUE)
        marker <- "F"
      } else if (total_range[[4]] %in% perms) {
        opt <- which(perms == total_range[[4]], arr.ind=TRUE)
        marker <- "F"
      }
    } else if (length(total_range) > 2) {
      opt <- which(perms == total_range[[4]], arr.ind=TRUE)
      marker <- "F"
    } else {
      opt <- c(1,1)
      marker <- sex
    } 
    
    if (c("numeric") %in% class(opt)) {
      if (length(total_range) > 2) {
        perms1 <- (perms+1)
        check <- c((total_range[[1]] %in% perms1), (total_range[[2]] %in% perms1), 
                   (total_range[[3]] %in% perms1), (total_range[[4]] %in% perms1))
        check1 <- which(check == T)
        if(length(check1) == 0) {
          perms1 <- (perms-1)
          check <- c((total_range[[1]] %in% perms1), (total_range[[2]] %in% perms1), 
                     (total_range[[3]] %in% perms1), (total_range[[4]] %in% perms1))
          check1 <- which(check == T)
        }
        if(length(check1) == 0) {
          return(paste(as.character(NA), as.character(NA),as.character(NA)))
        } else {
          opt <- which(perms1 == total_range[[check1]], arr.ind=TRUE)
        }
      }
    }
    
    #based on the values for snatch and clean and jerk, pick the likely value.
    
    # If there is more than one permutation, take the last row.
    # This is because the snatch precedes the clean and jerk, so we are always
    # taking the later row.
    
    if(length(opt) > 2) {
      opt <- opt[nrow(opt),]
    }
    
    best_snatch <- paste0("Snatch_", opt[[2]])
    best_cj <- paste0("Cl&Jerk_", opt[[1]])
    
    return(paste(best_snatch, best_cj, marker))
  } else if (opt == 1) {
    return(paste("Snatch_1", as.character(NA), as.character(NA)))
  } else if (opt == 2) {
    return(paste(as.character(NA), "Snatch_2", as.character(NA)))
  } else if (opt == 3) {
    return(paste(as.character(NA), as.character(NA), "Snatch_3"))
  } else if (opt == 4) {
    return(paste("Cl&Jerk_1", as.character(NA), as.character(NA)))
  } else if (opt == 5) {
    return(paste(as.character(NA), "Cl&Jerk_2", as.character(NA)))
  } else if (opt == 6) {
    return(paste(as.character(NA), as.character(NA), "Cl&Jerk_3"))
  } else {return(paste(as.character(NA), as.character(NA), as.character(NA)))}
}

