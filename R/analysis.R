##### Prediction #####

if (!exists("df17")) {
  warning("Run the 'data_processing.R' script before proceeding")
}

# Can I predict a weightlifters total based on the weight of their first snatch attempt 
# and if they make it or not?

# Let's generate weightlifter classification rankings based on sinclair values.

vec56M <- get_sinclair_classes(c(107,143,157,170,200,245,290), BW=56, sex= "M", year=2022)
vec62M <- get_sinclair_classes(c(118,158,173,188,245,282,319), BW=62, sex= "M", year=2022)
vec69M <- get_sinclair_classes(c(127,167,190,213,278,296,341), BW=69, sex= "M", year=2022)
vec77M <- get_sinclair_classes(c(136,178,209,240,305,335,365), BW=77, sex= "M", year=2022)
vec85M <- get_sinclair_classes(c(143,190,225,259,326,356,385), BW=85, sex= "M", year=2022)
vec94M <- get_sinclair_classes(c(149,198,233,268,343,374,405), BW=94, sex= "M", year=2022)
vec105M <- get_sinclair_classes(c(156,205,241,277,350,383,417), BW=105, sex= "M", year=2022)
vec110M <- get_sinclair_classes(c(167,221,261,300,376,414,451), BW=170, sex= "M", year=2022)

vec48F <- get_sinclair_classes(c(69,91,103,114,143,168,192), BW=48, sex= "F", year=2022)
vec53F <- get_sinclair_classes(c(78,105,115,124,159,188,217), BW=53, sex= "F", year=2022)
vec58F <- get_sinclair_classes(c(85,112,127,142,184,209,233), BW=58, sex= "F", year=2022)
vec63F <- get_sinclair_classes(c(89,118,135,152,189,218,246), BW=63, sex= "F", year=2022)
vec69F <- get_sinclair_classes(c(94,127,142,156,196,229,262), BW=69, sex= "F", year=2022)
vec75F <- get_sinclair_classes(c(100,134,147,160,210,244,278), BW=75, sex= "F", year=2022)
vec120F <- get_sinclair_classes(c(116,154,164,174,237,278,319), BW=120, sex= "F", year=2022)

# To generate the rankings, I took an average of all the individual sinclair cutoffs
# per weightclass as shown below m_vec and f_vec.

df_m <- data.frame(vec56M, vec62M, vec69M, vec77M, vec85M, vec94M, vec105M, vec110M)
m_vec <- rowMeans(df_m[1:7,])
df_f <- data.frame(vec48F, vec53F, vec58F, vec63F, vec69F, vec75F, vec120F)
f_vec <- rowMeans(df_f[1:7,])

rankings_m <- make_tribble(as.numeric(m_vec))
print.data.frame(rankings_m)
rankings_f <- make_tribble(as.numeric(f_vec))
print.data.frame(rankings_f)

# Small picker functions help return the ranking classifications from tribbles above.
# all thats needed is an imput of the BW, viable_numbers is either the sinclair values
# for men or women, and the rank wanted.
picker <-  function(x, viable_numbers) {
  min(viable_numbers[viable_numbers >= x])
}
picker(113, rankings_m$sinclair)
picker2 <- function(x, viable_numbers, rank) {
  a <- picker(x, viable_numbers)
  rank[which(viable_numbers == a)]
}

# Only keep sinclair values below 475 (impossible to go above) and greater than 50
summary(df17$Sincler)
df18 <- df17 %>%
  filter(.data$Sincler < 475 & .data$Sincler > 50)

# Assign ranks for all the lifting performances
Rank <- c()
for (i in 1:nrow(df18)) {
  if (df18$Gender[[i]] == "M") {
    if (length(picker2(df18$Sincler[[i]], rankings_m[[2]], rankings_m[[1]])) == 0) {
      Rank[[i]] <- rankings_m$level[[7]]
    } else {
    Rank[[i]] <- picker2(df18$Sincler[[i]], rankings_m[[2]], rankings_m[[1]])
    }
  } else {
    if (length(picker2(df18$Sincler[[i]], rankings_f[[2]], rankings_f[[1]])) == 0) {
      Rank[[i]] <- rankings_f$level[[7]]
    } else {
    Rank[[i]] <- picker2(df18$Sincler[[i]], rankings_f[[2]], rankings_f[[1]])
    }
  }
}

# Bind the classification rank variable in
df18 <- cbind(df18, Rank = unlist(Rank))

# These datasets are for the density distributions below... Uncomment the last two lines
# for the snatch to clean and jerk density plots.
df_plot_m <- df18 %>%
  filter(.data$Gender == "M")
  # mutate(Ratio = (.data$Snatch/.data$`Cl&Jerk`)*100) %>%
  # filter(.data$Ratio < 120 & .data$Ratio > 50)
  
df_plot_f <- df18 %>%
  filter(.data$Gender == "F") 
  # mutate(Ratio = (.data$Snatch/.data$`Cl&Jerk`)*100) %>%
  # filter(.data$Ratio < 120 & .data$Ratio > 50)

# Density distributions of sinclair values by ranking
# wider densities is more variance within that group, 
# expected left shift at elite, and right shift at beginner

ggplot(data=df_plot_f, aes(x=Sincler, group=Rank, fill=Rank)) +
  geom_density(adjust=3.0, alpha=.5) +
  theme_classic() +
  ggtitle("Female Rankings Density Distributions") +
  ylab("Density") +
  xlab("Sinclair")

ggplot(data=df_plot_m, aes(x=Sincler, group=Rank, fill=Rank)) +
  geom_density(adjust=3.0, alpha=.5) +
  theme_classic() +
  ggtitle("Male Rankings Density Distributions") +
  ylab("Density") +
  xlab("Sinclair")


# Really cool, snatch to clean and jerk ratio by ranking, seems that for males there is a right
# shift in the mean as weightlifters become more skilled. This is also accompanied by a smaller std.dev.

# Uncomment these with the uncomments up above for df_plot_m and df_plot_f...

# ggplot(data=df_plot_f, aes(x=Ratio, group=Rank, fill=Rank)) +
#   geom_density(adjust=3.0, alpha=.5) +
#   theme_classic() +
#   ggtitle("Snatch/Clean and Jerk Ratio Female Distributions by Ranking") +
#   ylab("Density") +
#   xlab("S/CJ Ratio (%)")
# 
# ggplot(data=df_plot_m, aes(x=Ratio, group=Rank, fill=Rank)) +
#   geom_density(adjust=3.0, alpha=.5) +
#   theme_classic() +
#   ggtitle("Snatch/Clean and Jerk Ratio Male Distributions by Ranking") +
#   ylab("Density") +
#   xlab("S/CJ Ratio (%)")

##### Regression - Can I predict a weightlifters total based on their opening snatch and if they missed #####

# Not in the functions tab as this is only specific for certain variables.
regression <- function(data, rank = "") {

  if (rank == "") {
df18_m <- subset(data, select = c("Gender", "Snatch_1", "Total", "Snatch", "Cl&Jerk", "Snatch_varience", "B.W", "competition_type", "Rank", "Nation", "Sincler")) %>%
  filter(.data$Total > 0) %>%
  filter(.data$Snatch > 0 & .data$`Cl&Jerk` > 0) %>% 
  filter(.data$Total < 500) %>%
  mutate(check1 = if_else(.data$Snatch_1 < 50 & .data$Total > 150, "Y", "N")) %>%
  mutate(check2 = if_else(.data$Snatch_1 < 125 & .data$Total > 400, "Y", "N")) %>%
  mutate(check3 = if_else(!(.data$Total > (.data$Snatch_1*2)), "Y", "N")) %>%
  filter(.data$check1 == "N") %>%
  filter(.data$check2 == "N") %>%
  filter(.data$check3 == "N")
  } else {
    df18_m <- subset(data, select = c("Gender", "Snatch_1", "Total", "Snatch", "Cl&Jerk", "Snatch_varience", "B.W", "competition_type", "Rank")) %>%
      filter(.data$Total > 0) %>%
      filter(.data$Snatch > 0 & .data$`Cl&Jerk` > 0) %>% 
      filter(.data$Total < 500) %>%
      mutate(check1 = if_else(.data$Snatch_1 < 50 & .data$Total > 150, "Y", "N")) %>%
      mutate(check2 = if_else(.data$Snatch_1 < 125 & .data$Total > 400, "Y", "N")) %>%
      mutate(check3 = if_else(!(.data$Total > (.data$Snatch_1*2)), "Y", "N")) %>%
      filter(.data$check1 == "N") %>%
      filter(.data$check2 == "N") %>%
      filter(.data$check3 == "N") %>%
      filter(.data$Rank == rank)
}

### Linear regression
fit1 <- lm(Total ~ Snatch_1 * Gender, data = df18_m)
print(summary(fit1))
# par(mfrow=c(2,2))
# plot(fit1)

p <- ggplot(df18_m, aes(x=Snatch_1, y=Total, color=Gender)) +
  geom_point(alpha = 0.5) +
  theme_classic() +
  theme(legend.position=c(0.90,0.15)) +
  ggtitle(paste0("First Snatch Attempt(kg) to Total(kg)", "                ", rank)) +
  ylab("Total (kg)") +
  xlab("Snatch Attempt 1 (kg)") + geom_smooth(method='lm', fullrange = T) 

if (rank == "") {
  p1 <- ggMarginal(p, type="histogram", groupFill = T, groupColour =T, xparams = list(alpha=0.5),
                 yparams = list(alpha=0.5))
  return(p1)
} else {return(p)}

}

# Run all the regressions, uncomment to perform

# regression(df18)
# regression(df18, rank = "Elite")
# regression(df18, rank = "Beginner1")
# regression(df18, rank = "Beginner2")
# regression(df18, rank = "Intermediate1")
# regression(df18, rank = "Intermediate2")
# regression(df18, rank = "Advanced1")
# regression(df18, rank = "Advanced2")


# Plot
# ggplot(df18_m, aes(x = `Total`, y = `Gender`, fill = `Gender`)) +
#   geom_density_ridges(scale = 2, rel_min_height = 0.001, alpha = 0.25) +
#   labs(title = 'Total (kg) Lifted By Gender Distribution', x = "Total (kg)", y = "Gender") +
#   theme_classic() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   )

# boxplot_nice(df18, df18$Gender, df18$B.W)
# boxplot_nice(df18_m, df18_m$Gender, df18_m$Total)
# boxplot_nice(df18_m, df18_m$Gender, df18_m$Total)

df20 <- df18_m %>%
  group_by(.data$Rank, .data$Gender) %>%
  arrange(.data$Rank, .by_group = T) %>%
  mutate(num = n())

### Bar plot for categorical vars looking at the gender count for each ranking
ggplot(df20, aes(x = Rank, y = num, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "\n Weightlifting Rank", y = "Count \n", fill = "Gender \n",
       title = "Male and Female Weightlifter Count Across Ranks \n") +
  scale_x_discrete(labels = c("Advanced1", "Advanced2", "Beginner1", "Beginner2", "Elite", "Intermediate1", "Intermediate2")) +
  scale_fill_discrete(labels=c("Female", "Male")) +
  theme_classic() +
  theme(legend.position=c(0.80,0.80))



