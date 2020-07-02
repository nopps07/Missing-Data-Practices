#Missing Data

#packages
library(naniar)
library(dplyr)
library(ggplot2)
library(simputation)
install.packages("simputation")



#Using and finding missing values

# Create x, a vector, with values NA, NaN, Inf, ".", and "missing"
x <- c(NA, NaN, Inf, ".", "missing")

# Use any_na() and are_na() on to explore the missings
any_na(x)
are_na(x)


#One of the firs things that you will want to check with
#a new dataset is if there are any missing missing values,
#and how many there are.
#You could use 'are_na()' to and count up the missing values,
#but the most efficient way to count missings is to use the
#'n_miss()' function.
#This will tell you the total number of missing values in the data


# Use n_miss() to count the total number of missing values in dat_hw
n_miss(dat_hw)

# Use n_miss() on dat_hw$weight to count the total number of missing values
n_miss(dat_hw$weight)

# Use n_complete() on dat_hw to count the total number of complete values
n_complete(dat_hw)

# Use n_complete() on dat_hw$weight to count the total number of complete values
n_complete(dat_hw$weight)

# Use prop_miss() and prop_complete() on dat_hw to count the total number of missing values in each of the variables
prop_miss(dat_hw)
prop_complete(dat_hw)


1 + NA
NA + NA
NA | TRUE
NA | FALSE


## Why care about missing values?

#Basic summaries of missingness
#n_miss
#n_complete

#Dataframe summaries of missingness
#miss_var_summary
#miss_case_summary [each row. case 5 - the fifth row in the dataset]
#These fuctions work with group_by()

#tabulations
#miss_var_table()
#miss_case_table()

#Spans of missing data [time series]
#miss_var_span(x, var = hourly_counts, span_every = 4000)

#x %>%
# group_by(Month) %>%
# miss_var_summary()


# Return the summary of missingness in each variable, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_var_summary()

# Return the summary of missingness in each case, grouped by Month, in the `airquality` dataset
airquality %>% group_by(Month) %>% miss_case_summary()


# Tabulate missingness in each variable and case of the `airquality` dataset
miss_var_table(airquality)
miss_case_table(airquality)

# Tabulate the missingness in each variable, grouped by Month, in the `airquality` dataset
airquality %>% 
  group_by(Month) %>%
  miss_var_table()

# Tabulate of missingness in each case, grouped by Month, in the `airquality` dataset
airquality %>% 
  group_by(Month) %>% 
  miss_case_table()

# Calculate the summaries for each run of missingness for the variable, hourly_counts
miss_var_run(pedestrian, var = hourly_counts)

# Calculate the summaries for each span of missingness, for a span of 4000, for the variable hourly_counts
miss_var_span(pedestrian, var = hourly_counts, span_every = 4000)

# For each `month` variable, calculate the run of missingness for hourly_counts
pedestrian %>% group_by(month) %>% miss_var_run(var = hourly_counts)

# For each `month` variable, calculate the span of missingness of a span of 2000, for the variable hourly_counts
pedestrian %>% group_by(month) %>% miss_var_span(var = hourly_counts, span_every = 2000)


install.packages("naniar")

##imputation practice?
#ȥ?? ???? ?????? ???? ?ش?
#multiple imputation?
install.packages("mice")
require("mice")

data(nhanes2)
str(nhanes2)
summary(nhanes2)

n_miss(nhanes2)
any_na(nhanes2)
miss_var_table(nhanes2)
miss_case_table(nhanes2)


nhanes2 %>%
  group_by(age) %>%
  miss_var_run(var = bmi)

miss_var_summary(nhanes2)


(imp.mice = mice(nhanes2, m = 5))

fit.chl <- with(imp.mice, lm(chl ~ age + bmi))      
pool(fit.chl)
round(summary(pool(fit.chl)), 2)

fit.hyp <- with(imp.mice, glm(hyp ~ bmi + chl, family = binomial))
pool(fit.hyp)
round(summary(pool(fit.hyp)),2)
#the difference of lm & glm?


##How do we visualize missing values?

#naniar provides a friendly family of missing data visualization functions.
#each visualization corresponds to a data summary.
vis_miss(nhanes2)
vis_miss(nhanes2, cluster = TRUE)
#'cluster = TRUE': this orders the rows by missingness to identify common co-occurence

#Quickly look at missings
gg_miss_var(nhanes2)
gg_miss_case(nhanes2)

gg_miss_var(nhanes2, facet = age)

#Visualizing missingness patterns
gg_miss_upset(nhanes2)

#Visualizaing factors of missingness
gg_miss_fct(x = nhanes2, fct = age)

##Practice
# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(riskfactors)

# Visualize and cluster all of the missingness in the `riskfactors` dataset
vis_miss(riskfactors, cluster = TRUE)

# visualise and sort the columns by missingness in the `riskfactors` dataset
vis_miss(riskfactors, sort_miss = TRUE)

# Visualize the number of missings in cases using `gg_miss_case()`
gg_miss_case(riskfactors)

# Explore the number of missings in cases using `gg_miss_case()` and facet by the variable `education`
gg_miss_case(riskfactors, facet = education)

# Visualize the number of missings in variables using `gg_miss_var()`
gg_miss_var(riskfactors)

# Explore the number of missings in variables using `gg_miss_var()` and facet by the variable `education`
gg_miss_var(riskfactors, facet = education)

# Using the airquality dataset, explore the missingness pattern using gg_miss_upset()
gg_miss_upset(airquality)

# With the riskfactors dataset, explore how the missingness changes across the marital variable using gg_miss_fct()
gg_miss_fct(x = riskfactors, fct = marital)

# Using the pedestrian dataset, explore how the missingness of hourly_counts changes over a span of 3000 
gg_miss_span(pedestrian, var = hourly_counts, span_every = 3000)

# Using the pedestrian dataset, explore the impact of month by facetting by month
# and explore how missingness changes for a span of 1000
gg_miss_span(pedestrian, var = hourly_counts , span_every = 1000, facet = month)




## Searching for and replacing missing values

#How to look for hidden missing values
#Replacing missing value labels with NA
#Checking your assumptions on missingness

#ideal: NA
#real life: 'missing' 'Not Available' 'N/A'

#How to replace them with regular 'NA' values

#miss_scan_count()
hsb2 %>%
  miss_scan_count(search = list("N/A", "N/a"))

#Replacing
hsb2 %>%
  replace_with_na(replace = list(grade = c("N/A", "N/a")))
#use chaos, then replace with NA for the variable 'grade' with the values N/A and N/a


#'scoped variants' of replace_with_na
#replace_with_na can be repetitive:
#Use is across many different variables and values
#Complex cases, replacing values less than -1, only affect character columns

#replace_with_na_all() All variables
#replace_with_na_at() A subset of selected variables
#replace_with_na_if() A subset of variables that fulfill some condition (numeric, character)
hsb2 %>%
  replace_with_na_all(condition = ~.x == -99)
#replace all cases of - 99 in a dataset

#Using scoped variants of replace_with_na
hsb2 %>%
  replace_with_na_all(condition = ~.x %in% c("N/A", "missing", "na"))


##exercise
# Explore the strange missing values "N/A"
miss_scan_count(data = pacman, search = list("N/A"))

# Explore the strange missing values "missing"
miss_scan_count(data = pacman, search = list("missing"))


# Use `replace_with_na_at()` to replace with NA
replace_with_na_at(pacman,
                   .vars = c("year", "month", "day"), 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_if()` to replace with NA the character values using `is.character`
replace_with_na_if(pacman,
                   .predicate = is.character, 
                   ~.x %in% c("N/A", "missing", "na", " "))

# Use `replace_with_na_all()` to replace with NA
replace_with_na_all(pacman, ~.x %in% c("N/A", "missing", "na", " "))

# Explore the strange missing values "na"
miss_scan_count(data = pacman, search = list("na"))

# Explore the strange missing values " " (a single space)
miss_scan_count(data = pacman, search = list(" "))

# Explore all of the strange missing values, "N/A", "missing", "na", " "
miss_scan_count(data = pacman, search = list("N/A", "missing", "na", " "))

# Print the top of the pacman data using `head()`
head(pacman)

# Replace the strange missing values "N/A", "na", and "missing" with `NA` for the variables, year, and score
pacman_clean <- replace_with_na(pacman, replace = list(year = c("N/A", "na", "missing"),
                                                       score = c("N/A", "na", "missing")))

# Test if `pacman_clean` still has these values in it?
miss_scan_count(pacman_clean, search = list("N/A", "na", "missing"))

?replace_with_na_all


##Filling down missing values

#how to efficiently handle implicit missing values

#explicitly: They are missing with NA
#implicitly: Not shown in the data, but implied

#making implicit missings explicit
tetris %>%
  tidyr::complete(name, time)
#handling explicitly missing values
tetris %>%
  tidyr::fill(name)
#A warning: only if it is easy to notice what the missing value is

# Use `complete()` on the `time` and `name` variables to make implicit missing values explicit
frogger_tidy <- frogger %>% complete(name, time)

# Correctly fill() and complete() missing values so that our dataset becomes sensible
frogger %>% 
  fill(name) %>%
  complete(name, time)

#Missing Data dependence
#MCAR: Missing Completely at Random
#MAR: Missing At Random
#MNAR: Missing Not at random

#MCAR - missingness has no association with any data you have observed, or not observed.
#implications:
#imputation is advisable
#deleting observations may reduce sample size, limiting inference, but will not bias
#You should be imputing data

#MAR
#missingness depends on data observed, but not data unobserved.
#Implications:
#impute
#Deleting observations not ideal, may lead to bias

#MNAR
#Missingness of the response is related to an unobserved value relevant to the assessment of interest
#Implications:
#Data will be biased from deletion and imputation
#Inference can be limited, proceeed with caution.



# Arrange by year
oceanbuoys %>% arrange(year) %>% vis_miss()

# Arrange by latitude
oceanbuoys %>% arrange(latitude) %>% vis_miss()

# Arrange by wind_ew (wind east west)
oceanbuoys %>% arrange(wind_ew) %>% vis_miss()

#how to explain the missingness of them..? hmm... tricky
#consider using facet!



##Tools to explore missing data dependence

#Missing Data workflows:
#The Shadow matrix and Nabular data

#The Shadow Matrix
#Two main features
#1.coordinated names
#2.clear values

#creating nabular data
#Using nabluar data to perform summaries
airquality %>%
  bind_shadow() %>%
  group_by(X) %>%
  summarise(mean = mean(Y))

#excercise

#missing data can be tricky to think about, 
#as they don't usually proclaim themselves for you,
#and instead hide amongst the weeds of the data

#one way to help expose missing values is to change
#the way we think about the data - by thinking about
#every single data value being missing or not missing.
#The 'as_shadow()' function in R transforms a dataframe
#into a shadow matrix, a special data format where the values are either missing or Not missing
#The column names of a shadow matrix are the same as the data,
#but have a suffix added _NA
#To keep track of and compare data values to their missingness state,
#use the 'bind_shadow()' function.
#Having data in this format, with the shadow matrix column bound to the regular data is called nabular data.


# Create shadow matrix data with `as_shadow()`
as_shadow(oceanbuoys)

# Create nabular data by binding the shadow to the data with `bind_shadow()`
bind_shadow(oceanbuoys)

# Bind only the variables with missing values by using bind_shadow(only_miss = TRUE)
bind_shadow(oceanbuoys, only_miss = TRUE)

##usually the first step in more advanced summaries of missing data


# `bind_shadow()` and `group_by()` humidity missingness (`humidity_NA`)
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>% 
  summarise(wind_ew_mean = mean(wind_ew), # calculate mean of wind_ew
            wind_ew_sd = sd(wind_ew)) # calculate standard deviation of wind_ew

# Repeat this, but calculating summaries for wind north south (`wind_ns`).
oceanbuoys %>%
  bind_shadow() %>%
  group_by(humidity_NA) %>%
  summarise(wind_ns_mean = mean(wind_ns),
            wind_ns_sd = sd(wind_ns))

##bind_shadow() for turning the data into nabular data
#and group_by and summarise what I want

#further exploring more combinations of missingness
#it can be useful to get a bit of extra information
#about the numbers of caes in each missing condition.
#In this exercise, we are going to add information about the number of observed cases using n() inside the summarise() function.
#We will then add an additional level of grouping by looking at the combination of humidity being missing ....



# Summarise wind_ew by the missingness of `air_temp_c_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA) %>%
  summarise(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

# Summarise wind_ew by missingness of `air_temp_c_NA` and `humidity_NA`
oceanbuoys %>% 
  bind_shadow() %>%
  group_by(air_temp_c_NA, humidity_NA) %>%
  summarise(wind_ew_mean = mean(wind_ew),
            wind_ew_sd = sd(wind_ew),
            n_obs = n())

#Visualizing missingness across one variable
#exploring conditional missings with ggplot

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             color = Ozone_NA)) +
  geom_density()

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Ozone_NA,
             y = Temp)) +
  geom_boxplot()

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp)) +
  geom_density() +
  facet_wrap(~Ozone_NA)
#splitting by facet can be useful if you want to compare different types of visualisation.

airquality %>%
  bind_shadow() %>%
  ggplot(aes(x = Temp,
             y = Wind,
             color = Ozone_NA)) +
  geom_point()

#this can sometimes help make comparisons easier.
#altho it is not always the case.

# First explore the missingness structure of `oceanbuoys` using `vis_miss()`
vis_miss(oceanbuoys)

# Explore the distribution of `wind_ew` for the missingness of `air_temp_c_NA` using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew, 
             color = air_temp_c_NA)) + 
  geom_density()

# Explore the distribution of sea temperature for the missingness of humidity (humidity_NA) using  `geom_density()`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = sea_temp_c,
             color = humidity_NA)) + 
  geom_density()

# Explore the distribution of wind east west (`wind_ew`) for the missingness of air temperature using  `geom_density()` and facetting by the missingness of air temperature (`air_temp_c_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Build upon this visualisation by coloring by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = wind_ew,
             color = humidity_NA)) + 
  geom_density() + 
  facet_wrap(~air_temp_c_NA)

# Explore the distribution of wind east west (`wind_ew`) for the missingness of air temperature using  `geom_boxplot()`
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot()

# Build upon this visualisation by facetting by the missingness of humidity (`humidity_NA`).
oceanbuoys %>%
  bind_shadow() %>%
  ggplot(aes(x = air_temp_c_NA,
             y = wind_ew)) + 
  geom_boxplot() + 
  facet_wrap(~humidity_NA)

##visualizing missingness across two variables

#to explore the missings in a scatter plot,
#we use 'geom_miss_point()'
#(it's because R removes all missings in a scatter plot)

ggplot(airquality,
       aes(x = Ozone,
           y = Solar.R)) +
  geom_miss_point()

#how geom_miss_point() works?
#by using a special imputation technique which imputes 10% below the minimum value

##exercise
# Explore the missingness in wind and air temperature, and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point()

# Explore the missingness in humidity and air temperature, and display the missingness using `geom_miss_point()`
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point()

#Q. I think it takes a lot of time

# Explore the missingness in wind and air temperature, and display the missingness using `geom_miss_point()`. Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = wind_ew,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Explore the missingness in humidity and air temperature, and display the missingness using `geom_miss_point()` Facet by year to explore this further.
ggplot(oceanbuoys,
       aes(x = humidity,
           y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~year)

# Use geom_miss_point() and facet_wrap to explore how the missingness in wind_ew and air_temp_c is different for missingness of humidity
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_wrap(~humidity_NA)

# Use geom_miss_point() and facet_grid to explore how the missingness in wind_ew and air_temp_c is different for missingness of humidity AND by year - by using `facet_grid(humidity_NA ~ year)`
bind_shadow(oceanbuoys) %>%
  ggplot(aes(x = wind_ew,
             y = air_temp_c)) + 
  geom_miss_point() + 
  facet_grid(humidity_NA~year)


##filling in the blanks
#performing and tracking imputation

#using imputations to understand data structure
#visualising + exploring imputed values
#imputing data to explore missingness
#Track missing values
#visualise imputed values against data

#naniar
#geom_miss_point()
#The imputation here is
impute_below(c(10, 3, 5, NA, 9, 14))
#has some useful variations that give flexibility to apply it to some, or all variables of a data.

impute_below_if(data, is.numeric)
impute_below_at(data, vars(var1, var2))
impute_below_all(data)

#Tracking missing values
bind_shadow(df)

bind_shadow(df) %>%
  impute_below_all()

#visualise imputed values against data values using histogram
ag_imp <- airquality %>%
  bind_shadow() %>%
  impute_below_all()

ggplot(aq_imp,
       aes(x = Ozone,
           fill = Ozone_NA)) +
  geom_histogram()

#facets
ggplot(aq_imp,
       aes(x = Ozone,
           fill = Ozone_NA)) +
  geom_histogram() +
  facet_wrap(~Month)

#scatterplots
aq_impt <- airquality %>%
  bind_shadow() %>%
  add_label_missings() %>%
  impute_below_all()

ggplot(aq_imp,
       aes(x = Ozone,
           y = Solar.R,
           colour = any_missing)) +
  geom_point()


#We want to keep track of values we imputed.
#If we don't, it is very difficult to assess how good the imputed values are.

#This is a very useful way to help further explore missingness,
#and also provides the framework for imputing missing values.

##exercises
# Impute the oceanbuoys data below the range using `impute_below`.
ocean_imp <- impute_below_all(oceanbuoys)

# Visualise the new missing values
ggplot(ocean_imp, 
       aes(x = wind_ew, y = air_temp_c)) +  
  geom_point()

# Impute and track data with `bind_shadow`, `impute_below_all`, and `add_label_shadow`
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>%
  add_label_shadow()

# Look at the imputed values
ocean_imp_track

# Impute and track the missing values
ocean_imp_track <- bind_shadow(oceanbuoys) %>% 
  impute_below_all() %>% 
  add_label_shadow()

# Visualise the missingness in wind and air temperature, coloring missing air temp values with air_temp_c_NA
ggplot(ocean_imp_track, 
       aes(x = wind_ew, y = air_temp_c, color = air_temp_c_NA)) + 
  geom_point()

# Visualise humidity and air temp, coloring any missing cases using the variable any_missing
ggplot(ocean_imp_track, 
       aes(x = humidity, y = air_temp_c, color = any_missing)) +  
  geom_point()

##Note: it is faster than using geom_miss_point() since we impute first and use this imputated data for visualization.

# Explore the values of air_temp_c, visualising the amount of missings with `air_temp_c_NA`.
p <- ggplot(ocean_imp_track, aes(x = air_temp_c, fill = air_temp_c_NA)) +  geom_histogram()

# Expore the missings in humidity using humidity_NA
p2 <- ggplot(ocean_imp_track,  aes(x = humidity, fill = humidity_NA)) + geom_histogram()

# Explore the missings in air_temp_c according to year, using `facet_wrap(~year)`.
p + facet_wrap(~year)

# Explore the missings in humidity according to year, using `facet_wrap(~year)`.
p2 + facet_wrap(~year)

#we can apply the same priciples for imputations to assist with prediction

#What makes a good imputation
#understand good and bad imputations
#evaluate missing values: mean, scale, spread
#using visualisations: boxplots, scatterplots, histograms, many variables

#explore bad imputations: the mean
impute_mean(data$variable)
impute_mean_if(data, is.numeric)
impute_mean_at(data, var(v1, v2))
impute_mean_all(data)

aq_impute_mean <- airquality %>%
  bind_shadow(only_miss = TRUE) %>%
  impute_mean_all() %>%
  add_label_shadow()

#add_label_shadow()?
#adding a label to identify cases with missing observations

#One thing to keep in mind is to use the only miss option to bind only columns with missing values

#visualizing imputations using the boxplot
#The mean and 'median'?
ggplot(aq_impute_mean,
       aes(x = Ozone_NA,
           y = Ozone)) +
  geom_boxplot()

#when evaluating imputations, explore changes/similarities in
#The spread (scatterplot)
ggplot(aq_impute_mean,
       aes(x = Ozone,
           y = Solar.R,
           colour = any_missing)) +
  geom_point()

#exploring imputations for many variables
aq_imp <- airquality %>%
  bind_shadow() %>%
  impute_mean_all()

aq_imp_long <- shadow_long(aq_imp,
                           Ozone,
                           Solar.R)
#we enter in our data, followed by the variables that we want to focus on - in this case 'Ozone' and 'Solar'.
ggplot(aq_imp_long,
       aes(x = value,
           fill = value_NA)) +
  geom_histogram() +
  facet_wrap(~variable)

# Impute the mean value and track the imputations 
ocean_imp_mean <- bind_shadow(oceanbuoys) %>% 
  impute_mean_all() %>% 
  add_label_shadow()

# Explore the mean values in humidity in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = humidity_NA, y = humidity)) + 
  geom_boxplot()

# Explore the values in air temperature in the imputed dataset
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c_NA, y = air_temp_c)) + 
  geom_boxplot()


#the scale
#While the mean imputation might not look so bad when we compare it using a boxplot,
#it is important to get a sense of the variation in the data.
#This is why it is important to explore how the scale and spread of imputed values changes compared to the data.

# Explore imputations in air temperature and humidity, coloring by the variable, any_missing
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

# Explore imputations in air temperature and humidity, coloring by the variable, any_missing, and faceting by year
ggplot(ocean_imp_mean, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point() +  
  facet_wrap(~year)

#across many variables
#We have covered ways to look at individual variables or pairs of variables and their imputed values.
#However, sometimes you want to look at imputations for many variables.
#To do this, you need to perform some data munging and re-arranging.
#Shadow_long() gets the data into the right shape for these kinds of visualizations.

# Gather the imputed data 
ocean_imp_mean_gather <- shadow_long(ocean_imp_mean,
                                     humidity,
                                     air_temp_c)
# Inspect the data
ocean_imp_mean_gather

# Explore the imputations in a histogram 
ggplot(ocean_imp_mean_gather, 
       aes(x = value, fill = value_NA)) + 
  geom_histogram() + 
  facet_wrap(~variable)

#Q. Could it be used in mutivariate analysis?


##Performing imputations
#imputation using the simputation package
#use linear model to impute values with impute_lm

#Building a good imputation model is essential,
#but it is a complex topic
#there is as much to building a good imputation model
#as there is for building a good statistical model.


#simputation package
#assess new imputations
#build many imputation models
#compare imputations across different models and variables

#How imputing a linear model works
df %>%
  bind_shadow(only_miss = TRUE) %>%
  add_label_shadow() %>%
  impute_lm(y ~ x1 + x2)

aq_imp_lm <- airquality %>%
  bind_shadow() %>%
  add_label_shadow() %>%
  impute_lm(Solar.R ~ Wind + Temp + Month) %>%
  impute_lm(Ozone ~ Wind + Temp + Month)



#tracking missing values
#an important part of imputing data is using the 
#'bind_shadow()' & 'add_label_shadow()'

aq_imp_lm <-
  airquality %>%
  bind_shadow() %>%
  add_label_missings() %>%
  impute_lm(Solar.R ~ Wind + Temp + Month) %>%
  impute_lm(Ozone ~ Wind + Temp + Month)

ggplot(aq_imp_lm,
       aes(x = Solar.R,
           y = Ozone,
           colour = any_missing)) +
  geom_point()

#evaluating and comparing imputations

aq_imp_small <-
  airquality %>%
  bind_shadow() %>%
  impute_lm(Solar.R ~ Wind + Temp + Month) %>%
  impute_lm(Ozone ~ Wind + Temp + Month) %>%
  add_label_missings()

aq_imp_large <-
  airquality %>%
  bind_shadow() %>%
  impute_lm(Solar.R ~ Wind + Temp + Month + Day) %>%
  impute_lm(Ozone ~ Wind + Temp + Month + Day) %>%
  add_label_missings()

bound_models <- bind_rows(small = aq_imp_small,
                          large = aq_impt_large,
                          .id = "imp_model")

ggplot(bound_models,
       aes(x = Ozone,
           y = Solar.R,
           colour = any_missing)) +
  geom_point() +
  facet_wrap(~imp_model)

#in multiple variables and models?
bound_models_gather <- bound_models %>%
  select(Ozone, Solar.R,
         any_missing, imp_model) %>%
  gather(key = "variable", value = "value",
         -any_missing, -imp_model)

#make sure that we keep 'any_missing' and 'imp_model' outside of the gather.

ggplot(bound_models_gather,
       aes(x = imp_model,
           y = value)) +
  geom_boxplot() +
  facet_wrap(~key)

bound_models_gather %>%
  filter(any_missing == "Missing") %>%
  ggplot(aes(x = imp_model,
             y = value)) +
  geom_boxplot() +
  facet_wrap(~key)

##exercise
#simputation package, which provides a simple, powerful interface into performing imputations

# Impute humidity and air temperature using wind_ew and wind_ns, and track missing values
ocean_imp_lm_wind <- oceanbuoys %>% 
  bind_shadow() %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns) %>% 
  impute_lm(humidity ~ wind_ew + wind_ns) %>%
  add_label_shadow()

# Plot the imputed values for air_temp_c and humidity, colored by missingness
ggplot(ocean_imp_lm_wind, 
       aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point()

#when you build up an imputation model,
#it is a good idea to compare it to another model.

# Bind the models together 
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          .id = "imp_model")
bound_models
# Inspect the values of air_temp and humidity as a scatterplot
ggplot(bound_models, 
       aes(x = air_temp_c, 
           y = humidity, 
           color = any_missing)) +
  geom_point() + 
  facet_wrap(~imp_model)

#a better imputation.
#using more variables? (must be careful before considering a variable in the model)
# Build a model adding year to the outcome
ocean_imp_lm_wind_year <- bind_shadow(oceanbuoys) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year) %>%
  add_label_shadow()

# Bind the mean, lm_wind, and lm_wind_year models together
bound_models <- bind_rows(mean = ocean_imp_mean,
                          lm_wind = ocean_imp_lm_wind,
                          lm_wind_year = ocean_imp_lm_wind_year,
                          .id = "imp_model")

# Explore air_temp and humidity, coloring by any missings, and faceting by imputation model
ggplot(bound_models, aes(x = air_temp_c, y = humidity, color = any_missing)) + 
  geom_point() + facet_wrap(~imp_model)


##assessing inference from imputed data in a modelling context

#exploring parameters of one model
lm(Temp ~ Ozone + Solar.R + Wind + Month + day, data = airquality)
#1. complete case analysis
#2. Imputation using the imputed data from the last lesson.

#1.
aq_cc <- airquality %>%
  na.omit() %>%
  bind_shadow() %>%
  add_label_shadow()

#2.
aq_imp_lm <- bind_shadow(airquality) %>%
  add_label_shadow() %>%
  impute_lm(Ozone ~ Temp + Wind + Month + Day) %>%
  impute_lm(Solar.R ~ Temp + Wind + Month + Day)

#3. Bind the models together
bound_models <- bind_rows(cc = aq_cc,
                          imp_lm = aq_imp_lm,
                          .id = "imp_model")
#This prepares us for fitting our new models,
#so we can summarise and compare differences in the data

#exploring the models
model_summary <- bound_models %>%
  group_by(imp_model) %>%
  nest() %>%
  mutate(mod = Map(data,
                   ~lm(Temp ~ Ozone + Solar.R + Wind + Temp + Days + Month
                       data = .)),
         res = map(mod, residuals),
         pred = map(mod, predict),
         tidy = map(mod, broom::tidy))
#first we group by the imputation model, then nest the data
#This collapses, or nests, the data down into a neat format where each row is one of our datasets.
#THis allows us to create linear models on each row of the data, using 'mutate', and a special function 'map'.
#map tells the fucntion we are applying to look at the data.
#using 'tidy' function from 'broom', to provide nicely formatted coefficients from our linear model.

#exploring coefficients of multiple models
model_summary %>%
  select(imp_model,
         tidy) %>%
  unnest()

#exploring residuals of multiple models
model_summary %>%
  select(imp_model,
         res) %>%
  unnest() %>%
  ggplot(aes(x = res,
             fill = imp_model)) +
  geom_histogram(position = "dodge")

#and predictions
model_summary %>%
  select(imp_model,
         pred) %>%
  unnest() %>%
  ggplot(aes(x = pred,
             fill = imp_model)) +
  geom_histogram(position = "dodge")


#to evaluate the different imputation methods,
#we need to put them into a single dataframe.
#Next, you will compare three different approaches to handling missing data using the dataset, oceanbuoys.


# Create an imputed dataset using a linear models
ocean_imp_lm_all <- bind_shadow(oceanbuoys) %>%
  add_label_shadow() %>%
  impute_lm(sea_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(air_temp_c ~ wind_ew + wind_ns + year + latitude + longitude) %>%
  impute_lm(humidity ~ wind_ew + wind_ns + year + latitude + longitude)

# Bind the datasets
bound_models <- bind_rows(cc = ocean_cc,
                          imp_lm_wind = ocean_imp_lm_wind,
                          imp_lm_all = ocean_imp_lm_all,
                          .id = "imp_model")



# Create the model summary for each dataset
model_summary <- bound_models %>% 
  group_by(imp_model) %>%
  nest() %>%
  mutate(mod = map(data, ~lm(sea_temp_c ~ air_temp_c + humidity + year, data = .)),
         res = map(mod, residuals),
         pred = map(mod, predict),
         tidy = map(mod, broom::tidy))

# Explore the coefficients in the model
model_summary %>% 
  select(imp_model, tidy) %>% 
  unnest()
best_model <- "imp_lm_all"
#this is because the highest estimate is found in 'imp_lm_all'
