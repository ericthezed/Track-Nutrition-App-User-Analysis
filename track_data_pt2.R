# Load packages

library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
library(reshape)


# Import data

users <- read.csv('users.csv', stringsAsFactors = FALSE)
food <- read.csv('users_food_logs_pt2.csv', header = FALSE, stringsAsFactors = FALSE)


# Name food columns

colnames(food) <- c('user_id', 'calories', 'fat_g', 'carbs_g', 'protein_g', 'consumed_at', 'created_at', 'upc', 'ndb_no', 'meal')

# Format date fields

users$signup <- as.POSIXct(users$created_at, format = '%Y-%m-%d %H:%M:%S')

food$login <- as.POSIXct(food$created_at, format = '%Y-%m-%d %H:%M:%S')
food$timestamp <- as.POSIXct(food$consumed_at, format = '%Y-%m-%d %H:%M:%S')

# Convert character fields to numeric

users$daily_carbs_pct <- as.numeric(users$daily_carbs_pct)
users$daily_fat_pct <- as.numeric(users$daily_fat_pct)
users$daily_protein_pct <- as.numeric(users$daily_protein_pct)
users$birth_year <- as.numeric(users$birth_year)
users$height_cm <- as.numeric(users$height_cm)
users$weight_kg <- as.numeric(users$weight_kg)

food$calories <- as.numeric(food$calories)
food$fat_g <- as.numeric(food$fat_g)
food$carbs_g <- as.numeric(food$carbs_g)
food$protein_g <- as.numeric(food$protein_g)


# Remove unneeded fields

users <- subset(users, select = -c(created_at, daily_carbs_pct, daily_fat_pct, daily_protein_pct))

food <- subset(food, select = -c(created_at, consumed_at))


# Remove duplicate rows

users <- unique(users)
food <- unique(food)


# Remove outliers

outlier_high <- function(x) {quantile(x, .75, na.rm = TRUE) + 1.5*(quantile(x, .75, na.rm = TRUE) 
                                                                   - quantile(x, .25, na.rm = TRUE))}
outlier_low <- function(x) {quantile(x, .25, na.rm = TRUE) - 1.5*(quantile(x, .75, na.rm = TRUE) 
                                                                  - quantile(x, .25, na.rm = TRUE))}

users$daily_kcal[users$daily_kcal > outlier_high(users$daily_kcal) | users$daily_kcal < outlier_low(users$daily_kcal)] <- NA
users$weight_kg[users$weight_kg > outlier_high(users$weight_kg) | users$weight_kg < outlier_low(users$weight_kg)] <- NA
users$height_cm[users$height_cm > outlier_high(users$height_cm) | users$height_cm < outlier_low(users$height_cm)] <- NA
users$birth_year[users$birth_year > 2017 | users$birth_year < 1900] <- NA
users$gender[users$gender != 'male' & users$gender != 'female'] <- NA
users$signup[users$signup == '1969-12-31 08:00:00'] <- NA

food$timestamp[food$timestamp == '1969-12-31 08:00:00'] <- NA
food$login[food$login == '1969-12-31 08:00:00'] <- NA


# Convert gender to factor

users$gender <- factor(users$gender, levels = c('male','female'), 
                       labels = c('male', 'female'))

# Create age factor

users$age <- as.numeric(format(Sys.Date(), "%Y")) - users$birth_year
users$age_group <- ifelse(users$age < 18,1,
                          ifelse(users$age >= 18 & users$age < 25,2,
                                 ifelse(users$age >= 25 & users$age < 35,3,
                                        ifelse(users$age >= 35 & users$age < 45,4,
                                               ifelse(users$age >= 45 & users$age < 55,5,
                                                      ifelse(users$age >= 55 & users$age < 65,6,
                                                             ifelse(users$age >= 65,7,NA)))))))
users$age_group <- factor(users$age_group, levels = c(1,2,3,4,5,6,7), 
                          labels = c('Under 18', '18-24', '25-34','35-44','45-54','55-64','65 and Over'))

# Create meal type factor

food$meal_type <- revalue(food$meal, c('1'='1', '2' = '0', '3' = '2', '4' = '0', '5' = '3', '6' = '0'))
food$meal_type <- factor(food$meal_type, levels = c(0,1,2,3), 
                         labels = c('Snack', 'Breakfast', 'Lunch', 'Dinner'))
  
# Sort data frames

users <- users[order(users$user_id, users$signup), ]
food <- food[order(food$user_id, food$login), ]


# Look at distribution of logins per day

food$login_date <- as.Date(food$login, tz='')
food$login_time <- format(as.POSIXct(strptime(food$login, '%Y-%m-%d %H:%M:%S', tz="")), format = '%H:%M:%S')
food$login_hour <- format(floor_date(as.POSIXct(food$login_time, tz = '', format = '%H:%M:%S', usetz = FALSE), 
                                     unit = 'hours'), format = '%H:%M')
food$login_hour_num <- as.numeric(format(floor_date(as.POSIXct(food$login_time, headtz = '', format = '%H:%M:%S', usetz = FALSE), 
                                         unit = 'hours'), format = '%H'))
food$login_minute <- format(floor_date(as.POSIXct(food$login_time, tz = '', format = '%H:%M:%S', usetz = FALSE), 
                                     unit = 'minutes'), format = '%H:%M')

daily_logins <- ddply(food, c('user_id', 'login_date'), summarise, 
                      num_logins_h = length(unique(login_hour)),
                      num_logins_m = length(unique(login_minute)))

daily_logins$num_logins_hour <- ifelse(daily_logins$num_logins_h > 10, 10, daily_logins$num_logins_h)
daily_logins$num_logins_min <- ifelse(daily_logins$num_logins_m > 20, 20, daily_logins$num_logins_m)

median(daily_logins$num_logins_h)
median(daily_logins$num_logins_m)

# Save files

write.csv(food, file = 'food_logins.csv')
write.csv(daily_logins, file = 'daily_logins.csv')


# Create density plot of login frequency by hour

plot_logins_hour <- ggplot(subset(food, !is.na(login_hour_num)), aes(x = login_hour_num)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = 'blue', col = 'black') +
  scale_x_continuous(breaks = seq(0,23,1), lim = c(-1,24)) +
  xlab("Hour of Day") + ylab("Proportion of Logins") + 
  ggtitle("Density Plot of Login Frequency by Hour") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=11))
plot_logins_hour


# Create density plot of number of logins per day

plot_logins_day_h <- ggplot(subset(daily_logins, !is.na(num_logins_hour)), aes(x = num_logins_hour)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = 'blue', col = 'black') +
  scale_x_continuous(breaks = seq(1,10,1), lim = c(0,11), labels = c(seq(1,9, by = 1), '10+')) +
  xlab("Number of Daily Logins") + ylab("Proportion of Days") + 
  ggtitle("Density Plot of Logins per Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=11))
plot_logins_day_h

plot_logins_day_m <- ggplot(subset(daily_logins, !is.na(num_logins_min)), aes(x = num_logins_min)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = 'blue', col = 'black') +
  scale_x_continuous(breaks = seq(1,20,1), lim = c(0,21), labels = c(seq(1,19, by = 1), '20+')) +
  xlab("Number of Daily Logins") + ylab("Proportion of Days") + 
  ggtitle("Density Plot of Logins per Day") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(text = element_text(size=11))
plot_logins_day_m


# Classify foods into categories

food$upc[food$upc == 'NULL'] <- NA
food$ndb_no[food$ndb_no == 'NULL'] <- NA

food$food_type <- ifelse(!is.na(food$upc), 1, ifelse(!is.na(food$ndb_no), 2, 3))

food$food_type <- factor(food$food_type, levels = c(1,2,3), 
                         labels = c('Grocery', 'Common', 'Restaurant'))


# Get percentage of each type of food

round(100*(table(food$food_type)/sum(table(food$food_type))),2)


# Get percentage of each type of food by age

food_age <- merge(food, users, on='user_id', type='inner', all=TRUE)

food_type_by_age <- round(prop.table(table(food_age$age_group, food_age$food_type), 1)*100,1)
food_type_by_age


# Plot percentage of each type of food by age

food_type_by_age_tidy <- as.data.frame(food_type_by_age)
colnames(food_type_by_age_tidy) <- c('age', 'food_type', 'percent')

plot_food_age <- ggplot(food_type_by_age_tidy, aes(x = age, y = percent, col = food_type, group = food_type)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + 
  geom_point(size = 2, stat = 'summary', fun.y = 'mean') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=percent), vjust=-1.0, size = 4, col = 'blue') +
  xlab('Age') + ylab('Frequency of Food Type') + 
  ggtitle('Frequency of Food Type By Age') +
  theme(text = element_text(size=10)) +
  scale_y_continuous(breaks = seq(0,80,10), lim = c(0,80))
plot_food_age

write.csv(food_type_by_age_tidy, file='food_by_age.csv')


# Detemine whether difference in food type by age

chisq.test(table(food_age$age_group, food_age$food_type))



# Compute macro nutrient calories

food$fat_cal <- food$fat_g*9
food$carbs_cal <- food$carbs_g*4
food$protein_cal <- food$protein_g*4
food$total_cal <- food$fat_cal + food$carbs_cal + food$protein_cal


# Compute percentage of calories for each macro nutrient by meal type

mn_by_meal <- ddply(food, c('user_id', 'timestamp', 'meal_type'), summarise, 
                    pct_fat = 100*(sum(fat_cal)/sum(total_cal)),
                    pct_carbs = 100*(sum(carbs_cal)/sum(total_cal)),
                    pct_protein = 100*(sum(protein_cal)/sum(total_cal)),
                    calories = sum(calories))

mn_by_meal_tidy <- melt(mn_by_meal, id=c('user_id', 'timestamp', 'meal_type', 'calories'))
colnames(mn_by_meal_tidy) <- c('user_id', 'timestamp', 'meal_type', 'calories', 'macro_nutrient', 'percent')
mn_by_meal_tidy$macro_nutrient <- revalue(mn_by_meal_tidy$macro_nutrient, 
                                          c('pct_fat' = 'fat', 'pct_carbs' = 'carbs', 'pct_protein' = 'protein'))

mn_by_meal_groups <- merge(mn_by_meal, users, on='user_id', type='inner', all=TRUE)

# Remove outliers

# mn_by_meal_groups <- mn_by_meal_groups[!(mn_by_meal_groups$calories > outlier_high(mn_by_meal_groups$calories)), ]


# Save files

write.csv(mn_by_meal_tidy, file = 'macronutrients_by_meal.csv')
write.csv(mn_by_meal, file = 'calories_by_meal.csv')
write.csv(mn_by_meal_groups, file = 'calories_by_meal_age_gender.csv')


# Determine if calorie consumption and macro-nutrient composition differ by meal type

mean_mn_by_meal <- ddply(mn_by_meal_tidy, c('meal_type', 'macro_nutrient'), summarise, mean = mean(percent, na.rm = TRUE))
mean_mn_by_meal

aov_mn_by_meal <- aov(percent ~ meal_type*macro_nutrient, data = mn_by_meal_tidy)
summary(aov_mn_by_meal)
TukeyHSD(aov_mn_by_meal)


mean_cal_by_meal <- ddply(mn_by_meal, c('meal_type'), summarise, mean_cal = mean(calories, na.rm = TRUE))
mean_cal_by_meal

mean_cal_by_meal_pct <- ddply(mn_by_meal, c('meal_type'), summarise, 
                              pct_cal = 100*mean(calories, na.rm = TRUE)/sum(subset(mean_cal_by_meal$mean_cal, !is.na(mean_cal_by_meal$meal_type))))
mean_cal_by_meal_pct

aov_cal_by_meal <- aov(calories ~ meal_type, data = mn_by_meal)
summary(aov_cal_by_meal)
TukeyHSD(aov_cal_by_meal)

mean_cal_by_meal_groups <- ddply(mn_by_meal_groups, c('meal_type', 'gender', 'age_group'), 
                                 summarise, mean_cal = mean(calories, na.rm = TRUE))
mean_cal_by_meal_groups

aov_cal_by_meal_groups <- aov(calories ~ meal_type*gender*age_group, data = mn_by_meal_groups)
summary(aov_cal_by_meal_groups)
TukeyHSD(aov_cal_by_meal_groups)


# Plot macro nutrient composition by meal

plot_mn_by_meal <- ggplot(subset(mn_by_meal_tidy, !is.na(meal_type)), aes(x = meal_type, y = percent, col = macro_nutrient, group = macro_nutrient)) + 
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + 
  geom_point(size = 2, stat = 'summary', fun.y = 'mean') +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=percent), vjust=-1.0, size = 4, col = 'blue') +
  xlab('Meal') + ylab('Percentage of Calories') + 
  ggtitle('Percentage of Calories by Macro-Nutrient Type Across Meals') +
  theme(text = element_text(size=10)) +
  scale_y_continuous(breaks = seq(0,60,10), lim = c(0,60))
plot_mn_by_meal


# Plot average calories by meal

cal_by_meal_ci <- ddply(mn_by_meal, c('meal_type'), summarise, 
                             mean = mean(calories, na.rm = TRUE),
                             ci = 1.96*(sd(calories)/sqrt(length(calories))))
cal_by_meal_ci <- transform(cal_by_meal_ci, upper=mean+ci, lower=mean-ci)


plot_cal_by_meal <- ggplot(subset(mn_by_meal, !is.na(meal_type)), aes(x = meal_type, y = calories)) +
  geom_line(size = 1, stat = 'summary', fun.y = 'mean') + 
  geom_point(size = 2, stat = 'summary', fun.y = 'mean') +
  geom_errorbar(aes(ymax=upper, ymin=lower), position = position_dodge(0.9), width = 0.5, data = cal_by_meal_ci) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label=calories), vjust=-1.0, size = 4, col = 'blue') +
  xlab('Meal') + ylab('Average Calorie Consumption') + 
  ggtitle('Average Calorie Consumption by Meal Type') +
  theme(text = element_text(size=10)) +
  scale_y_continuous(breaks = seq(200,500,50), lim = c(200,500))
plot_cal_by_meal

