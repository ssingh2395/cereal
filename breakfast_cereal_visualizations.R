library ("dplyr")
library ("readr")
library("gt")
library("ggplot2")


#PART 1 - IMPORT THE DATA 
cereal= read_csv("breakfastcereal.csv")

#PART 2- EXPLORE THE DATA 
#Print the first four (4) rows of data.
print("The first four rows of data:" )
slice_head(cereal, n = 4)

#Print the number of rows in the data.
rows = nrow(cereal)
print(sprintf( "There are %s rows of data", rows))

#Print the number of columns in the data.
columns = ncol(cereal)
print(columns)
print(sprintf( "There are %s columns of data", columns))

#Access the column names.

column_names = colnames(cereal)
#Iterate over the column names.
  for (name in column_names) {
    print(name)
  }
#Print each column name on its own line
i = 1
for (names in columb_names) {
  print(names)
  if (i >= 16) {
    break
  }
}

############### Part 3 ############ 
#Change the column names

cereal = cereal %>% rename (
  name = 'NAM',
  company = 'COM',
  type = 'TYP',
  calories = 'CAL',
  protein = 'PRO',
  sodium= 'SOD', 
  fiber= 'FIB',
  carbs = 'CAR',
  potassium = 'POT',
  vitamins= 'VIT',
  shelf = 'SHE',
  weight = 'WEI',
  cups= 'CUP',
  fat = 'FAT',
  sugar = 'SUG',
  rating= 'RAT'
)
View(cereal)

#Change the order of the columns
cereal = cereal %>% select (
  name,
  company,
  type,
  shelf,
  rating,
  weight,
  cups,
  calories,
  protein,
  sodium,
  fiber,
  carbs,
  potassium,
  vitamins,
  fat,
  sugar
)
glimpse(cereal)
#attributes(cereal)

#Modify the values in the company column

cereal_company = c (cereal$company)
#print(cereal_company)
# create vectors for levels and labels
compLevels = c ("A", "G", "K", "N", "P", "Q", "R")
compLabels = c ('American', 'GeneralMills', 'Kelloggs', 'Nabisco', 'Post', 'QuakerOats', 'RalstonPurina')
# create the factor with levels and labels
factor (cereal_company, levels = compLevels, labels = compLabels)
# reassign the factor to the original variable
cereal_company = factor (cereal_company, levels = compLevels, labels = compLabels)

cereal= cereal %>% mutate (company, cereal_company = cereal_company) 
cereal = cereal %>% select(
    name,
    type,
    shelf,
    rating,
    weight,
    cups,
    calories,
    protein,
    sodium,
    fiber,
    carbs,
    potassium,
    vitamins,
    fat,
    sugar,
    cereal_company
) %>%  
  rename (company = cereal_company)
glimpse(cereal)

#Modify the values in the type column From "H" to "Hot" From "C" to "Cold"
cereal_types = c (cereal$type)
print(cereal_types)
typeLevels = c ("C", "H")
typeLabels = c ('Cold', 'Hot')
cereal_types = factor (cereal_types, levels = typeLevels, labels = typeLabels)
cereal = cereal %>% mutate (cereal_types = cereal_types)
cereal = cereal %>% select(
  name,
  company, 
  cereal_types,
  shelf,
  rating,
  weight,
  cups,
  calories,
  protein,
  sodium,
  fiber,
  carbs,
  potassium,
  vitamins,
  fat,
  sugar
) %>%  
  rename (type = cereal_types)


cereal_shelf = c (cereal$shelf)
print(cereal_shelf)
shelfLevels = c (1, 2, 3)
shelfLabels = c ('Top Shelf', 'Eye Level', 'Bottom Shelf')
factor (cereal_shelf, levels = shelfLevels, labels = shelfLabels)
cereal_shelf= factor (cereal_shelf, levels = shelfLevels, labels = shelfLabels)
cereal = cereal %>% 
  mutate (cereal_shelf = cereal_shelf) %>% 
  select(
    name,
    company, 
    type,
    cereal_shelf,
    rating,
    weight,
    cups,
    calories,
    protein,
    sodium,
    fiber,
    carbs,
    potassium,
    vitamins,
    fat,
    sugar
  ) %>%  
  rename (shelf = cereal_shelf)
glimpse(cereal)



#caloriesPerCup is equal to the number 1 (for one serving) divided by the number of cups per serving times the number of calories per serving
cereal = cereal %>% mutate (caloriesPerCup = 1/ cups * calories)
View(cereal)
cereal = cereal %>%  arrange (desc ( caloriesPerCup))
View (cereal %>% slice_head (n = 10))


############ PART 4 ANALYZE THE DATA #############


dataFrame1 = group_by (cereal, type)
print(dataFrame1)
dataFrame2 = select (dataFrame1, caloriesPerCup)
# Calculate the average height and mass for each species group
dataFrame3 = summarize (
  dataFrame2,
  AVRcaloriesPerCup = mean (caloriesPerCup, na.rm = TRUE),
)
print( "There average calories per cup of cold cereal and hot cereal are:")

print(dataFrame3)

print("The average consumer reports rating is: ")
cereal %>%
  summarize (AVRrating = mean (rating, na.rm = TRUE))


#"Most Sugary Cereals": The ten (10) cold cereals with the most sugar per serving (from highest to lowest).
#"Lowest-Rated Cereals": The ten (10) cold cereals with the lowest Consumer Reports rating (from lowest to highest).
#"Cereals with No Nutritional Value": The ten (10) cold cereals with 0% FDA recommended vitamins and minerals.

cereal %>% 
  filter (type == "Cold") %>% 
  select (company, name, sugar) %>%
  arrange(desc(sugar)) %>% 
  slice_head(n = 10) %>% 
  gt()

cereal %>% 
  filter (type == "Cold") %>% 
  select (company, name, rating) %>%
  arrange(rating) %>% 
  slice_head(n = 10) %>% 
  gt()


cereal %>% 
  filter (type == "Cold") %>% 
  select (company, name, vitamins) %>%
  arrange(vitamins) %>% 
  filter (vitamins == "0") %>% 
  gt()


############# GGPLOT ANALYSIS ###################

#Create a scatterplot (using ggplot2 with geom_point) of Grams of Sugar Per Serving (from lowest to highest) vs. Consumer Reports Rating (from lowest to highest).
g1 = cereal %>% ggplot (
  aes (x = sugar, y = rating)
) +
  # add a scatterplot
  geom_point () +
  geom_smooth (method = "lm") +
  labs (title = "Grams of Sugar per Serving vs. Consumer ReportsnRatings", subtitle = "Cereal Dataset", x =
          "Sugar per Gram", y = "Ratings")
print(g1)


#Create a histogram (using ggplot2 with geom_histogram) with 15 bins using rating (Consumer Reports Rating) as the continuous variable
cereal %>% ggplot () +
  labs (title = "Number of Cereals by Reporting Rating", x = "Consumer Reports Rating", y = "Number of Cereals") +
  #add a histogram
  geom_histogram (
    mapping = aes (x = rating),
    bins = 15,
    fill = "blue"
  )


#Create a bar chart (using ggplot2 with geom_bar) with shelfName on the x-axis and thenumber of cereals for that shelfName on the y-axis. Filter the data to only include cerealswith a rating above the average value you determined in Part 4 

#one option 
#cereal %>% summarize (AVRrating = mean (rating, na.rm = TRUE))

#find the info you have to graph- this is the analysis 
cereal %>%
  select (name, shelf, rating) %>% 
  filter (rating >= mean (rating, na.rm = TRUE)) %>%
  count(shelf) %>% 
  #Plot the analysis 
  ggplot () +
  labs (title = "Number of Cereals with Above Average Rating by Supermarket Shelf Placement ", subtitle = "Cereal Dataset", x =
          "Supermarket Shelf Placement", y = "Number of Cereals") +
  geom_bar (
    mapping = aes (x = shelf, y = n),
    color = "lightblue3",
    fill = "white",
    stat = "identity", 
  )  


#Create a single boxplot (using ggplot2 with geom_boxplot) for caloriesPerCup
cereal %>% ggplot () +
  labs (title = "Box Plot for Cereal Calories Per Cup ", y = "Calories per Cup") +
  geom_boxplot (
    mapping = aes (y = caloriesPerCup)
  )


