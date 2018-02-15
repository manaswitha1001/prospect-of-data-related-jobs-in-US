#REQUIRED LIBRARIES
library(stringr)
library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
library(mongolite)


# Importing data scraped using IMPORT.IO in csv format
jobdata <- read.csv("crs.csv", header = TRUE, sep = "\t")
# Data Cleaning

# splitting the location into city and state
location <-
  str_split(as.vector(jobdata$location), ",", simplify = TRUE)
jobdata$city <- location[, 1]
jobdata$state <- location[, 2]
jobdata$state <- str_replace_all(jobdata$state, " ", "") 
# Eliminating the location column
jobdata <- jobdata[-4]
# parsing the median salary
temp <- gregexpr("[0-9]+", jobdata$Mediansal)
jobdata$medsalary <- as.numeric(regmatches(dad$Mediansal, temp))
# Removing rows with NULL in Salary
jobdata <- jobdata[!is.na(jobdata$medsalary), ]


#DATA STORAGE IN MONGODB
mongo_data <- mongo("cbn")
# Inserting data into mongo db
mongo_data$insert(jobdata)
#count the results
mongo_data$count
# Export the data into a txt file
mongo_data$export(file("cbn.txt"))


#DATA VISUALIZATION

#DISTRIBUTION OF JOBS ACROSS THE UNITED STATES

# Grouping by state and calculated average pay for each state using aggregate function.
avgsal <-
  mongo_data$aggregate('[{"$group":{"_id":"$state", "averagepay": {"$avg":"$parse"}}}]')
names(avgsal) <- c("state", "avgpay")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(data.frame(avgsal), locationmode = 'USA-states') %>%
  add_trace(
    z = ~ avgpay,
    text = ~ state,
    locations = ~ state,
    color = ~ avgpay,
    colors = 'Blues'
  ) %>%
  colorbar(title = "Average pay") %>%
  layout(geo = g)
p


#Top 5 Highly Hiring States in US

mostjobs <-
  mongo_data$aggregate('[{"$group":{"_id":"$state", "numberofjobs": {"$sum":1}}}]')
mosttop10 <- mostjobs %>% arrange(-numberofjobs) %>% top_n(5, numberofjobs)
names(mosttop10) <- c("state", "numberofjobs")

mosttop10 %>% mutate(st = fct_reorder(state, numberofjobs, .desc = TRUE)) %>%
  ggplot(aes(x = st, y = numberofjobs)) + geom_bar(stat = 'identity', fill = "#FF6666") +
  labs(title = "Top 5  Hiring States in US"
       , x ="State", y = "NumberofJobs")





#Top 5 Highly Paying States in US
mostsal <-
  mongo_data$aggregate('[{"$group":{"_id":"$state", "averagepay": {"$avg":"$parse"}}}]')
mostsaltop10 <- mostsal %>% arrange(-averagepay) %>% top_n(5, averagepay)
names(mostsaltop10) <- c("state", "averagepay")

mostsaltop10 %>% mutate(st = fct_reorder(state, averagepay, .desc = TRUE)) %>%
  ggplot(aes(x = st, y = averagepay)) + geom_bar(stat = 'identity', fill = "#FF0033") +
  labs(title = "Top 5 Highly Paid States in US"
       , x = "State", y = "Averagesalary")



#Top Paid Companies in US
mostcompany <-
  mongo_data$aggregate('[{"$group":{"_id":"$companyname", "pay": {"$max":"$parse"}}}]')
mosttopcompany <- mostcompany %>% arrange(-pay) %>% top_n(10, pay)
names(mosttopcompany) <- c("company", "pay")

mosttopcompany %>% mutate(company = fct_reorder(company, pay, .desc = TRUE)) %>%
  ggplot(aes(x = company, y = pay)) + geom_bar(stat = 'identity', fill = "#FF0066") +
  theme(axis.text.x = element_text(angle = 45, hjust = T)) +
  labs(title = "Top Paid Data Related Companies in US", x = "Company", y =
         "Average Annual Salary")
