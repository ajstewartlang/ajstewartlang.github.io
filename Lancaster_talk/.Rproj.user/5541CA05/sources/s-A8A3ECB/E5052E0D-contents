library(tidyverse)
library(yarrr)
library(viridis)
library(psych)
library(ggrepel)

# Create data for 10,000 people - each with measures of Working Memory (WM), IQ, and 
# reading Comprehension (Comp)
set.seed(1234)
ID <- seq(1:10000)
WM <- as.integer(rnorm(10000, mean = 50, sd = 5))
IQ <- as.integer(rnorm(10000, mean = 100, sd = 15))
Comp <- as.integer(rnorm(10000, mean = 20, sd = 2))

data <- data.frame(ID, WM, IQ, Comp)

colnames(data) = c("ID", "WM", "IQ", "Comp")

# Create data for 48 participants (all present in data) taking part in an experiment
set.seed(1234)
ID <- sample(ID, 48)
Simple <- as.integer(rnorm(48, mean = 2000, sd = 140))
Complex <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- data.frame(ID, Simple, Complex)
colnames(dataRT) = c("ID", "Simple Sentence", "Complex Sentence")

# Combine the data anda dataRT datasets using the inner_join function
# Map this onto a new variable we're calling dataRT_all
dataRT_all <- inner_join(data, dataRT, by = "ID")

data_transformed <- mutate(dataRT_all, log_Simple = log(dataRT_all$`Simple Sentence`), 
                            log_Complex = log(dataRT$`Complex Sentence`))

filtered_data <- filter(data_transformed, ID != 2006)

data_long <- gather(dataRT, "Condition", "RT", c("Simple Sentence", "Complex Sentence"))
View(data_long)

data_wide <- spread(data_long, "Condition", "RT", c("Simple Sentence", "Complex Sentence"))
View(data_wide)

# Data Visualisation Section
# Use dplyr to get some summary statistics from the RT dataset using the pipe operator
data_long %>% group_by(Condition) %>% summarise(Mean = mean(RT), Min = min(RT), Max = max(RT), SD = sd(RT))
#data_long$Condition <- as.factor (data_long$Condition)
describeBy(data_long$RT, group = data_long$Condition)

#revalue one column capturing 2x2 and then splitting
#first create the data set - 24 items each with one RT measure for each of 4 conditions
Participant <- rep(1:24, each = 4)
Condition <- rep(1:4, times = 24)
set.seed(1234)
RT <- as.integer(rnorm (24*4, 1000,100))

data <- as.data.frame(cbind(Participant, Condition, RT))
data

#recode Condition columns follows:
#Condition 1 = Prime A, Target A
#Condition 2 = Prime A, Target B
#Condition 3 = Prime B, Target A
#Condition 4 = Prime B, Target B
data$Condition <- recode(data$Condition, "1" = "PrimeA_TargetA", "2" = "PrimeA_TargetB",
                          "3" = "PrimeB_TargetA", "4" = "PrimeB_TargetB")

#now separate the Condition column using "_" as our separator
data <- separate(data, col = "Condition", into = c("Prime", "Target"), sep = "_")
head(data)

#combine again
data <- unite(data, col = "Condition", c("Prime", "Target"), sep = "_")
wide_data <- spread(data, key = "Condition", value = "RT")
head(wide_data)

#or in tidyverse style
data %>% 
  separate(col = "Condition", into = c("Prime", "Target"), sep = "_") %>% 
  unite(col = "Condition", c("Prime", "Target"), sep = "_") %>%
  spread(key = "Condition", value = "RT")

# Bar Graph
data_summ <- data_long %>% group_by(Condition) %>% summarise(Mean = mean(RT), sd = sd(RT))
ggplot(data_summ, aes (x = Condition, y = Mean, group = Condition, 
                       fill = Condition, ymin = Mean - sd, ymax = Mean + sd)) + 
  geom_bar(stat = "identity", width = .5) + 
  geom_errorbar(width = .25) +  
  ggtitle("Bar chart with Error Bars") + 
  guides(fill = FALSE) 

# When boxplots can mislead
Subject <- seq(1:80)
Group <- factor(rep(1,80))
set.seed(1234)
RT <- c(rnorm(40,500,200), rnorm(40,1900,200))
data1 <- tibble(Subject, Group, RT)

ggplot(data1, aes(x = Group, y = RT)) + geom_boxplot()
ggplot(data1, aes(x = Group, y = RT)) + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data1, aes(x = Group, y = RT)) + geom_boxplot() + geom_jitter(size = 2, width = .1, alpha = .25)
ggplot(data1, aes(x = Group, y = RT)) + geom_violin() + geom_jitter(width = .1, alpha = .5)

# Violin Plot
ggplot(data_long, aes(x = Condition, y = RT, group = Condition, fill = Condition)) + 
  geom_violin() + 
  geom_jitter(alpha = .25, position = position_jitter(0.05)) + 
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black", size = 1) 

# Pirate plots
pirateplot(formula = RT ~ Condition, data = data_long, inf.method = "se", cex.axis = .75, theme = 1)
pirateplot(formula = RT ~ Condition, data = data_long, inf.method = "se", cex.axis = .75, theme = 4)

# Raincloud plot on data 
library(RColorBrewer)
library(plyr) #note, need to detach this after this plot as clashes with aspects of dplyr
source("https://gist.githubusercontent.com/ajstewartlang/6c4cd8ab9e0c27747424acdfb3b4cff6/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

set.seed(1234)
ID <- sample(ID, 48)
Simple <- as.integer(rnorm(48, mean = 2000, sd = 140))
Complex <- as.integer(rnorm(48, mean = 2400, sd = 160))

dataRT <- data.frame(ID, Simple, Complex)
colnames(dataRT) <- c("ID", "Simple Sentence", "Complex Sentence")

dataRT <- gather(dataRT, key = "Condition", value = "RT", c("Simple Sentence", "Complex Sentence"))

raincloud_theme = theme(
  text = element_text(size = 12),
  axis.title.x = element_text(size = 12),
  axis.title.y = element_text(size = 12),
  axis.text = element_text(size = 12),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=12),
  legend.text=element_text(size=12),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld <- ddply(dataRT, ~ Condition, summarise, mean = mean(RT), median = median(RT), 
               lower = lb(RT), upper = ub(RT))

ggplot(data = dataRT, aes(y = RT, x = Condition, fill = Condition)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, trim=FALSE) +
  geom_point(aes(y = RT, color = Condition), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1,  outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 3) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Accent") +
  scale_fill_brewer(palette = "Accent") +
  coord_flip() +
  theme_bw() +
  raincloud_theme +
  labs(x=NULL) +
  scale_y_continuous(breaks = seq(1500,3000,by = 200))

# Recreate data for 10,000 people - each with measures of Working Memory (WM), IQ, and 
# Reading Comprehension (Comp)
set.seed(1234)
ID <- seq(1:10000)
WM <- as.integer(rnorm(10000, mean = 50, sd = 5))
IQ <- as.integer(rnorm(10000, mean = 100, sd = 15))
Comp <- as.integer(rnorm(10000, mean = 20, sd = 2))

data <- data.frame(ID, WM, IQ, Comp)

colnames(data) <- c("ID", "WM", "IQ", "Comp")

# Scatterplots
ggplot (data, aes (x = data$WM, y = data$IQ)) + geom_point()
ggplot (data, aes (x = data$WM, y = data$IQ)) + geom_jitter(alpha = .1, position = position_jitter(0.5)) 
ggplot (data, aes (x = data$WM, y = data$IQ)) + geom_jitter(alpha = .1, position = position_jitter(0.5)) + geom_smooth(method = "lm")
ggplot (data, aes (x = data$WM, y = data$IQ)) + stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) + scale_fill_viridis() + coord_cartesian(expand = FALSE) 

# more plotting using built in data - this time the mpg dataset
detach("package:plyr", unload=TRUE) #need to detach as some clashes with dplyr

# find out about the dataset and generate some descriptives
head(mpg)
unique(mpg$manufacturer)
length(unique (mpg$manufacturer))
mpg %>% group_by(class) %>% summarise(Mean = mean(hwy))
mpg %>% group_by(cyl) %>% summarise(Mean = mean(hwy))

# build a violin plot with added descriptives
ggplot (mpg, aes (x = factor(cyl), y = cty, fill = factor (cyl))) + 
  geom_violin() +
  guides(colour = FALSE, fill = FALSE) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = .5) +
  xlab("Number of Cylinders") + 
  ylab("City Fuel Consumption (mpg)") +
  ggtitle ("City Fuel Consumption by Number of Cylinders")

# facet wrap by vehicle class with displacement instead of cylinder number
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() +
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class")

# now add a linear function to each
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "lm") + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class with Linear Regression Line")

# now with a non-linear function to each
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point() + 
  facet_wrap(~ class) +
  guides(colour = FALSE) + 
  geom_smooth(method = "loess") + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Each Vehicle Class with Non-Linear Regression Line")

# focus on subcompact cars and use geom_repel() to label
filtered <- filter(mpg, class == "subcompact")

# first without labels
ggplot(filtered, aes(displ, hwy, colour = class, label = model)) + 
  geom_point() +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Subcompact Cars")

ggplot(filtered, aes(displ, hwy, colour = model, label = model)) + 
  scale_colour_brewer(palette = "Paired") +
  geom_label_repel(label.padding = 0.25, label.size = .25, 
                                  min.segment.length = 100, size = 3.5) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Highway Fuel Consumption by Cylinder Displacement \nfor Subcompact Cars")

# plot basic histogram
ggplot(mpg, aes(displ)) + 
  geom_histogram(binwidth = .5) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement")

# facet by class and include background histogram of all data in each facet
mpg1 <- select (mpg, - class)
ggplot(mpg, aes(displ)) + 
  geom_histogram(data = mpg1, fill = "grey", binwidth = .5) +
  geom_histogram(binwidth = .5) +
  guides(colour = FALSE) + 
  facet_wrap(~ class)  + 
  xlab("Displacement (litres)") + 
  ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement for Each \nVehicle Class")

# plot histogram of displacement, coloured by class
ggplot(mpg, aes(displ, colour = class, fill = class)) + 
  geom_histogram(binwidth = .5) +
  guides(colour = FALSE) + 
  xlab("Displacement (litres)") + 
  ylab("Count") +
  ggtitle ("Histogram of Cylinder Displacement Coloured By \nVehicle Class")

ggplot(mpg, aes(cty)) + 
  geom_density(aes(fill = factor(class)), alpha = 0.8) + 
  labs(title = "City mpg Grouped by Vehicle Class",
       x = "City Fuel Consumption (mpg)",
       fill = "Vehicle Class")

# scatterplot with jitter 
ggplot(mpg, aes(displ, hwy)) + 
  geom_jitter(width = 0.05, alpha = .2, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement")

ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement Grouped by Class")

ggplot(mpg, aes(displ, hwy, colour = cyl)) + 
  geom_jitter(width = 0.05, alpha = .5, size = 4) + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Scatter Plot of Highway Fuel Consumption against \nEngine Displacement Grouped by Cylinder Number")

# 2d histogram with density heatmap
ggplot(mpg, aes(displ, hwy)) +
  stat_bin2d(bins = 10, colour = "black") + 
  scale_fill_viridis() + 
  xlab("Displacement (litres)") + 
  ylab("Highway Fuel Consumption (mpg)") +
  ggtitle ("Density Heat Map of Highway Fuel Consumption against \nEngine Displacement")

# Time series plots using the 'economics' dataset
# First let's get the structure of the dataset and the first 10 rows
str(economics)
economics

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = pop)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Population Size from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Total Population (thousands)", x = "Date")

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = psavert)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Savings Rate from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Savings Rate (%)", x = "Date")

ggplot(economics, aes(x = date)) + 
  geom_line(aes(y = unemploy)) + 
  labs(title = "Time Series Chart", 
       subtitle = "US Unemployement Rate from 'Economics' Dataset", 
       caption = "Source: Economics", 
       y = "Unemployment (thousands)", x = "Date")

# Animated plot - first we need to tidy the data and aggregate by Year
# Need to load the lubridate package to extract the year information
library(lubridate)

economics.tidy <- economics %>% 
  mutate (year = year(date)) %>% 
  group_by(year) %>%
  summarise (mean_pop = mean(pop), mean_un = mean(unemploy))

economics.tidy$mean_pop <- scale(economics.tidy$mean_pop)
economics.tidy$mean_un <- scale(economics.tidy$mean_un)

# first a static plot
data <- economics.tidy
ggplot(data, aes(mean_pop, mean_un)) + 
  ylim(-3, 3) + 
  xlim (-3, 3) +
  geom_point(size = 5, colour = "red") + 
  ggtitle("All Years") + 
  labs(x = "Population Size (Z-score)", y = "Unemployment Size (Z-score)")

# using gganimate
# devtools::install_github('thomasp85/gganimate')
library(gganimate)

ggplot(data, aes(mean_pop, mean_un)) + 
  ylim(-3, 3) + 
  xlim (-3, 3) +
  geom_point(size =5 , colour = "red") + 
  labs (title = "Year: {as.integer(frame_time)}", x="Population (Z-score)", y="Unemployment (Z-score)") +
  transition_time(year) + 
  theme(title = element_text(size = 15)) +
  ease_aes('linear')

# Animated plot using the 'gapminder' dataset
library(gapminder)

# gganimate code of life expectancy by GDP for each contintent over time
ggplot(gapminder, aes(gdpPercap, lifeExp, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Now vary each point according to population size
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = continent)) +
  geom_point(alpha = 0.7, show.legend = TRUE) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# gganimate code faceted by continent
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~ continent) +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Dot plot on life expectancy data
# Static faceted by year
df_Americas <- gapminder %>% filter(continent == "Americas")

ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) + 
  facet_wrap(~ year) + 
  theme(text = element_text(size=8))

# Dynamic - separate frame per year
df_Americas <- gapminder %>% filter(continent == "Americas")
ggplot(df_Americas, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(title = element_text(size = 15)) +
  transition_time(year) +
  ease_aes('linear')

# animation of all countries and life expectancy
df_all <- gapminder
ggplot(df_all, aes(x = lifeExp, y = fct_reorder(country, lifeExp))) +
  geom_point(color = "#0072B2", size = 3) +
  scale_x_continuous(name = "life expectancy (years)",
                     limits = c(30, 85),
                     expand = c(0, 0)) +
  scale_y_discrete(name = NULL, expand = c(0, 0.5)) +
  labs(title = 'Year: {frame_time}') + 
  theme(title = element_text(size=15)) +
  transition_time(year) +
  ease_aes('linear')




