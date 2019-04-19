
"
Use data files: 
  cars04.csv
comics.csv
immigration.csv
life_exp_raw.csv
us_income_raw.csv
names.txt

"

library(readr)
library(ggplot2)
library(dplyr)

# Read the csv files
comics = read.csv("C:/shobha/R/DataCamp/dataFiles/CSV-files/comics.csv")
cars = read.csv("C:/shobha/R/DataCamp/dataFiles/CSV-files/cars04.csv")
life = read.csv("C:/shobha/R/DataCamp/dataFiles/CSV-files/life_exp_raw.csv")



#Save them as explore_datasets.RData
#Save and restore multiple R objects: 
#save(data1, data2, file = "my_data.RData")
#load("my_data.RData")

path = "C:/Users/shobha/Documents/RExploratyDataAnalysis"

save(comics, cars, life, file = paste0(path,"/RDataObjects/explore_datasets.RData"))

rm(comics)
rm(cars)
rm(life)

#Now load the datasets 
load(paste0(path,"/RDataObjects/explore_datasets.RData"))
glimpse(comics)
glimpse(cars)
glimpse(life)


# Start with analysing comics dataset
levels(comics$align)

unique(comics$id)

levels(comics$id)

"Note: NAs ignored by levels() function"

# To see combination of levels for two
# categorical variable use contingency table


table(comics$id, comics$align)


"
The output tells us the most common category had a count 
of 4493 characters with Secret identities.
"

# While tables of counts are useful you can get a bigger 
# picture by translating these counts into graphics

"
x-axis factor variable identities, so bar plot
"
ggplot(comics, aes(x = id, fill = align)) +
  geom_bar()

# Contingency table review
"
comics dataset is collection of characteristics on all the superheroes
created by Marvel and DC comics in the last 80 years.

Let's start by creating a contingency table 
which is a useful way to represent the total counts of observations that fall
into each combination of the levels of categorical variables.
"

head(comics,2)

levels(comics$align)

levels(comics$gender)

table(comics$align, comics$gender)

"
Dropping levels
The contingency table from the last exercise revealed that there are some
levels that have very low counts.
To simplify the analysis, it often helps to drop such levels.

In R, this requires two steps:
- first filtering out any rows with the levels that have very low counts
- then removing these levels from the factor variable with droplevels()
- this is because the droplevels() function would keep levels that 
have just 1 or 2 counts
- it only drops levels that don't exist in a dataset
"
table(comics$align, comics$gender)

"
Remove 'Reformed Criminals' and also drop that level
"
comics = comics %>% 
  filter(align != 'Reformed Criminals') %>%
  droplevels()


table(comics$align, comics$gender)


# Side-by-side barcharts for two categorical variables

"
While a contingency table represents the counts numerically
it's often more useful to represent them graphically.

Here you'll construct two side-by-side barcharts of the comics
data.
This shows that there can often be two or more options for
presenting the same data.
Passing the argument position = 'dodge' to geom_bar()
says that you want a side-by-side (i.e. not stacked) barchart.

side-by-sie indicates separate bar for each category
{ y axis has the frequency }
"

"
Create a side-by-side barchart with align on the x-axis and 
gender as the fill aesthetic.
"
# Side-by-side barchart for male female characters
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = "dodge")


"
Create another side-by-side barchart with gender on the x-axis 
and align as the fill aesthetic. Rotate the axis labels 90 degrees 
to help readability.
"
# Side-by-side barchart for gender based characters
ggplot(comics, aes(x = gender, fill = align)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))


"
Counts vs. proportions: From counts to proportions
"

# Simplify display format
options(scipen = 999, digits = 3)


tab_cnt = table(comics$id, comics$align)

tab_cnt

prop.table(tab_cnt)


sum(prop.table(tab_cnt))


# Conditional proportions : condition on the rows (i.e. rows sum to 1)
prop.table(tab_cnt, 1)

"
interpretation: 
so we can see that 57 % of all secret characters are Bad
31% are good and 12% are neutral
"

# condition on the columns: i.e. column sum to 1

prop.table(tab_cnt, 2)

"
Now columns sum to 1 and we learned 63 % of Bad characters
secret
"

"
Proportions in graphs is like scaling instead of side-by-side bars for each category
we fill the bar for the category and scale the y-axis frequency/count to proportion 0-1
all bars will be same height as we are comparing proportion of fill for the variable
on x axis
"
ggplot(comics, aes(x = id, fill = align)) +
  geom_bar(position = "fill") +
  ylab("proportion")


ggplot(comics, aes(x = align, fill = id)) +
  geom_bar(position = "fill") +
  ylab("proportion")


# Approximately what proportion of all female characters are good

tab = table(comics$align, comics$gender)
options(scipen = 999, digits = 3) # Print fewer digits

prop.table(tab) #Joint proportions

"proportion based on column, that is gender, that is column total should be 1"
prop.table(tab,2)


# 51%

"
Nice! To answer this question, you needed to look at how align was 
distributed within each gender. That is, you wanted to condition on 
the gender variable.

Note: in proportion all bars are same height and fill category
corresponds to the proportion 
"


"
Counts vs. proportions
Bars charts can tell dramatically different stories depending on whether
they represent counts or proportions and, if proportions, what the proportions
are conditioned on.
"

# Create a stacked barchart of gender counts with align on the x-axis
ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar()

"
Create a stacked barchart of gender proportions with align 
on the x-axis by setting the position argument to geom_bar() 
equal to 'fill'.
"

ggplot(comics, aes(x = align, fill = gender)) +
  geom_bar(position = 'fill') +
  ylab("proportion")

"
Conclusion:
Excellent work! By adding position = 'fill' to geom_bar(), 
you are saying you want the bars to fill the entire height 
of the plotting window, thus displaying proportions and not raw counts.
"

"
Distribution of one variable : Marginal distribution
"
table(comics$id)

tab_cnt

"
This is like summing up the row of single row of contingency table 
474 + 647 + 390 = 1511
Since we have summed over the margins of other variables
this is sometimes known as marginal distribution
"

# plot the distribution of id, x-axis factor variable so bar chart
ggplot(comics, aes(x = id)) +
  geom_bar()

# distribution of id for all neutral characters

"
We could filter the dataset to include only neutral characters
or we could use a technique called faceting 
Faceting breaks the data into subsets based on a categorical variable
and then constructs a plot for each
facet_wrap(~align) - ~ implying broken down by align, that is character
"

ggplot(comics, aes(x=id)) +
  geom_bar() +
  facet_wrap(~align)


"
The result is three simple barcharts side-by-side
- one for each of alignments/characters viz. Bad Good Neutral
"

#Faceting vs. stacking

"
Facet - one chart for each category
stacking - one bar with proportion of the categories
"

#Pie chart vs. bar chart

"
In pie charts it is not easy to determine the proportional differences 
if sections sizes don't differ too much, with bars difference is clear
hence barcharts are preferrable.
"

# Marginal barchart

"
If you are interested in the distribution of alignment of all 
superheroes, it makes sense to construct a barchart for just that single
variable.

You can improve the interpretability of the plot, though, by implementing
some sensible ordering.
Superheroes that are 'Neutral' shown an alignment between Good and Bad
so it makes sense to put that bar in the middle.
"
str(comics$align)

# Change the order of the levels in align
comics$align = factor(comics$align, levels = c("Bad", "Neutral", "Good"))

str(comics$align)


levels(comics$align)


# Create a simple barchart plot of align
ggplot(comics, aes(x = align)) +
  geom_bar()


#Conditional barchart

"
Conditional barchart - {based on other categorical variable - faceting}

Now if you want to break down the distribution of alignment based on 
gender, you're looking for conditional distribution.

You could make these by creating multiple filtered datasets (on for each gender)
or by faceting the plot of alignment based on gender.
"

#Create a barchart of align faceted by gender
"
plot will be broken down into as many categories for gender variable
each plot will be a simple barchart for align variable for 
corresponding gender variable
remember to use ~
"

ggplot(comics, aes(x = align)) +
  geom_bar() +
  facet_wrap(~ gender)




"
Improve piechart
The piechart is a very common way to represent the distribution of 
a single categorical variable, but they can be more difficult to
interpret than barcharts.

This is a piechart of a dataset called pies that contains the favourite
pie flavors of 98 people. 
Improve the representation of these data by constructing a barchart
that is ordered in descending order of count.
"

# Exploring numerical dataset cars
glimpse(cars)

# Dotplot vs Histogram vs Density plot vs Boxplot

# Dotplot 

"
Most common way to plot numerical data is dotplot
where each case is plotted as a dot on the x-axis
"
ggplot(cars, aes(x = weight)) +
  geom_dotplot(dotsize = 0.4)


# Histogram
"
In Dotplot there is zero information loss and the dataset 
can be rebuilt from it, but it gets difficult to read dotplots 
as the number of cases gets very large.
To solve this problem use histograms which aggregate the plot into bins 
on the x-axis and height of the bin corresponding to the number of cases
that fall in that bin
"
ggplot(cars, aes(x = weight)) +
  geom_histogram()

# Density Plot

"
Due to the binning it is not possible to perfectly reconstruct the dataset from
histograms but what we gain is the bigger picture of the distribution of the data
If the step-wise structure of the histogram irks you then you will like the 
Density plot
Density plot represents the shape of the Histogram using a smooth line 
- this provides an even bigger picture representation of the shape of the 
distribution
"

ggplot(cars, aes(x = weight)) +
  geom_density()

# Boxplot

"
So you will only want to use density plot when you have a large number of cases.
If you would want a more abstracted sense of distribution then you could identify 
the center of the distribution the values that markoff the middle half of the data and 
the values that markoff the vast majority of the data these values can then be used to
construct a boxpolt .
Where the box represents the central bulk of the data the whiskers contain almost all 
of the data and the extreme values are represented as points.
"

"
x-axis numerical variable weight, boxplot shows summary stats along
y-axis so set x to 1
"
ggplot(cars, aes(x=1, y = weight)) +
  geom_boxplot() 


ggplot(cars, aes(x=1, y = weight)) +
  geom_boxplot() +
  coord_flip()


str(cars$hwy_mpg)

# Distribution of highway mileage on the condition of whether or not the car
#is a pickup truck

# hwy_mpg is a numerical variable so histogram, faceted by pickup
ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_wrap(~pickup)

"
We get a warning that there are 14 missing values

Faceted histogram plot is informative
- it tells us that there are many more non pickups than pick ups
- median 26 vs median 20 shows that non-pickup get much better mileage
- also non-pickups have more variability than the pickups
- keep an eye on the typical observation and the variability of 
the distribution as you practise exploring numberical data
"

# Faceted histogram
"
In this chapter you'll be working with the cars dataset which records
characteristics on all of the new models of cars for sale in the US in 
a certain year.
You will investigate the distribution of mileage across a categorical
variable, but before you get there, you'll want to familiarize yourself 
with the dataset
"

#Plot a histogram of city_mpg faceted by suv

str(cars$city_mpg)


str(cars$suv)


table(cars$suv)


"
x-axis numerical variable city_mpg so histogram, faceted by logical variable suv
"
ggplot(cars, aes(x = city_mpg)) +
  geom_histogram() +
  facet_wrap(~suv)


"
To explore the relationship between these two variables:
The mileage of a car tends to be associated with the size of
its engine(as measured by the number of cylinders.
"
names(cars)

str(cars$hwy_mpg)

str(cars$ncyl)

length(unique(cars$ncyl))

table(cars$ncyl)

"
Filter cars to include only cars with 4, 6, or 8 cylinders 
and save the result as common_cyl. The %in% operator 
may prove useful here.
"

common_cyl = cars %>% filter(ncyl %in% c(4,6,8))

glimpse(common_cyl)


common_cyl$ncyl = as.factor(common_cyl$ncyl)

table(common_cyl$ncyl)

# Distribution of city mileage wrt ncyl

"
city mileage is numerical variable and ncyl categorical
"
# side-by-side boxplot of city_mpg fro each of the ncyl types

ggplot(common_cyl, aes(x = ncyl, y = city_mpg)) +
  geom_boxplot()

# overlaid density plot of city_mpg coloured by ncyl

ggplot(common_cyl, aes(x = city_mpg, fill = ncyl )) +
  geom_density()
  
# alpha for trasparency
ggplot(common_cyl, aes(x = city_mpg, fill = ncyl )) +
  geom_density(alpha = 0.3)

"
Which of the following interpretations of the plot is NOT valid?

Possible Answers
The highest mileage cars have 4 cylinders.

The typical 4 cylinder car gets better mileage than the typical 6 cylinder car, 
which gets better mileage than the typical 8 cylinder car.

Most of the 4 cylinder cars get better mileage than even the most efficient 8 cylinder 
cars.

The variability in mileage of 8 cylinder cars is similar 
to the variability in mileage of 4 cylinder cars.
(ans)
"


"
Answer:
The city_mpg spread of 4 cyl cars is wider in the density plot
- thus the conclusion that variability in mileage of 8 cylinder cars
seems much smaller than that of 4 cylinder cars is correct answer.

"

"
Distribution of one variable
If you are interested in the distribution of single numerical variable
- there are three ways you can get there
- first is to look at the marginal distribution
- like simple distribution of highway mileage using histogram
"

ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram()


" 
If we want to see the distribution on a different subset of
the data, says cars that are pickup trucks
- we can add the facet_wrap layer to break the distribution 
for pickups and non-pickups
"

ggplot(cars, aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_wrap(~pickup)


"
Another scenario would be to see the distribution
on a subset of the data
- says cars that have engine size with less than 2 litres 
- since engine size is numerical, it won't work with facets
- instead we have to filter out the rows first
"

str(cars$eng_size)


"
engine size is numerical and can not be factorised
So we will filter out the rows we need to see the hwy_distribution
"
cars2 = cars %>%
  filter(eng_size < 2.0)

ggplot(cars2, aes(x = hwy_mpg)) +
  geom_histogram()

"
Using data pipeline for filtering and plotting
"
cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram()


"
Resulting plot makes some sense
- these are small cars though
- and small engines are usually more efficient
- therefore we're seeing higher mileages than we saw with the
whole dataset
- your sense of distribution can also change with the binwidth 
that is selected
- ggplot does it's best to select the binwidth
- for wide bin width, use bindwidth argument, set to 5
- the result is a histogram that is much smoother
- the same principle holds true for density plots
- to plot a smoother density plot use argument bw = 5

How do we decide what is the best bandwidth for a plot
- usually the defaults are sensible
- but it is good practise to take a look at smoother and less
smoother versions of the plots to focus on the different structures
of the data
"

# Density plot
cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_density()

cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_density(bw = 5)


# Wide bin width
cars %>%
  filter(eng_size < 2.0) %>%
  ggplot(aes(x = hwy_mpg)) +
  geom_histogram(binwidth = 5)


"
Marginal and conditional histograms
- {marginal is for one variable
conditional means subsetted dataset}

Now, turn you attention to a new variable: horsepwr.
The goal is to get a sense of the marginal distribution of
this variable and then compare it to the distribution of horsepower
conditional on the price of the car being less than $25,000

You'll be making two plots using the 'data pipeline' paradigm
where you start with the raw data and end with the plot.
"
#Marginal Histogram
"
Create a histogram of the distribution of horsepwr across all cars
and add an appropriate title.
Start by piping in the raw dataset.
"

str(cars$horsepwr)

# Distribution of numerical variable horsepwr
cars %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  ggtitle("Distribution of Horsepower")


# Conditional Histogram
"
Create a second histogram of the distribution of horsepwer, but
only for those cars that have an msrp less than $25000
Keep the limits of the x-axis so that they're similar to that of
the first plot, and a descriptive title.
"
str(cars$msrp)

summary(cars$msrp)



cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  ggtitle("Distribution of Horsepower for cars under $25K")


cars %>% 
  filter(msrp < 25000) %>%
  ggplot(aes(x = horsepwr)) +
  geom_histogram() +
  xlim(c(90,550)) +
  ggtitle("Distribution of Horsepower for cars under $25K")

"
Marginal and conditional histograms interpretation
Observe the two histograms in the plotting window and decide
which of the following is a valid interpretation.

Possible Answers
Cars with around 300 horsepower are more common than cars with around 200 horsepower.

The highest horsepower car in the less expensive range has just under 250 horsepower.
(ans)

Most cars under $25,000 vary from roughly 100 horsepower to roughly 350 horsepower.
"

"
Three binwidths
Before you take these plots for granted, it's a good idea
to see how things change when you alter the binwidth
- The binwidth determines how smooth your distribution will appear
- the smaller the binwidth the more jagged your distribution becomes.
- it's good practice to consider several binwidths in order to 
detect differnt types of structure in your data

Create the following three plots adding a title to each to indicate 
the binwidth used:

- A histogram of horsepower with a binwidth of 3
- A second histogram of horsepower with a binwidth of 30
- A third histogram of horsepower with a binwidth of 60
"

# Create hist of horsepwr with binwidth of 3
cars %>%
  ggplot(aes(x= horsepwr)) +
  geom_histogram(binwidth = 3) +
  ggtitle("Histogram with binwidth 3")

# Create hist of horsepwr with binwidth of 30
cars %>% 
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 30) +
  ggtitle("Histogram with binwidth 30")

# Create hist of horsepwr with binwidth of 60
cars %>% 
  ggplot(aes(horsepwr)) +
  geom_histogram(binwidth = 60) +
  ggtitle("Histogram with binwidth 60")

"
Nice! Be sure to toggle back and forth in the plots pane to compare 
the histograms.
"

"
Three binwidths interpretation
What feature is present in Plot A that's not found in B or C?

Possible Answers
The most common horsepower is around 200.

There is a tendency for cars to have horsepower right at 200 or 300
horsepower.
(ans)

There is a second mode around 300 horsepowe
()

Nice one! Plot A is the only histogram that shows the 
count for cars with exactly 200 and 300 horsepower.
"

"
Box plots
- how are boxplots constructed starting by dotplots
- boxplots are based around three summary statistics
- firs, second and third quartile of the data
- second quartile is the median
- the value that is in the middle of the dataset
- half the data is below it
- and half is above it
- first quartile has quarter of data below it and third quartile
has three quarters of data below it
- these three numbers form the box in the box plot
- with the median in the middle and the first & second quartile
as the edges
- one thing you'll notice is that the middle half of the data is
inside this box
- there are various rules about where to draw the whiskers
that is the line that extends out of the box
- ggplot2 draws it out one and half times the length of box
then draws it in to the first observation that is encounter
- the particular rule is less important than the interpretation
- which is the whiskers to encompass nearly all of the data
- any data that is not encompassed by the box or the whiskers is
represented as a point
- this is one of the handy features of the boxplot that flag the points
that are far away from the bulk of the data
- a form of automated outlier detection
- let's revisit this side-by-side box plot that you constructed in your
exercise 

"

ggplot(common_cyl, aes(x = ncyl, y = city_mpg)) +
  geom_boxplot()

"
The above boxplot shows the distribution of city mileage broken down 
by cars that have 4 cylinders 6 cylinders and 8 cylinders
- the median mileage is greatest for 4 cylinders than for 6 cylinders
- for 8 cylinders something odd is going on,
the median is very close to the third quartile
- in terms of variability 4 cylinder cars have the widest box the 
whiskers that extend the farthest
- the middle half of the data in 6 cylinder cars expands a very
small range of values shown by the narrow box
- finally we see some outliers, one 6 cylinder car with low mileage
and several 4 cylinder cars with high mileage
- if you are wondering about that highest outlier in the 4 cylinders 
category that's indeed a hybrid vehicle
- by syntax ggplot expects you to plot several boxplots side-by-side
- if you want to see just a single one, you can set the x argument 
to 1 ggplot(cars, aes(x=1, y = msrp)) + geom_boxplot()
- so boxplots outshine when you have to detect outliers but they 
cannot detect if there are two humps or modes
- if a density plot show two humps for the distribution
{that is two bell shape curves or two peaks} and we if we construct
a boxplot for the same structure it will push this important
structure under the rug and just show a single box 
"
"
Box plots for outliers
In addition to indicating the center and spread of a distribution, 
a box plot provides a graphical means to detect outliers.
You can apply this method to msrp column ( manufacturer's suggested
retail price) to detect if there are unusually expensive or cheap cars.
"

#Construct a box plot of msrp.

cars %>% ggplot(aes(x = 1, y = msrp)) +
geom_boxplot()


"
Exclude the largest 3-5 outliers by filtering the rows to retain 
cars less than $100,000. Save this reduced dataset as cars_no_out.
"

# Exclude outliers from data
cars_no_out <- cars %>%
filter(msrp < 100000)


"
Construct a similar box plot of msrp using this reduced dataset. 
Compare the two plots.
"
# Construct box plot of msrp using the reduced dataset
cars_no_out %>%
ggplot(aes(x=1, y = msrp)) +
geom_boxplot()

"
Great job! Be sure to toggle back and forth in the plots pane 
to compare the box plots.
"

# Plot selection

"
Consider two other columns in the cars dataset: city_mpg and width
Which is the most appropriate plot for displaying the important
features of their distributions?
Remember, both density plots and boxplots display the central tendency
and spread of the data, but the box plot is more robust to outliers.
"
"
Use density plots or box plots to construct the following 
visualizations. For each variable, try both plots and submit 
the one that is better at capturing the important structure.
"

cars %>% ggplot(aes(x = 1, y = city_mpg)) +
  geom_boxplot()

cars %>% ggplot(aes(x = city_mpg)) +
  geom_density()


str(cars$width)

summary(cars$width)


cars %>% ggplot(aes(x = 1, y = width)) +
  geom_boxplot()

cars %>% ggplot(aes(x = width)) +
  geom_density()

"
Great work! Because the city_mpg variable has a much wider 
range with its outliers, it's best to display its distribution 
as a box plot.

{ For width there are two humps so we could go with density plot }
"

"
Visualizations in higher dimensions:
- what is the association between this variable and that one
- and if you condition on one level of distribution how does the 
distribution of another one change
- the answer to these variables requires multivariate thinking
- and it is an essential skill in reasoning about the structure of
real data
- but why stop at only two variables
- using facet grid we can incorporate 3 variables in the plot
"

"
msrp is a numerical variable, so there are many plots we can use.
- lets go with the density plot
- by adding a facet grid layer, we can break that distribution
down by two categorical variables separated by a ~
- lhs variable form the rows and rhs variables form the columns 
of the grid
- the below gives us a grid of four density plots
- we add the labeller argument so we can identify the 
variables labeller = label_both

"

ggplot(cars, aes(x = msrp)) +
  geom_density() +
  facet_grid(pickup ~ rear_wheel, labeller = label_both)


table(cars$rear_wheel, cars$pickup)

"
second column rear_wheel true 
shows that we have fewer cars with rear_wheel
"

"
Higner dimensional plots
Anything that can be discern visually, things like
- shape
- size
- color
- pattern
- movement
- relative location, x-coordiate y-coordinate
these can be mapped to a variable and plot alongside other variables

"
"
3 variable plot
Faceting is a valuable technique for looking at several conditional
distributions at the same time.

If the faceted distributions are laid out in a grid, you can
consider the association between a variable and two others.
One on the rows of the grid and the other on the columns.

"

"
Using common_cyl create a histogram of hwy_mpg faceted on 
both ncyl and suv

Add a title to your plot to indicate what variables are being
faceted on.
"

common_cyl %>% ggplot(aes(x = hwy_mpg)) +
  geom_histogram() +
  facet_grid(ncyl~suv, labeller = label_both) +
  ggtitle("Mileage by suv and ncyl")

"
Interpret 3 var plot
Which of the following interpretations of the plot is valid?

Possible Answers
Across both SUVs and non-SUVs, mileage tends to decrease as the number of 
cylinders increases.
(ans)

There are more SUVs than non-SUVs across all cylinder types.

There is more variability in 6-cylinder non-SUVs than in any 
other type of car.

"


# Exploratory analysis of life dataset

glimpse(life)

fem.x = head(round(life$Female.life.expectancy..years.), 11)

fem.x

# Typical measures of center: mean, median, mode

# average value
mean(fem.x)
#[1] 77.2

# median is the middle value in the assorted data sort
median(fem.x)
#[1] 77

# mode is simply the most common observation in the data set
# what does mode function return?
mode(fem.x)
#[1] "numeric"

"
Use the table function with one variable
- it gives the count for different values of the variable
"
table(fem.x)
"
fem.x
75 76 77 78 79 
1  1  6  1  2 
"
# 77 is the most occuring value

"
Note: Imp
Here both, median and mode are same. But thats often not 
the case.
The mean can be thought of as tha balance point of the data.
and it tends to be drawn at the longer tail of the distribution.
This highlights an important feature of the mean
- its sensitivity to extreme values
- for this reason when working with skewed distributions,
median is a more appropriate measure of centre.

Now that we have some sensible measures of center, we can 
answer questions like
- is the typical life expectancy on the west coast similar to the 
life expectancy in the rest of the country?
"

library(gapminder)
glimpse(gapminder)

# county dataset in openintro or gapminder

library(openintro)
glimpse(county)

unique(county$state)

unique(life$State)

# Let's start by creating a new variable 
life= life %>%
  mutate(west_coast = State %in% 
           c("California","Oregon", "Washington"))

county = county %>%
  mutate(west_coast = state %in% 
           c("California","Oregon", "Washington"))

names(life)

names(county)

head(life$west_coast)

str(life$west_coast)

unique(life$west_coast)

"
mean and median for west_coast and non west_coast states
- this is done using group_by and summarize
"

life %>%
  group_by(west_coast) %>%
  summarise(mean(Female.life.expectancy..years.),
            median(Female.life.expectancy..years.))

"
To understand group_by
- see the summarize function without group_by then group_by

"
life %>%
  slice(240:247) %>%
  summarize(mean(Female.life.expectancy..years.))


life %>%
  slice(240:247) %>%
  group_by(west_coast) %>%
  summarize(mean(Female.life.expectancy..years.))

"
Choice of center measure
The choice of measure for center can have a dramatic impact
on what we consider to be a typical observation,
so it is important that you consider the shape of the distribution
before deciding on the measure.

Which set of measures of central tendency would be worst for
describing the two distributions shown here?


A - has typical bell curve shaped distribution, no skews
B - has a peak on left side indicating modal value
and is highly skewed on right

Possible Answers
A: mode, B: median 

A: mean, B: mode ( B can't be mean as mean is sensitive to skew, 
aslo not median because the distribution is not divided into 
equal halves, mode is good as there is peak in the distribution 
corresponding to mode)

A: median, B: mean

A: median, B: median

"

"
Calculate center measures
- throughout this chapter, you will use data from gapminder
which tracks demographic data in countries of the world over time.
To learn more about it, you can bring up the help file with 
?gapminder

For this exercise, focus on how the life expectancy differs from
continent to continent.
This requires that you conduct your analysis not at the country level
but aggregated up to the continental level.

This is made possible by group_by() and summarize(), a very powerful syntax for 
carrying out the same analysis on different subsets of the full dataset.
"

glimpse(gapminder)


"
Create a dataset called gap2007 that contains only data from the year 2007.
"

str(gapminder$year)

length(unique(gapminder$year))

unique(gapminder$year)

gap2007 = gapminder %>% 
  filter(year == 2007 )


glimpse(gap2007)

"
Using gap2007 calculate the mean and median life expectancy for
each continent.
Don't worry about naming the new columns produced by summarize().
"

str(gapminder$continent)

unique(gapminder$continent)


gap2007 %>% 
  group_by(continent) %>%
  summarise(mean(lifeExp), median(lifeExp))


"
Confirm the trends that you see in the medians by
generating side-by-side box plots of life expectancy for each continent
"
gap2007 %>% 
  ggplot(aes(x = continent, y = lifeExp)) +
  geom_boxplot() 


"
Measure of variability
- how do you summarise the variability that you see in a set of numbers
- let's consider the life expectancies in those first eleven counties in the US
"  
life.x = head(gapminder$lifeExp, 11)
life.x

life.x - mean(life.x)

fem.x

fem.x - mean(fem.x)

"
As you can see some have positive difference and some negative
- let's reduce them to single level of variability
- so let's add up all the variabilities
"

sum(life.x - mean(life.x))

sum(fem.x - mean(fem.x))

"
sum of differences is essentially it is zero, 
which is not true because there are differences, it is because
the positive negative difference cancel out each other
- to fix this we square the differences
"

sum((life.x - mean(life.x))^2)

sum((fem.x - mean(fem.x))^2)

"
This new measure of variability is better 
- but it has an undesirable property that it gets bigger as
you add more data to it
- to fix this problem you can divide the sum by the number of
observations
"
length(life.x)

sum((life.x - mean(life.x))^2)/length(life.x)

sum((fem.x - mean(fem.x))^2)/length(fem.x)

"
{What is sample variance and sample standard deviation?}

So this is starting to look like a useful measure
- you find the center of the data
- then find the difference between the observations and that mean
- then square and divide by the number of observations(n)
- if you divide by n-1, then you are left with what is called
the sample variance
- one of the most useful measures of a spread of distribution
- in R this statistic is wrapped up in a function var for variance
- another useful measure is the square root of this measure
which is called the sample standard deviation or just sd in R
- the convenient thing about sd is that once computed
it is in the same unit as the original data 
{ var is square and this is sqrt so you get back the original unit
of measure}

{ What is IQR - Inter Quartile Range}
- there are two more measures of spread that are good to 
know about
- the inter quartile range or IQR is the distance between
the two numbers that cut off the middle fifty percent of the data
- this should sound familiar from the discussion of boxplots
- the height of the box is exactly IQR
- we can get the first and third quartile from summary function
and take their difference or use the built in IQR function

{Range}
- the final measure is simply the range of the data
- the distance between the max and min
- you can use max, min function to get range or diff(range(x))


{Which is the best measure}
- For any dataset, you can compute all four of these statistics
- but which ones are the most meaningful
- the most commonly used is the standard deviation sd
- so that is often a good place to start
- but what happens if data has some extreme observations
- let's say there was lifexp of 97 for a particular state
- mean var sd are sensitive to extreme values as measure of center
- range is completely affected by the extreme values, hence it
is not usually a good measure of center
- IQR does not change though, therefore IQR is a good measure when 
data is skewed or has extreme observations
- 

"

# Sample variance

sum((life.x - mean(life.x))^2)/(length(life.x) - 1)

var(life.x)

sqrt(var(life.x))

sd(life.x)

sum((fem.x - mean(fem.x))^2)/(length(fem.x) - 1)

var(fem.x)

sqrt(var(fem.x))

sd(fem.x)

summary(life.x)

IQR(life.x)

diff(range(life.x))

summary(fem.x)

IQR(fem.x)


diff(range(fem.x))

"
Which set of measures of spread would be WORST for describing 
the two distributions shown here?

A - bell shaped curve
B - lhs has peak and rhs has extreme valuem, hence skewed

{A is normal distribution hence SD is good
B is skewed hence mean, var, sd, range will all be affected by the extreme
values, IQR is not affected by these extreme values helps
so worst measure would anything other than these}

A: IQR, B: IQR 
A: SD, B: IQR 
A: Variance, B: Range (answer) Range is not good for skewed data
"

"
Calculate spread measures

Let's extend the powerful group_by() and summarize() syntax to measures of spread.
If you're unsure whether you're working with symmetric or skewed distributions,
it's good idea to consider a robust measure like IQR in addition to the 
usual measures of variance or standard deviation
"

names(gap2007)
nrow(gap2007)
glimpse(gap2007)



"
For each continent in gap2007 summarize life expectancies using 
sd() the IQR and the count of countries (n)
No need to name the new columns produced here.
The n() function within you summarize() call does not take any arguments.
"
gap2007 %>% 
  group_by(continent) %>% 
  summarise(sd(lifeExp), IQR(lifeExp), n())


"
Graphically compare the spread of these distributions by constructing 
overlaid density plots of life expectancy broken down by continent.
"

# overlaid implies not grids but overlay in same graph
# so no need to use facet_wrap or facet_grid, just fill by continent
"
x-axis numerical variable lifeExp overlaid by continent, fill by categorical
variable continent
"
ggplot(gap2007, aes(x=lifeExp, fill = continent)) +
  geom_density() 

#{ lighten up the grapbh, alpha is to set transparency}

ggplot(gap2007, aes(x=lifeExp, fill = continent)) +
  geom_density(alpha = 0.3) 


# facet_wrap vs facet_grid

# facet_wrap creates different graps - separate x,y axis
ggplot(gap2007, aes(x=lifeExp)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~continent)

# facet_grid splits the graph into grids keeping the original 
# x,y axis
ggplot(gap2007, aes(x=lifeExp)) +
  geom_density(alpha = 0.3) +
  facet_grid(~continent)


" 
Choose measures for center and spread
Consider the density plots shown here.
What are the most appropriate measures to describe their centers and
spreads?
In this exercise, you'll select the measure and then calculate them.
"

"
Using the shapes of the density plots, calculate the most
appropriate measures of center and spread for the following

The distribution of life expectancy in the countries of the 
Americas. Note you'll need to appy a filter here.

The distribution of country populations across the entire gap2007 
dataset.
"

"Both graphs have peaks but not extreme values hence mean 
can be used as measure of center and sd as the measure
for distribution
- median is good when data is equally normally distributed
in this case there are peaks at the ends 
- mode is good measure of center if there are peaks but here we
can go with mean as there are no extreme values
"

# Compute stats for lifeExp in Americas
gap2007 %>%
  filter(continent == 'Americas') %>%
  summarize(mean(lifeExp),
            sd(lifeExp))

# Compute stats for entire population
gap2007 %>%
  summarize(median(pop),
            IQR(pop))


"
Excellent! Like mean and standard deviation, 
median and IQR measure the central tendency and spread, 
respectively, but are robust to outliers and non-normal data.
"

"
Shapes and transformations
- there are generally four characteristics of distribution
that are of interest
- the first two we have covered already 
- the center and spread or variability of the distribution
- the third is the shape of the distribution, which
can be described in terms of modality and the skew

Modality - Unimodal Bimodal
- Modality of the distribution is the number of prominent
humps that show up in the distribution
- if there is a single mode as in the case of bell-curve, 
it is called unimodal and if there are two humps it is called
bimodal and multimodal if more than two humps
- and there is one last case when there is no distinct 
mode because the distribution is flat across all values,
it is referred to as uniform

Skew - Right-skewed Left-skewed Symmetric
- the other aspect that concerns the shape of distribution
is its skew
- if the distribution has a long tail that stretches out
to the right, it is referred to as right-skewed
- and if that long tail stretches out to the left, it is
reffered to as left-skewed
- if neither tail is longer than the other, the distribution
is called symmetric

"

"
Shape of income (life dataset)
An overlay density plot with income on x-axis
filling the two curves based on whether they're on 
the west coast or not, to make the density plot lighter
we use make the colours transparent so that we can
see where they overlap - that is where the incomes overlap
"
names(life)

str(county$income)

"
x-axis numerical variable income, density plot overlaid by categorical
variable west-coast
"
ggplot(county, aes(x = income, fill = west_coast)) +
  geom_density(alpha = 0.3)



"
Each distribution is unimode
- it is difficult to compare both these distributions
as they're both heavily right skewed
- that is there are few conties in each group
that have very high incomes
- one way to remedy this is to construct a plot of a transformed version
of this plot
- since income has a very high right skew either the square root or log
transform will do a good job of drawing in the tail and spreading 
out the lower values so that we can see what's going on
- the result is a picture which is bit better to interpret
"

"
scaling income values using log
"
ggplot(county, aes(x = log(income), fill = west_coast)) + 
  geom_density(alpha = 0.3)

"
Income in west coast counties is indeed greater than in the rest
of the country
"

"
Let's explore the gapminder data

Transformations
Highly skewed distributions can make it very difficult to learn
anything from a visualization.
Transformations can be helpful in revealing the more subtle
structure

Here you'll focus on the population variable, which exhibits
strong right skew and transform it with the natural logarithm
function (log() in R)

Using the gap2007 data:
"

# Create a density plot of the population variable.

names(gap2007)

gap2007 %>% ggplot(aes(x = pop)) +
  geom_density()


#Mutate a new column called log_pop that is the natural 
#log of the population and save it back into gap2007.

gap2007 = gap2007 %>% 
  mutate(pop_log = log(pop))

names(gap2007)

# Create a density plot of your transformed variable
gap2007 %>% ggplot(aes(x = pop_log)) +
  geom_density()



"
Outliers
- we have discussed three different aspects of
distribution that are important to note when 
conducting an exploratory data analysis

1. Center
2. Variability
3. Shape

- and the final thing to look out for are

4. Outliers

- these are observations that have extreme values
far from the bulk of distribution
- they're often very interesting cases but they're
also good to know about before proceding with 
the analysis
- we saw few extreme values when we plotting
distributions of incomes of counties on west coast
- what do we make of this blip of counties
- one thing we can do is, try this as a box plot
"

ggplot(county, aes( x= 1, y = log(income), fill = west_coast)) + 
  geom_boxplot() +
  coord_flip()

"
- the box plot reveals that there are many
outliers both in west coast and non west coast
counties
- so why was the blip (slight bump on the right side
of distribution) more apparent in the case of
west coast?
- it has something to do with the sample size
- there are far fewer counties in west coast so 
these few outliers had an outsized effect on the
density plot
- in the case of the  non-west coast group there
are many many more counteis that were able to 
wash out the effect of these outliers on the density
plot

Indicating outliers
- it is often useful to consider outliers separately 
from the data
"

names(county)

summary(county$income)

"
create outlier income indicator 
for any threshold income, say 45000 usd
Below mutate assign true to the new variable
if condition is true else false
"
county = county %>%
  mutate(is_outlier = income > 45000) 

names(county)

# observations with outlier income
county %>%
  filter(is_outlier) %>% 
  arrange(desc(income)) %>%
  select(name, state, income, is_outlier)


"
We see the highest income is in Pitkin County of Colorado
state
"

"
Plotting without outliers
- first filter out those counties that are not
outliers and pass this to ggplot using %>% 
operator
"
# Overlay density plot of incomes for west_coast 
# and non west_coast without outliers
county %>%
  filter(!is_outlier) %>%
  ggplot(aes(x = income, fill = west_coast)) +
  geom_density(alpha = 0.3)


"
The result is a plot that focuses much more
on the body of the distribution.
- compare this with the original plot which was
dominated by the right skew caused the the extreme
values
- non of these plots is right or wrong, they both 
tell us different stories about the structure in 
this data, both of which are valuable

"

"
Exploring outliers in gapminder data

Identify outliers
Consider the distribution shown here of the life expectancies
of the countries in Asia.
The box plot identifies one clear outlier:
a country with a notably low life expectancy.
Do you have a guess as to which country this might be?
Test your guess in the console using either min()
or filter() then proceed to building a plot with
that country removed.
"

names(gap2007)

gap_asia = gap2007 %>%
  filter(continent == 'Asia') %>%
  mutate(is_outlier = lifeExp < 50 )

names(gap_asia)

gap_asia %>%
  ggplot(aes(x = 1, y = lifeExp)) +
  geom_boxplot()


gap_asia %>% 
  filter(!is_outlier) %>%
  ggplot(aes(x = 1, y = lifeExp)) +
  geom_boxplot()


'
End of  "Numerical Summaries"!

'










