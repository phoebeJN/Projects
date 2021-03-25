# DataCamp Projects in R
[DataCamp](https://www.datacamp.com) is a learning platform where you can build your data skills online.

## R projects 
|No.|Project |Description |Note
|:--|:------------------|:---------------------|:----
|1|Visualizing COVID-19|We visualize COVID-19 data to see at what point this virus became a global pandemic.|Data visualization(`ggplot2` package)|
|2|Predict Taxi Fares with Random Forests|Use **regression trees** and **random forests** to predict where a taxi driver can earn the most in Manhattan.|Supervised Learning: Classification|
|3|Clustering Heart Disease Patient Data|Use unsupervised clustering algorithms to help doctors understand which treatments might work with their patients.|Unsupervised Learning: **k-means** and **hierarchical clustering**|
|4|Degree that pay you back|Use **k-means clustering** to explore the short and long term financial implications of college major decisions.|Three methods for determining **the optimal number of clusters**: the elbow, the silhouette, and the gap statistic methods (`fviz_nbclust()` and `fviz_gap_stat()` from `factoextra` package)|
|5|Planning Public Policy in Argentina|Use **PCA** and **K-means clustering** to help plan an education program in Argentina.|run a PCA with the `PCA()` function from the `FactoMineR` package, and build a correlation circle plot and a PCA biplot using the `factoextra` package.| 
|6|Clustering Bustabit Gambling Behavior|Use **PCA** and **K-means clustering** to glean insights into cryptocurrency gambling behavior.|The `ggparcoord()` function from `GGally` will be used to produce a Parallel Coordinate Plot or PCP.|
|7|The Impact of Climate Change on Birds|Use **a generalized logistic regression** to predict where our bird species of interest is likely to occur in the future.|Making models with `caret` package|
|8|What Makes a Pokemon Legendary?|Use **a decision tree** and **a random forest** to identify the characteristics of legendary Pokemon.|Plot the ROC curves to assess model fit using `ROCR` package| 
|9|Reducing Traffic Mortality in the USA|Use **a multivariate linear regression**, **PCA** and **K-means clustering** to find a good strategy for reducing traffic-related deaths.|Check the name of the current folder using `getwd()`; list all files in the current folder using `list.files()`; view the first 20 lines of a dataset using `readLines(..., 20)`; create a pairwise scatterplot between all columns - a "scatterplot matrix", using `ggpairs()` from `GGally`; perform PCA using `princomp()` |
|10|Dr.Semmelweis and the Discovery of Handwashing|Reanalyse the data behind one of the most important discoveries of modern medicine: handwashing.|Use `t.test()` for two-sample t-test in order to get a 95% CI.|
|11|Exploring the Kaggle Data Science Survey|Discover the top tools Kaggle participants use for data science and machine learning.|Plot a bar chart using `fct_reorder()` to show most counts on the far right. Use `case_when()` to create a new column. Use `str_detect()` to detect the presence or absence of a pattern in a string. Use `n()` to get the number of observations in the current group within `summarise()`.|
|12|Level Difficulty in Candy Crush Saga|Analyze data from the hit mobile game, Candy Crush Saga.|Use `geom_hline()` to add a horizontal line within `ggplot`. Use `geom_errorbar()` to add error bars at each point. Use `prod()` to multiply all the numbers in a vector together.|
|13|Visualizing Inequalities in Life Expectancy|Compare life expectancy across countries and genders with `ggplot2`.|Convert one column into two other columns and reshape datasets using `spead()`; Combine two columns into one column using `mutate()` and `paste()`|
|14|Partnering to Protect You from Peril|Examine the **network** of connections among local **health** departments in the United States.|The project will use `igraph`, `readr` and `dplyr` to import and examine a network made up of an edgelist and an attribute file.|
|15|What Your Heart Rate Is Telling You|Examine the relationship between heart rate and heart disease using **multiple logistic regression**.|Explore the associations for each variable using t-test (for cts vars) or chi-squared test (for categorical vars) to get p-values, and using boxplot (cts) or barplot (categ). Use `auc()`, `accuracy()`, and `ce()` within `Metrics` package to get AUC, accuracy rate, and classification error rate to see the model performance.|
|16|Where Would You Open a Chipotle?|Create and explore interactive maps using Leaflet to determine where to open the next Chipotle.|Using `leaflet` (Create Interactive Web Maps with the JavaScript 'Leaflet') and `leaflet.extras` packages.|
|17|Who Is Drunk and When in Ames, Iowa?|Flex your **data manipulation** muscles on breath alcohol test data from Ames, Iowa, USA.|Using the `lubridate` function `ymd()` to make a date column. |
|18|Going Down to South Park: A **Text Analysis**|Analyze the dialog and IMDB ratings of 287 South Park episodes. Warning: contains explicit language.|`tidytext` and `sweary` packages|
|19|Where Are the Fishes?|Explore acoustic backscatter data to find fish in the U.S. Atlantic Ocean.|With `patchwork`, you can arrange the plots side-by-side with +. Convert Values To NA using`na_if()`; creates an Interval object with the specified start and end dates using `interval()`. |
|20|Drunken Datetimes in Ames, Iowa|Apply your skills from "**Working with Dates and Times** in R" to breathalyzer data from Ames, Iowa.|`lubridate` package|
|21|Gender Bias in Graduate Admissions|Analyze admissions data from UC Berkeley and find out if the university was biased against women. (a classic example of Simpson's paradox)|`UCBAdmissions` dataset is included in base R. Using the `tidy()` function from the `broom` package, which converts a model object (a three-dimensional array in our case) into a tidy tibble, where each variable is a column and each observation is a row. To format acceptance rate as a percentage using the `percent` function from the `scales` package. We can hypothesise that the effect of gender on acceptance is null when you control for department. We can test that hypothesis using **binary logistic regression**, but first we need to de-aggregate the dataset so that each row represents one student using `rep()`. |
|22|Data Science for Social Good: Crime Study|Use data science to catch criminals, plus find new ways to volunteer personal time for social good.|The dataset used in this project is hosted on **Kaggle** and updated daily. Create a "long format" data frame by using `gather()`|
|23|Are You Ready for the Zombie Apocalypse?|Use your **logistic regression** skills to protect people from becoming zombies!|`gridExtra` package provides a number of user-level functions to work with "grid" graphics, notably to arrange multiple grid-based plots on a page, and draw tables. Use`prop.table()` to compute percent zombies and humans for each factor. `odds.n.ends` package for a logistic regression function to get model significance, fit, and odds ratios with 95% CI. Checking model assumptions uses `vif()` within `car` parkage, and graphs to see linearity.|
|24|Health Survey Data Analysis of BMI|Analyze health survey data to determine how BMI is associated with physical activity and smoking.|Load `NHANES`package to get the dataset. Using the `survey` package to specify the complex survey design.|
|25|Importing and Cleaning Data|Apply your **importing and data cleaning skills** to real-world soccer data.|Using `which()` and `is.na()`, find the index value for the NA in the variable column of the dataset and use `[]` to subset and view the row with the missing data, then replace the NA with the correct data value (given) [Task4/5]. Or, using `replace_na()`. 
|26|Functions for Food Price Forecasts|**Write functions** to **forecast** time series of food prices in Rwanda.|`read`r's `cols_only()`, `dplyr`'s `rename()`. The best and most mature tools for analysis are based around a time series data type called `ts`. Use `forecast` package to forecast.|
|27|Trends in Maryland Crime Rates|Apply **hierarchical and mixed-effect models** to analyze Maryland crime rates.| a linear mixed-effects regression model (a hierarchical model), using `lmer()` within `lmerTest` package. Use `fixef()` to extract only the fixed-effects and Use ranef() to extract only the random-effects.
|28|Rise and Fall of Programming Languages|Analyze the relative popularity of programming languages over time based on Stack Overflow data.|`ggplot2` and `dplyr` packages|
|29|TV, Halftime Shows, and the Big Game|**Load, clean, and explore** Super Bowl data in the age of soaring ad costs and flashy halftime shows.|`str_detect()`|
|30|Phyllotaxis: Draw Flowers Using Mathematics|Use R to make art and create imaginary flowers inspired by nature.|`ggplot2` package
|31|Wrangling and Visualizing Musical Data|Wrangle and visualize musical data to find common chords and compare the styles of different artists.|`slice()`, `reorder()`, and `lead()`|
|32|Scout your Athletics Fantasy Team|Analyze athletics data to find new ways to scout and assess jumpers and throwers.|`dplyr` and `ggplot2` packages|
