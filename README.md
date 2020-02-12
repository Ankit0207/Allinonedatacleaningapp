# ALL IN ONE DATA CLEANING APP

Every now and then you must have heard that most of the data scientists spend 60-70% of their time in data cleaning.  76% of data scientists view data preparation as the least enjoyable part of their work.

While working on any data science assignments or projects you must have surely noticed the fact that most of our time is spent on data cleaning and preparation after which we are left out with very less time on building the models. I know data cleaning is equally important as we cannot do better predictions if our data is not good enough. But learning and building the models is much more important as the models which we learn are much more broader than what we generally think of it. There is so much we can do with the data science algorithms. Just like people say "sky is the limit", with the data science algorithms in hand our imagination is the limit to what we can achieve with it. 

So in order to focus more on the algorithms we need a faster way to finish the data cleaning activities. This is where my "All in One Data Cleaning App" would come in handy.

(/Allinonedatacleaningapp/all_in_one_data_cleaning_app.JPG)

What's so special in this app?

Well, there are so many things this app can do. The main purpose of this app is to automate the data cleaning process. Below are the data cleaning functionalities which the app can perform - 

1. Handling Outliers

2. Removes Columns having NA's greater than input threshold

3. Shows variables which are highly correlated based on input threshold

4. Removes Highly Correlated Variables(works for everything except multi-class target variable)

5. Shows and removes non-significant variables based on P-value significance tests(works for everything except multi-class target variable)

6. Imputes Missing values using the BEST approaches like mice and missForest algorithms.

7. Imputes Missing values using the EASY approach like mean/median/mode imputation

8. Standardize numerical variables using the min-max Transformation

9. Converts Categorical variables into dummy variables.

10. Removes Columns having unique values i.e. SNO and Primary keys columns

11. Removes Columns having only 1 Level i.e. Zero Variance

12. After selecting all of the above options, the cleaned dataset can be downloaded in one go.

Below are the instructions on the detailed use of the app - 

1. Handling Outliers

a. Ignore Outliers

You can choose this option if you do not want to handle outliers

b. Show Outliers

Once this option is selected, the app shows you the columns in which outliers are present along with the number of outliers in each of those columns.

c. Show Box Plots for Outlier columns

Once this option is selected, the app shows box plots for the outlier columns(provided the number of outlier columns are less than or equal to 10). If the number of columns having outliers exceeds 10, then the app automatically gives you the option to see the column for which you want to see boxplot by giving manual input.

d. Replace Outliers with NA's

Once this option is selected, the app will replace all the outliers in each and every column with NA's.

2. Threshold % limit for NA's in each column

a. Ignore

This option is selected if you do not want to remove any columns with NA's

b. Remove Columns

Once this option is selected, you will have the option to remove columns having NA's based on percentage threshold limit.

Example: If you give the percentage threshold as 70%, the app will remove all the columns in which NA's are greater than 70% of the total number of rows.

3. Correlation between Independent Variables

a. Ignore

This option can be selected if you do not want to handle correlation between independent variables

b. Check for variables having high correlation

Once this option is selected, the app will show the variables which are highly correlated based on the input threshold you would provide.

Example: If you give the threshold % for correlation coefficient(r) as 80%, then the app would display all the correlated independent variables having r > 80%

c. Remove Highly correlated Variables(Works ONLY FOR BINARY TARGET VARIABLE)

Once this option is selected, the app would remove one among the pair of correlated variables obtained above based on P-value significance tests.

4. Best Missing Values Imputation Method

***NOTE***- These methods are much much better than the usual mean/median imputation as for each variable the algorithm fits a random forest on the observed part and predicts the missing part. The missing values are filled up entirely using the predictions rather than just filling it based on mean/median which is much likely to be biased.
 
a. Ignore

This option can be selected if you do not want to handle missing values using some of the best methods.

b. MissForest 

missForest is a nonparametric imputation method for basically any kind of data. It can cope with mixed-type of variables, nonlinear relations, complex interactions and high dimensionality (p ≫ n). It only requires the observation (i.e. the rows of the data frame supplied to the function) to be pairwise independent. 

Once this option is selected, the missing values would be imputed using the missForest algorithm.

Heads up: This algorithm is one of the best algorithms for imputing missing values but it does require lots of time. Sometimes it may take some an hour or many hours(totally depends on the size of the data). But dont worry the app would keep you updated on the status of each and every thing.

c. Mice

Once this option is selected, the missing values would be imputed using the mice algorithm.

5. Easiest Missing Values Imputation Method

a. Ignore

Select this option if you do not want to handle missing values using the usual mean/median approach.

b. Mean (Numerical) and Mode (Categorical)

Select this option if you want numerical variables missing values to be imputed by the mean and mode for categorical variables.

c. Median (Numerical) and Mode (Categorical)

Select this option if you want numerical variables missing values to be imputed by the median and mode for categorical variables.

6. Handling Imbalanced Target Variables Class

a. Ignore

Select this option if you do not want to handle imbalanced target variable classes.

b. Check Target Variable Classes

Once this option is selected, app will display the frequency of the target variable classes.

c. Handle Imbalanced data through SMOTE

Based on the 2nd option, if you see that the target variable classes are imbalanced, then you can make a balanced dataset of target variable classes by uisng this option.

Once this option is selected, two slider inputs would pop up. 

1. Select Perc Over for SMOTE input

2. Select Perc Under for SMOTE input

Select the inputs based on your requirement and the app would show you how the new target variables classes frequency are.  

The new balanced dataset can be downloaded by clicking on the download option.

NOTE:

If you want a perfectly balanced dataset, use perc over = 100 and perc under = 200

7. Standardize Numerical Variables

a. Ignore

Select this option if you do not want to standardize numerical variables.

b. Min-Max Transformation

Select this option if you want to standardize numerical variables using min-max transformation.

8. Convert Categorical Variables into dummy Variables

a. Ignore

Select this option if you do not want to convert categorical variables into dummy variables.

b. Convert

Select this option if you want to convert categorical variables into dummy variables.

9. Remove Columns having Unique Value i.e. SNO's, Primary Keys

a. Ignore

Select this option if you do not want to remove columns having Unique Value i.e. SNO's, Primary Keys.

b. Convert

Select this option if you want to remove columns having Unique Value i.e. SNO's, Primary Keys.

10. Remove Columns having just 1 Level

a. Ignore

Select this option if you do not want to remove columns having just 1 level.

b. Remove

Select this option if you want to remove columns having just 1 level.

#datascience #datascienceinnovation
