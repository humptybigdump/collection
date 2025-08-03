# Media Management Tutorial Exercises

## 1. Data preparation

# load libraries
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
import statsmodels.api as sm

# load in data
data = pd.read_csv("games_may2024_full.csv")

# check variables
data.columns

# check for missing values
def check_na (column):
  return pd.isnull(column).sum()

# check_na is applied to every column of the dataset, the counts are divided by
# the total number of observations (data.shape[0]) to get fractions
data.apply(check_na, axis = 0) / data.shape[0]

# we can make it easier to read by rounding the results on 4 digits
round(data.apply(check_na, axis = 0) / data.shape[0], 4)

# we can save the result as a dataframe
missing_percentages = pd.DataFrame(round(data.apply(check_na, axis = 0) / data.shape[0], 4))

# you can give the column a nicer name
missing_percentages.columns = ["missing_percent"]

# add the indices as column
missing_percentages["variable"] = missing_percentages.index

# now you can change the indices to numbers
missing_percentages.index = range(missing_percentages.shape[0])

# now we can sort the column with the missing values so we get a better idea
# about the missings
missing_percentages = missing_percentages.sort_values(by = "missing_percent", ascending = False)

# now we have a nice overview over how many missings each variable has

# if we want to deal with the missing values now, there are multiple ways
# for simplicity I would only expect you to delete rows with missings
# (another way would e.g. be imputation)

# first we remove the variables with too many missing values

# we could for example say that variables with more than 10% missings are ok to
# remove, if we do not need these variables
var_to_remove = missing_percentages["variable"][missing_percentages["missing_percent"] > 0.1].tolist()
# now we have a list of variables with a high number of missings, we can check
# this list
var_to_remove

# none of these variables are relevant for our later tasks, so we can remove
# them
# in general it makes sense to remove all variables that we do not need, but
# for now it is fine to not go through every single variable
# let us remove the variables
data = data.drop(var_to_remove, axis = 1)

# subsequently we remove all rows with missings in this case (in real world
# applications other procedures may make more sense)
# let us also check how many observations we still have
print(data.shape)
data = data.dropna()
print(data.shape)

# we lost about 4000 rows, given the size of the dataset it is ok for now
# in practice you would discuss more about which variables to keep in general
# and how to deal with missing values

# this is a relatively difficult case of missings, your datasets will be more
# tame in this regard, but you will be able to get information about how to deal
# with a lot of things from this notebook

## 2. Data exploration

# let us get some descriptives
data.shape

data.describe()

data.dtypes

# correlation plot
data_cor = data.corr(numeric_only = True)
sns.heatmap(data_cor)

## Task 1: Show recent developments in gaming by filtering the data for recent games and plotting the metacritic scores on a timeline. Can you make out a trend? Discuss!

# filter dataset for recent games, let's say from 2020 onwards
data["release_date"].describe()

# transform date-variable to date-datatype
from datetime import datetime

data["release_date"] = data["release_date"].apply(lambda x: datetime.strptime(x, "%Y-%m-%d"))

# filter for video games that released from 2020 onwards
data_task_1 = data[data["release_date"] >= "2020-01-01"]

# plot the metacritic scores over time
plt.scatter(data_task_1["release_date"], data_task_1["metacritic_score"])

data_task_1_filtered = data_task_1[data_task_1["metacritic_score"] != 0]
plt.scatter(data_task_1_filtered["release_date"], data_task_1_filtered["metacritic_score"])

# make the graph nicer
fig, ax = plt.subplots()
ax.scatter(data_task_1_filtered["release_date"], data_task_1_filtered["metacritic_score"],
           color = "cornflowerblue")
ax.set_title("Metacritic scores over time")
ax.set_ylim(0, 100)
plt.xlabel("Release date")
plt.ylabel("Metacritic Score")
plt.xticks(rotation = 90)
plt.show()

## Task 2: Conduct a regression analysis with metacritic_score as the independent variable and recommendations as the dependent variable. Interpret the results. Do consumers have "good taste"? Discuss!

# univariate regression of metacritic score on recommendations
X = pd.DataFrame(data_task_1_filtered["metacritic_score"])
y = pd.DataFrame(data_task_1_filtered["recommendations"])


reg_1 = sm.OLS(y, X).fit()
print(reg_1.summary())

# add more variables to try to explain the number of recommendations a game receives
X["num_reviews_total"] = data_task_1_filtered["num_reviews_total"]

reg_2 = sm.OLS(y, X).fit()
print(reg_2.summary())

X = pd.DataFrame(data_task_1_filtered["price"])

reg_3 = sm.OLS(y, X).fit()
print(reg_3.summary())

X["num_reviews_total"] = data_task_1_filtered["num_reviews_total"]

reg_4 = sm.OLS(y, X).fit()
print(reg_4.summary())

"""we can see the effect of price gets smaller when we include num_reviews_total
as proxy for number of purchases, since number of purchases is not in the
data... the effect of price might even disapear entirely if we had the number of
purchases as a variable"""
X["median_playtime_forever"] = data_task_1_filtered["median_playtime_forever"]

reg_5 = sm.OLS(y, X).fit()
print(reg_5.summary())