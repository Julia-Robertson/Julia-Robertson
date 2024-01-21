# importing required libraries
import os
import subprocess
import stat
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from datetime import datetime
sns.set(style="white")

# absolute path till parent folder
abs_path = os.getcwd()
path_array = abs_path.split("/")
path_array = path_array[:len(path_array)-1]
homefolder_path = ""
for i in path_array[1:]:
    homefolder_path = homefolder_path + "/" + i   

# path to clean data
clean_data_path = homefolder_path + "/CleanData/CleanedDataSet/cleaned_autos.csv"

# reading csv into raw dataframe
df = pd.read_csv(clean_data_path,encoding="latin-1")

# ## Distribution of Vehicles based on Year of Registration
# Distribution of vehicles based on year of registration
fig, ax = plt.subplots(figsize=(8,6))
sns.distplot(df["yearOfRegistration"], color="#33cc33",kde=True, ax=ax)
ax.set_title('Distribution of vehicles based on Year of Registration', fontsize= 15)
plt.ylabel("Density (KDE)", fontsize= 15)
plt.xlabel("Year Of Registration", fontsize= 15)
plt.show()

# saving the plot
fig.savefig(abs_path + "/Plots/vehicle-distribution.png")

# ## Variation of the price range by the vehicle type
# Boxplot to see the distribution after outliers has been removed
sns.set_style("whitegrid")
fig, ax = plt.subplots(figsize=(8,6))
sns.boxplot(x="vehicleType", y="price", data=df)
ax.text(5.25,27000,"Boxplot After removing outliers",fontsize=18,color="r",ha="center", va="center")
plt.show()

# saving the plot
fig.savefig(abs_path + "/Plots/price-vehicleType-boxplot.png")

##################################### Analysis2 ##############################################

# ## Total count of vehicles by type available on ebay for sale
# Count plot to show the number of vehicles belonging to each vehicleType
sns.set_style("white")
g = sns.factorplot(x="vehicleType", data=df, kind="count",
                   palette="BuPu", size=6, aspect=1.5)
# to get the counts on the top heads of the bar
for p in g.ax.patches:
    g.ax.annotate((p.get_height()), (p.get_x()+0.1, p.get_height()+500))

# saving the plot
g.savefig(abs_path + "/Plots/count-vehicleType.png")

# ## No of Vehicles by Brand Available on ebay for sale
# Count plot to show the number of vehicles belonging to each brand
sns.set_style("whitegrid")
g = sns.factorplot(y="brand", data=df, kind="count",
                   palette="Reds_r", size=7, aspect=1.5)
g.ax.set_title("Count of vehicles by Brand",fontdict={'size':18})
# for p in g.ax.patches:
#      g.ax.annotate((p.get_width()), (p.get_width()-0.1, p.get_y()-0.1))

# saving the plot
g.savefig((abs_path + "/Plots/brand-vehicleCount.png"))

# ## Average price for vehicles based on the type of vehicle as well as on the type of gearbox
fig, ax = plt.subplots(figsize=(8,5))
colors = ["#00e600", "#ff8c1a","#a180cc"]
sns.barplot(x="vehicleType", y="price",hue="gearbox", palette=colors, data=df)
ax.set_title("Average price of vehicles by vehicle type and gearbox type")
plt.show()

# saving the plot
fig.savefig((abs_path + "/Plots/vehicletype-gearbox-price.png"))

############################################# Analysis3 #########################################

# ## Average price of vehicle by fuel type and gearbox type
# barplot for price based on fuel type and gearbox type
fig, ax = plt.subplots(figsize=(8,5))
colors = ["#00e600", "#ff8c1a","#a180cc"]
sns.barplot(x="fuelType", y="price",hue="gearbox", palette="husl",data=df)
ax.set_title("Average price of vehicles by fuel type and gearbox type")
plt.show()

# saving the plot
fig.savefig((abs_path + "/Plots/vehicletype-fueltype-price.png"))

# ## Average power of a vehicle by vehicle type and gearbox type 
# barplot for price based on fuel type and gearbox type
colors = ["windows blue", "amber", "greyish", "faded green", "dusty purple"]
fig, ax = plt.subplots(figsize=(8,5))
sns.set_palette(sns.xkcd_palette(colors))
sns.barplot(x="vehicleType", y="powerPS",hue="gearbox",data=df)
ax.set_title("Average price of vehicles by fuel type and gearbox type")
plt.show()

# saving the plot
fig.savefig((abs_path + "/Plots/vehicletype-fueltype-power.png"))

############################################ Analysis4 ##########################################

trial = pd.DataFrame()
for b in list(df["brand"].unique()):
    for v in list(df["vehicleType"].unique()):
        z = df[(df["brand"] == b) & (df["vehicleType"] == v)]["price"].mean()
        trial = trial.append(pd.DataFrame({'brand':b , 'vehicleType':v , 'avgPrice':z}, index=[0]))
trial = trial.reset_index()
del trial["index"]
trial["avgPrice"].fillna(0,inplace=True)
trial["avgPrice"].isnull().value_counts()
trial["avgPrice"] = trial["avgPrice"].astype(int)
trial.head(5)

# ## Average price of a vehicle by brand as well as vehicle type 
# HeatMap tp show average prices of vehicles by brand and type together
tri = trial.pivot("brand","vehicleType", "avgPrice")
fig, ax = plt.subplots(figsize=(15,20))
sns.heatmap(tri,linewidths=1,cmap="YlGnBu",annot=True, ax=ax, fmt="d")
ax.set_title("Average price of vehicles by vehicle type and brand",fontdict={'size':18})
plt.show()

fig.savefig((abs_path + "/Plots/heatmap-price-brand-vehicleType.png"))

df.head(5)

############################################# Analysis5 ###########################################

# concatinating files of the same brand 
search_term = str(sys.argv[1])
# search_term = "audi"
path = homefolder_path + "/CleanData/DataForAnalysis/" + search_term # use your path
allFiles = glob.glob(path + "/*.csv")
frame = pd.DataFrame()
list_ = []
for file_ in allFiles:
    df = pd.read_csv(file_,index_col=None, header=0)
    list_.append(df)
frame = pd.concat(list_)

frame.head(2)

# colors = ["#47d147", "#ff8c1a","#a180cc"]
colors = ["windows blue", "amber", "greyish", "faded green", "dusty purple"]
fig, ax = plt.subplots(figsize=(8,5))
sns.set_palette(sns.xkcd_palette(colors))
sns.stripplot(x="vehicleType", y="NoOfDaysOnline", hue="gearbox", split=True, data=frame,size=8, alpha=0.5, jitter=True)
ax.set_title("No of days a add is online before the vehicles of brand " + search_term + " is sold")
plt.show()

fig.savefig((abs_path + "/Plots/vehicletype-NoOfDaysOnline.png"))
