import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import confusion_matrix
import statsmodels.api as sm
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve, auc
from sklearn.preprocessing import label_binarize

# Load the dataset
data = pd.read_csv("C:/Users/bismi/OneDrive/Desktop/Credit Risk Files/CreditRisk_Verify.csv")

# Save the variable columns
default_column = data['DEFAULT']
duration_column = data['DURATION']
num_credits_column = data['NUM_CREDITS']
age_column = data['AGE']
install_rate_column = data['INSTALL_RATE']
foreign_column = data['FOREIGN']
amount_column = data['AMOUNT']
other_install_column = data['OTHER_INSTALL']
new_car_column = data['NEW_CAR']
used_car_column = data['USED_CAR']
furniture_column = data['FURNITURE']
radio_tv_column = data['RADIO.TV']
education_column = data['EDUCATION']
retraining_column = data['RETRAINING']
male_div_column = data['MALE_DIV']
male_single_column = data['MALE_SINGLE']
male_mar_div_column = data['MALE_MAR_or_WID']
co_applicant_column = data['CO.APPLICANT']
guarantor_column = data['GUARANTOR']
real_estate_column = data['REAL_ESTATE']
prop_unknown_none_column = data['PROP_UNKN_NONE']
rent_column = data['RENT']
own_res_column = data['OWN_RES']
num_dependents_column = data['NUM_DEPENDENTS']
telephone_column = data['TELEPHONE']

# Explicitly specify levels for categorical variables
data['CHK_ACCT'] = pd.Categorical(data['CHK_ACCT'], categories=["0", "1", "2", "3"], ordered=True)
data['HISTORY'] = pd.Categorical(data['HISTORY'], categories=["0", "1", "2", "3", "4"], ordered=True)
data['SAV_ACCT'] = pd.Categorical(data['SAV_ACCT'], categories=["0", "1", "2", "3", "4"], ordered=True)
data['EMPLOYMENT'] = pd.Categorical(data['EMPLOYMENT'], categories=["0", "1", "2", "3", "4"], ordered=True)
data['PRESENT_RESIDENT'] = pd.Categorical(data['PRESENT_RESIDENT'], categories=["1", "2", "3", "4"], ordered=True)
data['JOB'] = pd.Categorical(data['JOB'], categories=["0", "1", "2", "3"], ordered=True)

# Define the main categorical variables
main_categorical_vars = ["SAV_ACCT", "JOB", "EMPLOYMENT", "HISTORY", "PRESENT_RESIDENT", "CHK_ACCT"]

# Apply one-hot encoding to the categorical variables
data = pd.get_dummies(data, columns=main_categorical_vars, drop_first=True)

# Add back relevant columns
data['DEFAULT'] = default_column
data['DURATION'] = duration_column
data['AGE'] = age_column
data['AMOUNT'] = amount_column
data['NUM_CREDITS'] = num_credits_column
data['INSTALL_RATE'] = install_rate_column
data['FOREIGN'] = foreign_column
data['OTHER_INSTALL'] = other_install_column
data['NEW_CAR'] = new_car_column
data['USED_CAR'] = used_car_column
data['FURNITURE'] = furniture_column
data['RADIO.TV'] = radio_tv_column
data['EDUCATION'] = education_column
data['RETRAINING'] = retraining_column
data['CO.APPLICANT'] = co_applicant_column
data['GUARANTOR'] = guarantor_column
data['REAL_ESTATE'] = real_estate_column
data['PROP_UNKN_NONE'] = prop_unknown_none_column
data['RENT'] = rent_column
data['OWN_RES'] = own_res_column
data['NUM_DEPENDENTS'] = num_dependents_column
data['TELEPHONE'] = telephone_column

# Create a new column "NO_CHK_OR_SAV"
data['NO_CHK_OR_SAV'] = data.apply(lambda row: 1 if (row['CHK_ACCT.3'] + row['SAV_ACCT.4']) > 0 else 0, axis=1)

# Remove the original columns if needed
data = data.drop(columns=["CHK_ACCT.3", "SAV_ACCT.4", "HISTORY.3"])

# Check for Missing Values
print(data.isnull().sum())

# Verify the updated structure
print(data.info())
print(data.describe())

# Correlation
numeric_variables = data.select_dtypes(include='number')
correlation_matrix = numeric_variables.corr()
print(correlation_matrix)

# Correlation with DEFAULT
correlation_with_default = numeric_variables.apply(lambda var: var.corr(data['DEFAULT']))
print(correlation_with_default)

# Variable Selection
top_n_influential = correlation_with_default.abs().nlargest(30).index
print(top_n_influential)

# Feature Selection
exclude_vars = ["RADIO.TV", "USED_CAR", "MALE_DIV", "OWN_RES", "FURNITURE", "TELEPHONE", "MALE_MAR_or_WID", "RETRAINING",
                "CO.APPLICANT", "EDUCATION", "REAL_ESTATE", "NUM_DEPENDENTS", "PRESENT_RESIDENT.4", "EMPLOYMENT.4", "JOB.3"]
data = data.drop(columns=exclude_vars)

# Make sure the "DEFAULT" variable remains a category
data['DEFAULT'] = data['DEFAULT'].astype('category')

# Rebuild the Logistic Regression model
X = data.drop(columns=['DEFAULT'])
y = data['DEFAULT']
logistic_model = sm.Logit(y, sm.add_constant(X))
result = logistic_model.fit()

# Split the data into training and testing sets after data preprocessing
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=123)

# Model
logistic_model_sklearn = LogisticRegression()
logistic_model_sklearn.fit(X_train, y_train)

# Make predictions on the test data
logistic_predictions = logistic_model_sklearn.predict(X_test)

# Confusion Matrix
conf_matrix = confusion_matrix(y_test, logistic_predictions)
print(conf_matrix)

# ROC Curve
y_probs = logistic_model_sklearn.predict_proba(X_test)[:, 1]
fpr, tpr, _ = roc_curve(y_test, y_probs)
roc_auc = auc(fpr, tpr)

# Plot ROC Curve
plt.figure()
plt.plot(fpr, tpr, color='darkorange', lw=2, label='ROC curve (area = %0.2f)' % roc_auc)
plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Receiver Operating Characteristic (ROC) Curve')
plt.legend(loc="lower right")
plt.show()
