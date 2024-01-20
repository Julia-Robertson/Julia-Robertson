import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.naive_bayes import GaussianNB
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import confusion_matrix, accuracy_score
from yellowbrick.classifier import ConfusionMatrix

# Load dataset
credit_data = pd.read_csv('Credit.csv')

# Identify categorical attributes (type 'Object')
attributes_to_encode = []
for column in credit_data.columns:
    if credit_data[column].dtype == 'O':
        attributes_to_encode.append(column)

# Remove the "class" attribute from the list of attributes for encoding
attributes_to_encode.remove('class')

# Encode 'Object' type attributes for the model
label_encoder = LabelEncoder()
for attribute in attributes_to_encode:
    credit_data[attribute] = label_encoder.fit_transform(credit_data[attribute])

# Define predictor attributes and the class attribute
predictors = credit_data.iloc[:, 0:20]
target_class = credit_data.iloc[:, 20]

# Form training and test dataset
X_train, X_test, y_train, y_test = train_test_split(predictors, target_class, test_size=0.3, random_state=0)

# Model training
naive_bayes = GaussianNB()
naive_bayes.fit(X_train, y_train)

# Test the model
predictions = naive_bayes.predict(X_test)
confusion = confusion_matrix(y_test, predictions)
accuracy_rate = accuracy_score(y_test, predictions)
error_rate = 1 - accuracy_rate

# Visualization of Machine Learning models
visualizer = ConfusionMatrix(GaussianNB())
visualizer.fit(X_train, y_train)
visualizer.score(X_test, y_test)
visualizer.poof()

# Simulating the model in production
# Load data for prediction
new_credit = pd.read_csv('NewCredit.csv')

# Identify categorical attributes (type 'Object')
attributes_to_encode_production = []
for i in list(new_credit.columns):
    if new_credit[i].dtype == 'O':
        attributes_to_encode_production.append(i)

# Encoder for 'Object' type attributes to use the model
label_encoder = LabelEncoder()
for i in attributes_to_encode_production:
    new_credit[i] = label_encoder.fit_transform(new_credit[i])

# Prediction
naive_bayes.predict(new_credit)
