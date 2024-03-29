# -*- coding: utf-8 -*-
"""
Created on Wed Jul 31 16:40:05 2019

@author: FA356935
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


#Import dataset
dataset = pd.read_csv('Restaurant_Reviews.tsv',delimiter ='\t',quoting = 3)

#Cleaning the texts
import re
import nltk
nltk.download('stopwords')
from nltk.corpus import stopwords
from nltk.stem.porter import PorterStemmer
corpus = []
for i in range(len(dataset)):
    review = re.sub('[^a-zA-Z]',' ',dataset['Review'][i])
    review = review.lower()
    review = review.split()
    ps = PorterStemmer()
    review = [ps.stem(word) for word in review if not word in set(stopwords.words('english'))]
    review = ' '.join(review)
    corpus.append(review)

from sklearn.feature_extraction.text import CountVectorizer
cv = CountVectorizer(max_features=1500)
X = cv.fit_transform(corpus).toarray()
y = dataset.iloc[:,1].values

from sklearn.cross_validation import train_test_split
X_train,X_test,y_train,y_test = train_test_split(X, y, test_size=0.2, random_state=0)

from sklearn.naive_bayes import GaussianNB
classifier = GaussianNB()
classifier.fit(X_train,y_train)

y_pred = classifier.predict(X_test)
accuracy = (55+91)/200  #===0.73

# Feature Scaling
from sklearn.preprocessing import StandardScaler
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# Fitting classifier to the Training set
from sklearn.ensemble import RandomForestClassifier
classifier = RandomForestClassifier(n_estimators = 10,
                                    criterion = 'entropy',
                                    random_state = 0)
classifier.fit(X_train,y_train)
# (87+57)/200  => .72

from sklearn.svm import SVC
classifier = SVC(kernel = 'rbf', random_state = 0)
classifier.fit(X_train, y_train)
# (64+83)/200  => .735

from sklearn.neighbors import KNeighborsClassifier
classifier = KNeighborsClassifier(n_neighbors = 5, metric= 'minkowski',p=2)
classifier.fit(X_train,y_train)
## (75+55)/200 ==> 0.65
# Predicting the Test set results
y_pred = classifier.predict(X_test)


from sklearn.metrics import confusion_matrix
cm= confusion_matrix(y_test, y_pred)
