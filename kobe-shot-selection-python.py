# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.neural_network import MLPClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
from sklearn.metrics import confusion_matrix
from sklearn.model_selection import GridSearchCV
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import cross_val_score
from sklearn.metrics import log_loss
from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import PolynomialFeatures
from xgboost.sklearn import XGBModel
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

import os
print(os.listdir("../input"))

## Reading data into environment
data = pd.read_csv("../input/data.csv")
data.isnull().sum()

# Shot ID's for submission data
shot_ids = (data[data.shot_made_flag.isnull() == True]).shot_id

## Adding a year column
data['yr'] = data.game_date[:4]
## Adding column representing if Lakers are visitng another Team
data['is_away'] = data.matchup.str.contains('@').astype(int)

## Adding a column for total time remaining
data['total_time'] = data.seconds_remaining + (60*data.minutes_remaining)+0.00001

## Distance * Time Remaining
data['Super-Feature1'] = np.log(data.total_time)

#Dropping stuff
data = data.drop(['season', 'combined_shot_type','game_id','lat','lon',
'team_id','team_name','game_date','matchup','shot_id','shot_zone_area','shot_zone_basic','shot_zone_range'], axis = 1)

#One Hot Encoding
data = pd.get_dummies(data, columns = ['yr','action_type','shot_distance','minutes_remaining','period','playoffs','shot_type','opponent'])

# Seperating submission data out
submission_data = data[data.shot_made_flag.isnull() == True]
data = data[data.shot_made_flag.isnull() == False]

submission_data_X = submission_data.drop(['shot_made_flag'], axis =1)
submission_data_y = submission_data.shot_made_flag

data_X = data.drop(['shot_made_flag'], axis =1 )
data_y = data.shot_made_flag


#Train-Test split
X_train, X_test, y_train, y_test = train_test_split(data_X,data_y, random_state = 11230, test_size = 0.3)

# Modelling with Logit
model = LogisticRegression(penalty = 'l2',C=0.5) # Hyper params found through Grid Search shown below
model.fit(X = X_train,y = y_train)
confusion_matrix(y_true = y_test, y_pred = model.predict(X_test))
roc_auc_score(y_true = y_test, y_score = model.predict_proba(X_test)[:,1])
log_loss(y_true = y_test, y_pred = model.predict_proba(X_test)[:,1])

# Modelling with XGBoost
model_XGB = XGBModel()
model_XGB.fit(X_train, y_train)
log_loss(y_true = y_test, y_pred = model_XGB.predict(X_test))
roc_auc_score(y_true = y_test, y_score = model_XGB.predict(X_test))

## Grid Search with Logit
#param_grid = {'penalty':['l1','l2'],'C': np.arange(0.1, 2.0, 0.025)}
#GS = GridSearchCV(model, param_grid, cv = 5, scoring = 'roc_auc')
#GS.fit(X_train, y_train)

# Submission
submission= pd.DataFrame({'shot_id':shot_ids, 'shot_made_flag': model.predict(submission_data_X)})
submission.to_csv(index = False, path_or_buf ='submission.csv')