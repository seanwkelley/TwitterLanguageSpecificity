#Elastic Net trained on Anxious Depression and tested on other transdiagnostic dimensions 
from numpy import mean
from numpy import median
import sklearn.datasets as dt
from numpy import std
import scipy
from sklearn.model_selection import train_test_split
from sklearn.model_selection import StratifiedKFold
from sklearn.datasets import make_classification
from sklearn.model_selection import KFold
from sklearn.model_selection import GridSearchCV
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import accuracy_score
import pandas as pd
import os
from sklearn import preprocessing
import numpy as np
from matplotlib import pyplot
from sklearn.utils import shuffle
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import mean_squared_error
from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV
from sklearn.linear_model import ElasticNet
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import cross_validate
from sklearn import metrics
from sklearn.metrics import r2_score

def npv_specificity(y_test, y_pred_test):
    # Note: More parameters are defined than necessary. 
    # This would allow return of other measures other than sensitivity and specificity
    
    # Get true/false for whether a breach actually occurred
    actual_pos = y_test == 1
    actual_neg = y_test == 0
    
    # Get true and false test (true test match actual, false tests differ from actual)
    true_pos = (y_pred_test == 1) & (actual_pos)
    false_pos = (y_pred_test == 1) & (actual_neg)
    true_neg = (y_pred_test == 0) & (actual_neg)
    false_neg = (y_pred_test == 0) & (actual_pos)
    
    # Calculate sensitivity and specificity
    npv = np.sum(true_neg) / np.sum(true_neg + false_neg)
    specificity = np.sum(true_neg) / np.sum(actual_neg)
    
    return npv, specificity


os.chdir('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter')



#load datasets
df = pd.read_csv("ElasticNet/mean_features_td.csv")

#feature scaling of LIWC text features 
df.iloc[:,0:87] =  preprocessing.scale(df.iloc[:,0:87])

#feature scaling of questionnaire scales
df.iloc[:,0:86][df.iloc[:,0:86] > 3] = 3
df.iloc[:,0:86][df.iloc[:,0:86] < -3] = -3

df.to_numpy()

X = df.iloc[:,0:88]
y = df.iloc[:,88:91]

print(y)

#70% training set for hyperparameter validation and an independent 30% test set 
X, X_final_test, y, y_final_test = train_test_split(
     X, y, stratify = X['Gender'],test_size=0.30, random_state=42)

#reset indices after train/test spliting 
X = X.reset_index(drop=True); y = y.reset_index(drop=True) 
X_final_test = X_final_test.reset_index(drop=True); y_final_test = y_final_test.reset_index(drop=True) 


r_list_ad = list(); r_list_cit = list(); r_list_sw = list()

betas = np.empty(shape=(86,1))
inter = list()

NUM_TRIALS = 100
for i in range(NUM_TRIALS):
	print(i)

	#shuffle the text features 
	#X.iloc[:,0:86] = X.iloc[:,0:86].sample(frac=1).reset_index(drop=True)

	# configure the cross-validation procedure
	#stratify outer loops by gender, to ensure gender split in folds is similar to overall representation in full sample
	cv_outer = StratifiedKFold(n_splits=10, shuffle=True, random_state=i) 
	# enumerate splits

	outer_results = list()
	for train_ix, test_ix in cv_outer.split(X,X['Gender']):

		#only use text features in the model 
		X_train, X_test = X.iloc[train_ix, 0:86], X.iloc[test_ix, 0:86]
		y_train, y_test = y.iloc[train_ix], y.iloc[test_ix]
		# configure the cross-validation procedure
		cv_inner = KFold(n_splits=5, shuffle=True, random_state=i)
		# define the model
		alphas = np.linspace(0.01,1,30)
		l1_ratio = np.linspace(0.1,1,30)
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=0)

		#fit model 
		result = search.fit(X_train, y_train['AD_score'])
		target_pred = result.predict(X_test)

		inter.append(result.intercept_)

		coefs = result.coef_.reshape((86,1))
		betas = np.append(betas,coefs,axis = 1)

		#R squared value from hold out fold 
		rsquare_ad = result.score(X_test,y_test['AD_score'])
		rsquare_cit = result.score(X_test,y_test['CIT_score'])
		rsquare_sw = result.score(X_test,y_test['SW_score'])


		# store the result
		outer_results.append(rsquare_ad)

		r_list_ad.append(rsquare_ad)
		r_list_cit.append(rsquare_cit)
		r_list_sw.append(rsquare_sw)

		# report progress
		print('>rsquare=%.3f' % (rsquare_ad))
	# summarize the estimated performance of the model
	print('R Squared: %.3f (%.3f)' % (median(outer_results), std(outer_results)))

best_parameters = pd.DataFrame(
    {'r2_ad': r_list_ad,
     'r2_cit': r_list_cit,
     'r2_sw': r_list_sw,
    })


tuned = best_parameters['r2_ad'].idxmax(axis = 0)

betas = pd.DataFrame(betas)
betas = betas.drop(betas.columns[[0]], axis=1) 
betas.index = X.iloc[:,0:86].columns 


final_betas = betas.iloc[:,tuned]
ad_test = X_final_test.iloc[:,0:86].dot(final_betas) + inter[tuned]


ad_score = r2_score(y_final_test['AD_score'],ad_test)
cit_score = r2_score(y_final_test['CIT_score'],ad_test)
sw_score = r2_score(y_final_test['SW_score'],ad_test)

test_scores = pd.DataFrame(
    {'AD': ad_score,
     'CIT': cit_score,
     'SW': sw_score,
    }, index=[0])




print(test_scores)
#print(classification_parameters)
test_scores.to_csv('ElasticNet/tf_only_td.csv',index = False)
