#Train elastic net on depression and test on other psychiatric dimensions using only text features
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
df = pd.read_csv("ElasticNet/mean_features.csv")

#feature scaling of LIWC text features 
df.iloc[:,0:87] =  preprocessing.scale(df.iloc[:,0:87])

#feature scaling of questionnaire scales
df.iloc[:,88:97] =  preprocessing.scale(df.iloc[:,88:97])
df.iloc[:,0:86][df.iloc[:,0:86] > 3] = 3
df.iloc[:,0:86][df.iloc[:,0:86] < -3] = -3

df.to_numpy()

X = df.iloc[:,0:88]
y = df.iloc[:,88:98]

print(X)

#70% training set for hyperparameter validation and an independent 30% test set 
X, X_final_test, y, y_final_test = train_test_split(
     X, y, stratify = X['Gender'],test_size=0.30, random_state=42)

#reset indices after train/test spliting 
X = X.reset_index(drop=True); y = y.reset_index(drop=True) 
X_final_test = X_final_test.reset_index(drop=True); y_final_test = y_final_test.reset_index(drop=True) 


r_list = list(); r_list_oci = list(); r_list_ssms = list()
r_list_bis = list(); r_list_lsas = list(); r_list_eat = list()
r_list_audit = list(); r_list_aes = list(); r_list_stai = list()
l1_list = list()
alpha_list = list()

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
		result = search.fit(X_train, y_train['SDS_total'])
		target_pred = result.predict(X_test)

		inter.append(result.intercept_)

		coefs = result.coef_.reshape((86,1))
		betas = np.append(betas,coefs,axis = 1)

		#R squared value from hold out fold 
		rsquare = result.score(X_test,y_test['SDS_total'])
		rsquare_oci = result.score(X_test,y_test['OCI_total'])
		rsquare_ssms = result.score(X_test,y_test['SSMS_total'])
		rsquare_bis = result.score(X_test,y_test['BIS_total'])
		rsquare_lsas = result.score(X_test,y_test['LSAS_total'])
		rsquare_eat = result.score(X_test,y_test['EAT_total'])
		rsquare_audit = result.score(X_test,y_test['AUDIT_total'])
		rsquare_aes = result.score(X_test,y_test['AES_total'])
		rsquare_stai = result.score(X_test,y_test['STAI_total'])


		# store the result
		outer_results.append(rsquare)

		r_list.append(rsquare)
		r_list_oci.append(rsquare_oci)
		r_list_ssms.append(rsquare_ssms)
		r_list_bis.append(rsquare_bis)
		r_list_lsas.append(rsquare_lsas)
		r_list_eat.append(rsquare_eat)
		r_list_audit.append(rsquare_audit)
		r_list_aes.append(rsquare_aes)
		r_list_stai.append(rsquare_stai)

		# report progress
		print('>rsquare=%.3f' % (rsquare))
	# summarize the estimated performance of the model
	print('R Squared: %.3f (%.3f)' % (median(outer_results), std(outer_results)))

best_parameters = pd.DataFrame(
    {'r2_sds': r_list,
     'r2_oci': r_list_oci,
     'r2_ssms': r_list_ssms,
     'r2_bis': r_list_bis,
     'r2_lsas': r_list_lsas,
     'r2_eat': r_list_eat,
     'r2_audit': r_list_audit,
     'r2_aes': r_list_aes,
     'r2_stai': r_list_stai
    })


tuned = best_parameters['r2_sds'].idxmax(axis = 0)

betas = pd.DataFrame(betas)
betas = betas.drop(betas.columns[[0]], axis=1) 
betas.index = X.iloc[:,0:86].columns 
betas.to_csv('ElasticNet/betas.csv',index = False)


final_betas = betas.iloc[:,tuned]
print(final_betas.shape)
print(X_final_test.iloc[:,0:86].shape)


sds_test = X_final_test.iloc[:,0:86].dot(final_betas) + inter[tuned]


sds_score = r2_score(y_final_test['SDS_total'],sds_test)
oci_score = r2_score(y_final_test['OCI_total'],sds_test)
ssms_score = r2_score(y_final_test['SSMS_total'],sds_test)
bis_score = r2_score(y_final_test['BIS_total'],sds_test)
lsas_score = r2_score(y_final_test['LSAS_total'],sds_test)
eat_score = r2_score(y_final_test['EAT_total'],sds_test)
audit_score = r2_score(y_final_test['AUDIT_total'],sds_test)
aes_score = r2_score(y_final_test['AES_total'],sds_test)
stai_score = r2_score(y_final_test['STAI_total'],sds_test)

test_scores = pd.DataFrame(
    {'sds': sds_score,
     'oci': oci_score,
     'ssms': ssms_score,
     'bis': bis_score,
     'lsas': lsas_score,
     'eat': eat_score,
     'audit': audit_score,
     'aes': aes_score,
     'stai': stai_score
    }, index=[0])




print(test_scores)
#print(classification_parameters)
#test_scores.to_csv('ElasticNet/tf_only_25_50p.csv',index = False)
