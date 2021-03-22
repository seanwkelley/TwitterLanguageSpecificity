#Find best performing model for each psychiatric scale trained and tested on that scale
#Input data for Figure 4b
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
from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV
from sklearn.linear_model import ElasticNet
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import cross_validate
from sklearn import metrics
from sklearn.metrics import r2_score

os.chdir('C:/Users/seanw/TCDUD.onmicrosoft.com/Claire Gillan - Gillan Lab Resources/Projects/Transdiagnostic_Twitter')

#load datasets
df = pd.read_csv("ElasticNet/mean_features.csv")


#feature scaling of LIWC text features 
df.iloc[:,0:87] =  preprocessing.scale(df.iloc[:,0:87])

#feature scaling of questionnaire scales
df.iloc[:,88:97] =  preprocessing.scale(df.iloc[:,88:97])

#
df.iloc[:,0:86][df.iloc[:,0:86] > 3] = 3
df.iloc[:,0:86][df.iloc[:,0:86] < -3] = -3

df.to_numpy()

X = df.iloc[:,0:88]
y = df.iloc[:,88:97]

print(y)

#70% training set for hyperparameter validation and a 30% test set 
X, X_final_test, y, y_final_test = train_test_split(
     X, y, stratify = X['Gender'],test_size=0.30, random_state=42)

#reset indices after train/test spliting 
X = X.reset_index(drop=True); y = y.reset_index(drop=True) 
X_final_test = X_final_test.reset_index(drop=True); y_final_test = y_final_test.reset_index(drop=True) 

r_list_sds = list(); r_list_oci = list(); r_list_ssms = list()
r_list_bis = list(); r_list_lsas = list(); r_list_eat = list()
r_list_audit = list(); r_list_aes = list(); r_list_stai = list()

inter_sds = list(); inter_oci = list(); inter_ssms = list()
inter_bis = list(); inter_lsas = list(); inter_eat = list()
inter_audit = list(); inter_aes = list(); inter_stai = list()


coefs_sds =  np.empty(shape=(86,1))
coefs_oci =  np.empty(shape=(86,1))
coefs_ssms = np.empty(shape=(86,1))
coefs_bis =  np.empty(shape=(86,1)) 
coefs_lsas = np.empty(shape=(86,1))
coefs_eat =  np.empty(shape=(86,1))
coefs_audit =  np.empty(shape=(86,1))
coefs_aes = np.empty(shape=(86,1))
coefs_stai =  np.empty(shape=(86,1))

NUM_TRIALS = 100
for i in range(NUM_TRIALS):
	print(i)
	# configure the cross-validation procedure
	#stratify outer loops by gender, to ensure gender split in folds is similar to overall representation in full sample
	cv_outer = StratifiedKFold(n_splits=10, shuffle=True, random_state=i) 
	# enumerate splits

	outer_results = list()
	for train_ix, test_ix in cv_outer.split(X,X['Gender']):

		X_train, X_test =  X.iloc[train_ix, 0:86], X.iloc[test_ix, 0:86]
		y_train, y_test = y.iloc[train_ix], y.iloc[test_ix]

		print(y_train['OCI_total'])
		# configure the cross-validation procedure
		cv_inner = KFold(n_splits=5, shuffle=True, random_state=i)
		#cv_inner = StratifiedKFold(n_splits=5, shuffle=True, random_state=i) 
		# define the model
		alphas = np.linspace(0.01,1,30)
		l1_ratio = np.linspace(0.1,1,30)
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		#fit model 
		result_sds = search.fit(X_train, y_train['SDS_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_oci = search.fit(X_train, y_train['OCI_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_ssms = search.fit(X_train, y_train['SSMS_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_bis = search.fit(X_train, y_train['BIS_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_lsas = search.fit(X_train, y_train['LSAS_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_eat = search.fit(X_train, y_train['EAT_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_audit = search.fit(X_train, y_train['AUDIT_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_aes = search.fit(X_train, y_train['AES_total'])
		search = ElasticNetCV(l1_ratio=l1_ratio,alphas=alphas, precompute='auto', max_iter=100000, cv=cv_inner, copy_X=True, verbose=1)
		result_stai = search.fit(X_train, y_train['STAI_total'])

		inter_sds.append(result_sds.intercept_)
		inter_oci.append(result_oci.intercept_)
		inter_ssms.append(result_ssms.intercept_)
		inter_bis.append(result_bis.intercept_)
		inter_lsas.append(result_lsas.intercept_)
		inter_eat.append(result_eat.intercept_)
		inter_audit.append(result_audit.intercept_)
		inter_aes.append(result_aes.intercept_)
		inter_stai.append(result_stai.intercept_)


		coefs_sds = np.append(coefs_sds,result_sds.coef_.reshape((86,1)),axis = 1)
		coefs_oci = np.append(coefs_oci,result_oci.coef_.reshape((86,1)),axis = 1)
		coefs_ssms = np.append(coefs_ssms,result_ssms.coef_.reshape((86,1)),axis = 1)
		coefs_bis = np.append(coefs_bis,result_bis.coef_.reshape((86,1)),axis = 1)
		coefs_lsas = np.append(coefs_lsas,result_lsas.coef_.reshape((86,1)),axis = 1)
		coefs_eat = np.append(coefs_eat,result_eat.coef_.reshape((86,1)),axis = 1)
		coefs_audit = np.append(coefs_audit,result_audit.coef_.reshape((86,1)),axis = 1)
		coefs_aes = np.append(coefs_aes,result_aes.coef_.reshape((86,1)),axis = 1)
		coefs_stai = np.append(coefs_stai,result_stai.coef_.reshape((86,1)),axis = 1)


		rsquare_sds = result_sds.score(X_test,y_test['SDS_total'])
		rsquare_oci = result_oci.score(X_test,y_test['OCI_total'])
		rsquare_ssms = result_ssms.score(X_test,y_test['SSMS_total'])
		rsquare_bis = result_bis.score(X_test,y_test['BIS_total'])
		rsquare_lsas = result_lsas.score(X_test,y_test['LSAS_total'])
		rsquare_eat = result_eat.score(X_test,y_test['EAT_total'])
		rsquare_audit = result_audit.score(X_test,y_test['AUDIT_total'])
		rsquare_aes = result_aes.score(X_test,y_test['AES_total'])
		rsquare_stai = result_stai.score(X_test,y_test['STAI_total'])

		# store the result
		outer_results.append(rsquare_sds)

		r_list_sds.append(rsquare_sds)
		r_list_oci.append(rsquare_oci)
		r_list_ssms.append(rsquare_ssms)
		r_list_bis.append(rsquare_bis)
		r_list_lsas.append(rsquare_lsas)
		r_list_eat.append(rsquare_eat)
		r_list_audit.append(rsquare_audit)
		r_list_aes.append(rsquare_aes)
		r_list_stai.append(rsquare_stai)

	# report progress
		print('>rsquare=%.3f' % (rsquare_sds))
	# summarize the estimated performance of the model
	print('R Squared: %.3f (%.3f)' % (median(outer_results), std(outer_results)))


coefs_sds = pd.DataFrame(coefs_sds)
coefs_oci = pd.DataFrame(coefs_oci)
coefs_ssms = pd.DataFrame(coefs_ssms)
coefs_bis = pd.DataFrame(coefs_bis)
coefs_lsas = pd.DataFrame(coefs_lsas)
coefs_eat = pd.DataFrame(coefs_eat)
coefs_audit = pd.DataFrame(coefs_audit)
coefs_aes = pd.DataFrame(coefs_aes)
coefs_stai = pd.DataFrame(coefs_stai)


coefs_sds = coefs_sds.drop(coefs_sds.columns[[0]], axis=1) 
coefs_sds.index = X.iloc[:,0:86].columns 

coefs_oci = coefs_oci.drop(coefs_oci.columns[[0]], axis=1) 
coefs_oci.index = X.iloc[:,0:86].columns 

coefs_ssms = coefs_ssms.drop(coefs_ssms.columns[[0]], axis=1) 
coefs_ssms.index = X.iloc[:,0:86].columns 

coefs_bis = coefs_bis.drop(coefs_bis.columns[[0]], axis=1) 
coefs_bis.index = X.iloc[:,0:86].columns 

coefs_lsas = coefs_lsas.drop(coefs_lsas.columns[[0]], axis=1) 
coefs_lsas.index = X.iloc[:,0:86].columns 

coefs_eat = coefs_eat.drop(coefs_eat.columns[[0]], axis=1) 
coefs_eat.index = X.iloc[:,0:86].columns 

coefs_audit = coefs_audit.drop(coefs_audit.columns[[0]], axis=1) 
coefs_audit.index = X.iloc[:,0:86].columns 

coefs_aes = coefs_aes.drop(coefs_aes.columns[[0]], axis=1) 
coefs_aes.index = X.iloc[:,0:86].columns 

coefs_stai = coefs_stai.drop(coefs_stai.columns[[0]], axis=1) 
coefs_stai.index = X.iloc[:,0:86].columns 

best_parameters = pd.DataFrame(
    {'r2_sds': r_list_sds,
     'r2_oci': r_list_oci,
     'r2_ssms': r_list_ssms,
     'r2_bis': r_list_bis,
     'r2_lsas': r_list_lsas,
     'r2_eat': r_list_eat,
     'r2_audit': r_list_audit,
     'r2_aes': r_list_aes,
     'r2_stai': r_list_stai
    })


tuned_sds = best_parameters['r2_sds'].idxmax(axis = 0)
tuned_oci = best_parameters['r2_oci'].idxmax(axis = 0)
tuned_ssms = best_parameters['r2_ssms'].idxmax(axis = 0)
tuned_bis = best_parameters['r2_bis'].idxmax(axis = 0)
tuned_lsas = best_parameters['r2_lsas'].idxmax(axis = 0)
tuned_eat = best_parameters['r2_eat'].idxmax(axis = 0)
tuned_audit = best_parameters['r2_audit'].idxmax(axis = 0)
tuned_aes = best_parameters['r2_aes'].idxmax(axis = 0)
tuned_stai = best_parameters['r2_stai'].idxmax(axis = 0)


sds_test = X_final_test.iloc[:,0:86].dot(coefs_sds.iloc[:,tuned_sds]) + inter_sds[tuned_sds]
oci_test = X_final_test.iloc[:,0:86].dot(coefs_oci.iloc[:,tuned_oci]) + inter_oci[tuned_oci]
ssms_test = X_final_test.iloc[:,0:86].dot(coefs_ssms.iloc[:,tuned_ssms]) + inter_ssms[tuned_ssms]
bis_test = X_final_test.iloc[:,0:86].dot(coefs_bis.iloc[:,tuned_bis]) + inter_bis[tuned_bis]
lsas_test = X_final_test.iloc[:,0:86].dot(coefs_lsas.iloc[:,tuned_lsas]) + inter_lsas[tuned_lsas]
eat_test = X_final_test.iloc[:,0:86].dot(coefs_eat.iloc[:,tuned_eat]) + inter_eat[tuned_eat]
audit_test = X_final_test.iloc[:,0:86].dot(coefs_audit.iloc[:,tuned_audit]) + inter_audit[tuned_audit]
aes_test = X_final_test.iloc[:,0:86].dot(coefs_aes.iloc[:,tuned_aes]) + inter_aes[tuned_aes]
stai_test = X_final_test.iloc[:,0:86].dot(coefs_stai.iloc[:,tuned_stai]) + inter_stai[tuned_stai]

sds_score = r2_score(y_final_test['SDS_total'],sds_test)
oci_score = r2_score(y_final_test['OCI_total'],oci_test)
ssms_score = r2_score(y_final_test['SSMS_total'],ssms_test)
bis_score = r2_score(y_final_test['BIS_total'],bis_test)
lsas_score = r2_score(y_final_test['LSAS_total'],lsas_test)
eat_score = r2_score(y_final_test['EAT_total'],eat_test)
audit_score = r2_score(y_final_test['AUDIT_total'],audit_test)
aes_score = r2_score(y_final_test['AES_total'],aes_test)
stai_score = r2_score(y_final_test['STAI_total'],stai_test)

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
test_scores.to_csv('ElasticNet/tf_only_all.csv',index = False)
