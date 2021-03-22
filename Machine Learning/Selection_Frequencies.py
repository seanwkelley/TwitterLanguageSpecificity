#Text feature selection frequencies 
#Input data for Figure 4a and Figure4c 
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


r_list = list()

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


		print(result_sds.coef_)
		print(result_oci.coef_)
		coefs_sds = np.append(coefs_sds,result_sds.coef_.reshape((86,1)),axis = 1)
		coefs_oci = np.append(coefs_oci,result_oci.coef_.reshape((86,1)),axis = 1)
		coefs_ssms = np.append(coefs_ssms,result_ssms.coef_.reshape((86,1)),axis = 1)
		coefs_bis = np.append(coefs_bis,result_bis.coef_.reshape((86,1)),axis = 1)
		coefs_lsas = np.append(coefs_lsas,result_lsas.coef_.reshape((86,1)),axis = 1)
		coefs_eat = np.append(coefs_eat,result_eat.coef_.reshape((86,1)),axis = 1)
		coefs_audit = np.append(coefs_audit,result_audit.coef_.reshape((86,1)),axis = 1)
		coefs_aes = np.append(coefs_aes,result_aes.coef_.reshape((86,1)),axis = 1)
		coefs_stai = np.append(coefs_stai,result_stai.coef_.reshape((86,1)),axis = 1)

	

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
coefs_oci = coefs_oci.drop(coefs_oci.columns[[0]], axis=1) 
coefs_ssms = coefs_ssms.drop(coefs_ssms.columns[[0]], axis=1) 
coefs_bis = coefs_bis.drop(coefs_bis.columns[[0]], axis=1) 
coefs_lsas = coefs_lsas.drop(coefs_lsas.columns[[0]], axis=1) 
coefs_eat = coefs_eat.drop(coefs_eat.columns[[0]], axis=1) 
coefs_audit = coefs_audit.drop(coefs_audit.columns[[0]], axis=1) 
coefs_aes = coefs_aes.drop(coefs_aes.columns[[0]], axis=1) 
coefs_stai = coefs_stai.drop(coefs_stai.columns[[0]], axis=1) 

coefs_sds.index = coefs_oci.index = coefs_ssms.index = coefs_bis.index = coefs_lsas.index = coefs_eat.index = coefs_audit.index = coefs_aes.index = coefs_stai.index = X.iloc[:,0:86].columns 

sds_mean = pd.DataFrame(coefs_sds.mean(axis=1))
oci_mean = pd.DataFrame(coefs_oci.mean(axis=1))
ssms_mean = pd.DataFrame(coefs_ssms.mean(axis=1))
bis_mean = pd.DataFrame(coefs_bis.mean(axis=1))
lsas_mean = pd.DataFrame(coefs_lsas.mean(axis=1))
eat_mean = pd.DataFrame(coefs_eat.mean(axis=1)) 
audit_mean = pd.DataFrame(coefs_audit.mean(axis=1))
aes_mean = pd.DataFrame(coefs_aes.mean(axis=1))
stai_mean = pd.DataFrame(coefs_stai.mean(axis=1)) 


beta_mean = pd.concat([sds_mean,oci_mean,ssms_mean,bis_mean,lsas_mean,eat_mean,audit_mean,aes_mean,stai_mean],axis = 1)
beta_mean.columns = ['SDS','OCI','SSMS','BIS','LSAS','EAT','AUDIT','AES','STAI']

beta_mean.to_csv('ElasticNet/Selection_Frequency_Betas.csv')


coefs_sds[abs(coefs_sds) > 0]  = 1
coefs_oci[abs(coefs_oci) > 0]  = 1
coefs_ssms[abs(coefs_ssms) > 0]  = 1
coefs_bis[abs(coefs_bis) > 0]  = 1
coefs_lsas[abs(coefs_lsas) > 0]  = 1
coefs_eat[abs(coefs_eat) > 0]  = 1
coefs_audit[abs(coefs_audit) > 0]  = 1
coefs_aes[abs(coefs_aes) > 0]  = 1
coefs_stai[abs(coefs_stai) > 0]  = 1


coefs_sds = pd.DataFrame(coefs_sds.sum(axis = 1))/(NUM_TRIALS*10)
coefs_oci = pd.DataFrame(coefs_oci.sum(axis = 1))/(NUM_TRIALS*10)
coefs_ssms = pd.DataFrame(coefs_ssms.sum(axis = 1))/(NUM_TRIALS*10)
coefs_bis = pd.DataFrame(coefs_bis.sum(axis = 1))/(NUM_TRIALS*10)
coefs_lsas = pd.DataFrame(coefs_lsas.sum(axis = 1))/(NUM_TRIALS*10)
coefs_eat = pd.DataFrame(coefs_eat.sum(axis = 1))/(NUM_TRIALS*10)
coefs_audit = pd.DataFrame(coefs_audit.sum(axis = 1))/(NUM_TRIALS*10)
coefs_aes = pd.DataFrame(coefs_aes.sum(axis = 1))/(NUM_TRIALS*10)
coefs_stai = pd.DataFrame(coefs_stai.sum(axis = 1))/(NUM_TRIALS*10)

coefs = pd.concat([coefs_sds,coefs_oci,coefs_ssms,coefs_bis,coefs_lsas,coefs_eat,coefs_audit,coefs_aes,coefs_stai],axis = 1)
coefs.columns = ['SDS','OCI','SSMS','BIS','LSAS','EAT','AUDIT','AES','STAI']

print(coefs)

#coefs.to_csv('ElasticNet/Selection_Frequency.csv')




