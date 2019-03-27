#!/usr/bin/env python
# coding: utf-8

# In[1]:


#너무 멋진 시각화를 위한 모듈들
from plotly.offline import init_notebook_mode, iplot
import plotly.graph_objs as go
import plotly.plotly as py
from plotly import tools
from datetime import date
import pandas as pd
from pandas import DataFrame
import numpy as np 
import plotly.figure_factory as ff

#모델링을 위한 모듈
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.neighbors import KNeighborsRegressor
import lightgbm as lgb

#그래프를 그리기 위한 모듈
import matplotlib.pyplot as plt
import seaborn as sns
import random 
import warnings
import operator
warnings.filterwarnings("ignore")
init_notebook_mode(connected=True)

import scipy as sp

import copy


# <hr>
# ## Part A : Exploration
# <hr>
# 
# ## 1. Dataset Preparation
# 
# 데이터 셋을 살펴보는 것으로 분석의 기초를 다진다.

# In[2]:


train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")

print ("Train Dataset: Rows, Columns: ", train.shape)
print ("Test Dataset: Rows, Columns: ", test.shape)


# In[190]:


target = DataFrame(train["Target"])

train = train.drop(columns=["Id"])

train = train.drop(columns = ["idhogar"])


# In[191]:


print ("Glimpse of Train Dataset: ")
train.head()


# In[192]:


print ("Summary of Train Dataset: ")
train.describe()


# In[193]:


print ("Top Columns having missing values")
missmap = train.isnull().sum().to_frame().sort_values(0, ascending = False)
missmap.head()


# In[194]:


## SQbage, SQBmeaned, SQBovercrowding, Age, Agesq, 


# In[195]:


train.hist("Target")


# In[196]:


train.hist(bins=50,figsize = (30,30))

plt.show()


# In[197]:


## 상관행렬도 히트맵

trace1 = {
  "x": train.columns, 
  "y": train.columns, 
  "z": 
    train.corr().values, 
  "name": "trace 0", 
  "type": "heatmap", 
  "uid": "0f0d45", 
}
    

fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)
iplot(fig)


# In[198]:


#핸드폰 보유 대수의 히스토그램

trace1 = go.Histogram(name="휴대폰 보유대수", x=train["qmobilephone"])
fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)

fig['layout'].update(height=400, showlegend=False, title="핸드폰 보유 대수 히스토그램")
iplot(fig)
#원-핫-인코딩으로 만들어볼까?


# In[199]:


#총 가구원수 변수의 동일 여부 검정
print((train["tamhog"] == train["hhsize"]).value_counts())
print("=================")
print((train["tamhog"] == train["hogar_total"]).value_counts())
print("=================")
print((train["tamhog"] == train["r4t3"]).value_counts())
print("=================")
print((train["tamhog"] == train["tamviv"]).value_counts())


# In[200]:


#차이나는 총 가구원수 변수들 확인

print((train[train["tamhog"] != train["r4t3"]].loc[:,"r4t3":"tamhog"]).head(5))
print((train[train["tamhog"] != train["tamviv"]].loc[:,['tamhog','tamviv']]).head(5))


# In[201]:


print("=======결측값 확인=======")
print(train["v2a1"].isnull().value_counts())
print("===임대료 결측값의 자가소유 변수와의 상관관계===")
print(train[train["v2a1"].isnull()]["tipovivi1"].value_counts())
print("====자가 소유를 하고 있는 경우 대부분이 결측이다.====")
print(train[train["v2a1"].isnull()]["tipovivi2"].value_counts())
print("===자가 소유지만 대출이 있는 경우 전부 결측이 아니다.===")
print(train[train["v2a1"].isnull()]["tipovivi3"].value_counts())
print("=======임대의 경우 전부 결측이 아니다.=======")
print(train[train["v2a1"].isnull()]["tipovivi4"].value_counts())
print("====불확실의 경우 대부분이 결측이 아니지만, 일부(163건)의 결측치가 존재한다.====")
print(train[train["v2a1"].isnull()]["tipovivi5"].value_counts())
print("====기타의 경우 대부분이 결측이 아니지만, 일부(786)건의 결측치가 존재한다.====")


# ###핸드폰 결측치 처리

# In[202]:


#qmobilephone 변수의 결측 여부 확인
print("보유대수 결측값:",train["qmobilephone"].isnull().value_counts().values)
print("===================")
#mobilephone(핸드폰 보유 여부)변수와의 일치성 여부

#보유한 케이스와 미보유 케이스의 분포
print("보유자의 수:",train["mobilephone"].value_counts().values[0])
print("미 보유자의 수:",train["mobilephone"].value_counts().values[1])
print("===================")
print("핸드폰 보유대수 1이상:",train[train["qmobilephone"] != 0]["mobilephone"].value_counts().values)
print("핸드폰 보유대수 0:",train[train["qmobilephone"] == 0]["mobilephone"].value_counts().values)
## 보유 케이스와 미보유 케이스와 일치하는 것으로 봐서, "미보유인데 보유한 것으로 나타난" 모순된 사례는 없는 것으로 보인다.


# In[203]:


#핸드폰 보유 대수의 히스토그램
trace1 = go.Histogram(name="휴대폰 보유대수", x=train["qmobilephone"])
fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)

fig['layout'].update(height=400, showlegend=False, title="핸드폰 보유 대수 히스토그램")
iplot(fig)
#원-핫-인코딩으로 만들어볼까?


# ###타블렛 결측 처리

# In[204]:


print("v18q1 결측값:",train["v18q1"].isnull().value_counts().values[0])
### 결측값이 엄청 많네


# In[205]:


#핸드폰 보유 대수의 히스토그램
trace1 = go.Histogram(name="타블렛 보유대수", x=train["v18q1"])
fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)

fig['layout'].update(height=400, showlegend=False, title="타블렛 보유 대수 히스토그램")
iplot(fig)
#<0대>가 하나도 없는 것을 볼 수 있다, 0대가 곧 결측일까?


# In[206]:


## 타블렛을 보유하지 않았다고 대답한 사람들은 모두 타블렛 보유 대수가 결측이다.
print("타블렛 보유X 응답자수:",train[train["v18q"] == 0]["v18q1"].isnull().value_counts().values[0])
print("v18q1 결측값:",train["v18q1"].isnull().value_counts().values[0])


# In[207]:


print("타블렛 보유자의 수:",train["v18q"].value_counts().values[0])
print("타블렛 미 보유자의 수:",train["v18q"].value_counts().values[1])
print("타블렛 보유대수 0:",train[train["v18q1"] == 0]["v18q"].value_counts().values)
print("타블렛 보유대수 1 이상:",train[train["v18q1"] != 0]["v18q"].value_counts().values[0])


# In[208]:


### 미보유를 결측처리 했음을 알았으니깐, 전부 0으로 처리해준다.


# In[209]:


## 임대료 결측치의 case1 : 자가소유와 관계가 깊으므로 모든 결측치를 0으로 처리
train_zero = pd.DataFrame()
train_zero = copy.copy(pd.DataFrame(train))
## 얕은 카피로 메모리가 분리되었다.
print(id(train_zero),id(train))


# In[210]:


filtering = ["r4","tamhog","tamviv","hhsize","hogar", "inst", "Id","Target"]
train_filt = list()
train_ind = list()

for i in train_zero.columns:
    for j in filtering:
        if i.startswith(j) is True:
            #필터링 변수에 담는다
            train_filt.append(i)
        else : pass
    if i not in train_filt:
        #필터링 변수와 일치하지 않으면 컬럼 리스트에 담는다.
        train_ind.append(i)
        
train_zero[train_ind].head(5)


# In[211]:


train_zero["v18q1"] = train_zero["v18q1"].fillna(0)


# In[212]:


print("v18q1의 비 결측값 수:",train_zero["v18q1"].isnull().value_counts().values[0])


# In[213]:


## 핸드폰 보유대수를 원-핫으로 처리한다.


# In[214]:


'''
phone_dict = {0:"phone 0",1:"phone 1",2:"phone 2",3:"phone 3",4:"phone 4",5:"phone 5",6:"phone 6",7:"phone 7",8:"phone 8",9:"phone 9",10:"phone 10"}
train['phone_cat'] = train['qmobilephone'].map(phone_dict)

phone_cat = pd.get_dummies(train['phone_cat'])

train_zero = pd.concat([train_zero, phone_cat], axis=1)

del train["phone_cat"]


# In[215]:


### case5 knn으로 결측값을 대치
### knn의 이웃수를 2로 설정하여, knn의 회귀 결과를 토대로 v2a1 결측값을 생산(예측)한다.


# In[216]:


train_zero.head()


# In[217]:


#knn 결측값 제거


filtering = ["dependency","edjefe","edjefa"]
train_filt = list()
train_chr = list()

features = train_zero.columns
num_cols = train_zero._get_numeric_data().columns
cat_cols = list(set(features) - set(num_cols))

for i in features:
    for j in filtering:
        if i.startswith(j) is True:
            #필터링 변수에 담는다
            train_filt.append(i)
    if i not in train_filt:
        if i not in cat_cols:
            if train_zero[i].isnull().any() == False :
            #필터링 변수와 일치하지 않으면 컬럼 리스트에 담는다.
                train_chr.append(i)
        
train_zero[train_chr].head(5)


# In[218]:


train_zero["v2a1"].fillna(train.groupby("Target")["v2a1"].transform("mean"),inplace=True)

train_zero["v2a1"].head(20)


# In[219]:


from sklearn.preprocessing import MinMaxScaler

imput_target = DataFrame()
imput_target = copy.copy(train_zero["v2a1"])

scaler_train = MinMaxScaler()
scaler_target = MinMaxScaler()

scaler_train.fit(train_zero[train_chr])
x_imput = scaler_train.transform(train_zero[train_chr])

scaler_target.fit(DataFrame(imput_target))
x_imput_target = scaler_target.transform(DataFrame(imput_target))

x_imput_target


# In[220]:


imput = KNeighborsRegressor(2)
imput.fit(x_imput,x_imput_target)
x_imput_target_knn = imput.predict(x_imput)


# In[221]:


inversed = scaler_target.inverse_transform(x_imput_target_knn)
train_zero["v2a1"] = copy.copy(DataFrame(inversed))

print(DataFrame(inversed).head())
print("====================")
print(train_zero["v2a1"].head())


# In[224]:


filtering = ["dependency","edjefe","edjefa"]
train_filt = list()
train_chr = list()

features = train_zero.columns
num_cols = train_zero._get_numeric_data().columns
cat_cols = list(set(features) - set(num_cols))

for i in features:
    for j in filtering:
        if i.startswith(j) is True:
            #필터링 변수에 담는다
            train_filt.append(i)
    if i not in train_filt:
        if i not in cat_cols:
            if train_zero[i].isnull().any() == False :
            #필터링 변수와 일치하지 않으면 컬럼 리스트에 담는다.
                train_chr.append(i)
        
train_zero[train_chr].head(5)


# In[225]:


#업 샘플링

from imblearn import *
from imblearn.over_sampling import SMOTE, ADASYN

X_samp, y_samp = SMOTE(random_state=4).fit_sample(train_zero[train_chr],target)


# In[226]:


plt.hist(DataFrame(X_samp)[134])


# In[227]:


train_zero = DataFrame(X_samp,columns = train_zero[train_chr].columns)


# In[228]:


filtering = ["r4","tamhog","tamviv","hhsize","hogar", "inst", "Id","Target"]
train_filt = list()
train_ind = list()

for i in train_zero.columns:
    for j in filtering:
        if i.startswith(j) is True:
            #필터링 변수에 담는다
            train_filt.append(i)
        else : pass
    if i not in train_filt:
        #필터링 변수와 일치하지 않으면 컬럼 리스트에 담는다.
        train_ind.append(i)
        
train_zero[train_ind].head(5)

<hr>

## Part B: Modelling

<hr>

## 8. Baseline Model

### 8.1 train_zero 모형으로 xgboost 적합
# In[236]:


#원본용 타겟변수

## list of features to be used
#features = [c for c in train_zero.columns if c not in ['Id', 'Target']]

## target variable 
target_index = {1:0, 2:1, 3:2, 4:3}
target = np.array([target_index[c] for c in target["Target"]])


# In[184]:


## list of features to be used
#features = [c for c in train_zero.columns if c not in ['Id', 'Target']]

## target variable 
target_index = {1:0, 2:1, 3:2, 4:3}
target_zero = np.array([target_index[c] for c in y_samp])


# ### 8.2 Label Encode the categorical variables 

# ### 8.3 Prepare Training and Validation Sets

# In[232]:


#원본 데이터셋
def label_encoding(col):
    le = LabelEncoder()
    le.fit(list(train[col].values) + list(test[col].values))
    train[col] = le.transform(train[col].astype(str))
    test[col] = le.transform(test[col].astype(str))

features = train.columns
num_cols = train._get_numeric_data().columns
cat_cols = list(set(features) - set(num_cols))
for col in cat_cols:
    label_encoding(col)


# In[233]:


del train["Target"]


# In[239]:


#원본 데이터셋
X_train, X_valid, y_train, y_valid = train_test_split(train[train_ind].values, target, test_size=0.2, random_state=1)
lgb_train = lgb.Dataset(X_train, y_train)
lgb_valid = lgb.Dataset(X_valid, y_valid)


# In[229]:


#수정 데이터셋
X_train, X_valid, y_train, y_valid = train_test_split(train_zero[train_ind].values, target_zero, test_size=0.2, random_state=1)
lgb_train_zero = lgb.Dataset(X_train, y_train)
lgb_valid_zero = lgb.Dataset(X_valid, y_valid)


# In[134]:


from sklearn.metrics import f1_score

def manual_scoring(preds, dtrain):
    labels = dtrain.get_label()
    preds = preds.reshape(-1, 4)  # I should have reshaped pred
    preds = preds.argmax(axis = 1)
    f_score = f1_score(preds, labels, average = 'macro')
    return 'f1_score', f_score, True

evals_result = {}


# str### 8.4 Baseline LightGBM

# In[357]:


from sklearn.model_selection import GridSearchCV


# In[240]:


#껍데기 모델을 만들어준다.

model4 = lgb.LGBMClassifier(boosting_type = "gbdt",objective = "mutliclass",max_depth = -1,random_state = 0)


# In[241]:


gridParams = {
    'learning_rate': [0.01, 0.005],
    'n_estimators': [40,80,100],
    'num_leaves': [10,20,30,40],
    'colsample_bytree' : [0.65, 0.66],
    'subsample' : [0.7,0.75],
    }


grid = GridSearchCV(model4, gridParams,
                    verbose = 0,
                    cv=5,
                    n_jobs=-1)


# In[242]:


grid.fit(train_zero[train_ind], target)


# In[243]:


# 학습률, 트리의 수, 가지의 수의 best_params
# 학습률은 의외로 0.01이 0.005일 때보다 성능이 좋았고
# 학습기의 수는 역시 많으면 많을수록 좋았고
# 허용된 가지의 수는 10개. 좀 의외인 부분
# 표본 샘플링은 70%, 특성 샘플링은 0.65일때 성능이 준수한 것으로 나타났다.

grid.best_params_


# In[248]:


evals_result = dict()

## 원본 데이터셋
params = {'boosting_type': 'gbdt', 'objective': 'multiclass', 'metric': 'multi_logloss',
          'num_class': 4, 'max_depth': -1, 'num_leaves': 10, 'learning_rate': 0.01,
          "n_estimator" : 100000, "subsample" : 0.7, "colsample_bytree" : 0.65,'verbose': -1, 'num_threads': -1 }

model1 = lgb.train(params, lgb_train, num_boost_round=3000,  valid_sets=[lgb_train, lgb_valid], 
                  early_stopping_rounds=1000, verbose_eval=100,feval = manual_scoring,evals_result=evals_result)

lgb.plot_metric(evals_result)


# In[247]:


## 수정 데이터셋
params = {'boosting_type': 'gbdt', 'objective': 'multiclass', 'metric': 'multi_logloss',
          'num_class': 4, 'max_depth': -1, 'num_leaves': 10, 'learning_rate': 0.01,
          "n_estimator" : 100000, "subsample" : 0.7, "colsample_bytree" : 0.65,'verbose': -1, 'num_threads': -1 }

model1 = lgb.train(params, lgb_train_zero, num_boost_round=3000,  valid_sets=[lgb_train_zero, lgb_valid_zero], 
                  early_stopping_rounds=1000, verbose_eval=100,feval = manual_scoring,evals_result=evals_result)

lgb.plot_metric(evals_result)

