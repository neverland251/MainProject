#!/usr/bin/env python
# coding: utf-8

# In[97]:


#너무 멋진 시각화를 위한 모듈들
from plotly.offline import init_notebook_mode, iplot
import plotly.graph_objs as go
import plotly.plotly as py
from plotly import tools
from datetime import date
import pandas as pd
import numpy as np 
import plotly.figure_factory as ff

#모델링을 위한 모듈
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import lightgbm as lgb

#그래프를 그리기 위한 모듈
import matplotlib.pyplot as plt
import seaborn as sns
import random 
import warnings
import operator
warnings.filterwarnings("ignore")
init_notebook_mode(connected=True)

import copy


# <hr>
# ## Part A : Exploration
# <hr>
# 
# ## 1. Dataset Preparation
# 
# 데이터 셋을 살펴보는 것으로 분석의 기초를 다진다.

# In[98]:


train = pd.read_csv("train.csv")
test = pd.read_csv("test.csv")

print ("Train Dataset: Rows, Columns: ", train.shape)
print ("Test Dataset: Rows, Columns: ", test.shape)


# In[99]:


print ("Glimpse of Train Dataset: ")
train.head()


# In[100]:


print ("Summary of Train Dataset: ")
train.describe()


# In[101]:


print ("Top Columns having missing values")
missmap = train.isnull().sum().to_frame().sort_values(0, ascending = False)
missmap.head()


# In[102]:


train = train.drop(columns=["Id","idhogar"])


# In[103]:


#총 가구원수 변수의 동일 여부 검정
print((train["tamhog"] == train["hhsize"]).value_counts())
print("=================")
print((train["tamhog"] == train["hogar_total"]).value_counts())
print("=================")
print((train["tamhog"] == train["r4t3"]).value_counts())
print("=================")
print((train["tamhog"] == train["tamviv"]).value_counts())


# In[104]:


#차이나는 총 가구원수 변수들 확인

print((train[train["tamhog"] != train["r4t3"]].loc[:,"r4t3":"tamhog"]).head(5))
print((train[train["tamhog"] != train["tamviv"]].loc[:,['tamhog','tamviv']]).head(5))


# In[105]:


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


# In[106]:


## 임대료 결측치의 case1 : 자가소유와 관계가 깊으므로 모든 결측치를 0으로 처리
train_zero = pd.DataFrame()
train_zero = copy.copy(pd.DataFrame(train))
## 얕은 카피로 메모리가 분리되었다.
print(id(train_zero),id(train))


# In[107]:


train_zero["v2a1"] = train_zero["v2a1"].fillna(0)
print(train_zero[train_zero["v2a1"].isnull()]["v2a1"].value_counts())


# In[108]:


### case 1-1 개인 특성 관련 변수만 사용
### 개인 특성 변수는 parent, estado, inst, escolari, rez_esc 6개임.

### 가구 특성 변수들을 선언한다.("v2a1은 테스트의 대상이므로 제외함")
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


# ###핸드폰 결측치 처리

# In[109]:


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


# In[110]:


#핸드폰 보유 대수의 히스토그램
trace1 = go.Histogram(name="휴대폰 보유대수", x=train["qmobilephone"])
fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)

fig['layout'].update(height=400, showlegend=False, title="핸드폰 보유 대수 히스토그램")
iplot(fig)
#원-핫-인코딩으로 만들어볼까?


# In[111]:


## 개별 테스트 결과, 원핫보단 단변량이 더 우수했으므로 이 부분의 전처리는 제외한다.


# ###타블렛 결측 처리

# In[112]:


print("v18q1 결측값:",train["v18q1"].isnull().value_counts().values[0])
### 결측값이 엄청 많네


# In[113]:


#핸드폰 보유 대수의 히스토그램
trace1 = go.Histogram(name="타블렛 보유대수", x=train["v18q1"])
fig = tools.make_subplots(rows=1, cols=1, print_grid=False)
fig.append_trace(trace1, 1, 1)

fig['layout'].update(height=400, showlegend=False, title="타블렛 보유 대수 히스토그램")
iplot(fig)
#<0대>가 하나도 없는 것을 볼 수 있다, 0대가 곧 결측일까?


# In[114]:


## 타블렛을 보유하지 않았다고 대답한 사람들은 모두 타블렛 보유 대수가 결측이다.
print("타블렛 보유X 응답자수:",train[train["v18q"] == 0]["v18q1"].isnull().value_counts().values[0])
print("v18q1 결측값:",train["v18q1"].isnull().value_counts().values[0])


# In[115]:


print("타블렛 보유자의 수:",train["v18q"].value_counts().values[0])
print("타블렛 미 보유자의 수:",train["v18q"].value_counts().values[1])
print("타블렛 보유대수 0:",train[train["v18q1"] == 0]["v18q"].value_counts().values)
print("타블렛 보유대수 1 이상:",train[train["v18q1"] != 0]["v18q"].value_counts().values[0])


# In[116]:


### 미보유를 결측처리 했음을 알았으니깐, 전부 0으로 처리해준다.


# In[117]:


train_zero["v18q1"] = train_zero["v18q1"].fillna(0)


# In[118]:


print("v18q1의 비 결측값 수:",train_zero["v18q1"].isnull().value_counts().values[0])

<hr>

## Part B: Modelling

<hr>

## 8. Baseline Model

### 8.1 train_zero 모형으로 xgboost 적합
# In[59]:


## list of features to be used
#features = [c for c in train_zero.columns if c not in ['Id', 'Target']]

## target variable 
target = train['Target'].values
target_index = {1:0, 2:1, 3:2, 4:3}
target = np.array([target_index[c] for c in target])


# In[48]:


## list of features to be used
#features = [c for c in train_class.columns if c not in ['Id', 'Target']]

## target variable 
target = train_zero['Target'].values
target_index = {1:0, 2:1, 3:2, 4:3}
target = np.array([target_index[c] for c in target])


# ### 8.2 Label Encode the categorical variables 

# ### 8.3 Prepare Training and Validation Sets

# In[60]:


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


# In[61]:


#원본 데이터셋
X_train, X_valid, y_train, y_valid = train_test_split(train[train_ind].values, target, test_size=0.2, random_state=1)
lgb_train = lgb.Dataset(X_train, y_train)
lgb_valid = lgb.Dataset(X_valid, y_valid)


# In[62]:


#원본 데이터셋
def label_encoding(col):
    le = LabelEncoder()
    le.fit(list(train_zero[col].values) + list(test[col].values))
    train_zero[col] = le.transform(train_zero[col].astype(str))
    test[col] = le.transform(test[col].astype(str))

features = train_zero.columns
num_cols = train_zero._get_numeric_data().columns
cat_cols = list(set(features) - set(num_cols))
for col in cat_cols:
    label_encoding(col)


# In[63]:


#원본 데이터셋
X_train, X_valid, y_train, y_valid = train_test_split(train_zero[train_ind].values, target, test_size=0.2, random_state=1)
lgb_train_zero = lgb.Dataset(X_train, y_train)
lgb_valid_zero = lgb.Dataset(X_valid, y_valid)


# str### 8.4 Baseline LightGBM

# In[64]:


## 원본 데이터셋
params = {'boosting_type': 'gbdt', 'objective': 'multiclass', 'metric': 'multi_logloss',
          'num_class': 4, 'max_depth': 44, 'num_leaves': 36, 'learning_rate': 0.01,
          'feature_fraction': 0.8, 'bagging_fraction': 0.8, 'bagging_freq': 5,
          'lambda_l2': 1.0, 'verbose': -1, 'num_threads': -1 }

model1 = lgb.train(params, lgb_train, num_boost_round=100,  valid_sets=[lgb_train, lgb_valid], 
                  early_stopping_rounds=2000, verbose_eval=100)


# In[65]:


## 전처리 모두 적용
params = {'boosting_type': 'gbdt', 'objective': 'multiclass', 'metric': 'multi_logloss',
          'num_class': 4, 'max_depth': 44, 'num_leaves': 36, 'learning_rate': 0.01,
          'feature_fraction': 0.8, 'bagging_fraction': 0.8, 'bagging_freq': 5,
          'lambda_l2': 1.0, 'verbose': -1, 'num_threads': -1 }

model4 = lgb.train(params, lgb_train_zero, num_boost_round=100,  valid_sets=[lgb_train_zero, lgb_valid_zero], 
                  early_stopping_rounds=2000, verbose_eval=100)


# ### 8.5 Predict and Submit

# In[ ]:


## revert back to original classes
reverse_index = {}
for k,v in target_index.items():
    reverse_index[v] = k
preds = [reverse_index[np.argmax(p)] for p in preds]

## submission
subs = pd.DataFrame()
subs['Id'] = test['Id']
subs['Target'] = preds
subs.to_csv('submission.csv', index=False)
subs.head()


# Further Improvement Ideas 
# - Feature Engineering  
# - Model Tuning  
# - Model Ensembling  
