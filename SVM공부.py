#!/usr/bin/env python
# coding: utf-8

# In[558]:


import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import seaborn as sns
import konlpy
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS
from sklearn.metrics import confusion_matrix
from sklearn.metrics import classification_report
from sklearn.metrics import accuracy_score
from sklearn.model_selection import cross_val_score
from sklearn.linear_model import LogisticRegression
from sklearn.linear_model import LinearRegression
from sklearn.svm import SVC
from sklearn.model_selection import GridSearchCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
import warnings


# In[744]:


text_train = pd.read_table("drugLibTrain_raw.tsv")
text_test = pd.read_table("drugLibTest_raw.tsv")


# In[391]:


text_train.head()


# In[ ]:


# drugname : 약물 이름, rating : 평가, effectiveness : 효과(5점척도), sideeffect : 부작용(5점척도) #condition : 복약조건`


# In[737]:


#카테고리 종류 파악
## effectiveness 변수의 데이터

print(text_train["effectiveness"].unique())
print(text_train["effectiveness"].unique().shape)


# In[738]:


sns.countplot(text_train["effectiveness"])


# In[394]:


#ratings 변수의 데이터
print(np.unique(text_train["rating"],return_inverse = True))
print(np.unique(text_train["rating"],return_inverse = True)[0].shape)


# In[395]:


sns.countplot(text_train["rating"])


# In[396]:


## sideEffects 변수의 데이터
print(text_train["effectiveness"].unique())
print(text_train["effectiveness"].unique().shape)


# In[397]:


sns.countplot(text_train["effectiveness"])


# In[398]:


## condition 변수의 데이터
print(text_train["condition"].unique())
print(text_train["condition"].unique().shape)


# In[367]:


sns.countplot(text_train["condition"])


# In[745]:


## 텍스트의 상태 확인(분리 저장)
text_train_new = pd.DataFrame()
for i in text_train.columns:
    if i.find("Review") != -1:
        text_train_new[i] = text_train[i]
    else : pass

text_train_new


# In[746]:


text_train_new["rating"] = text_train["rating"]


# In[747]:


## 부작용 카테고리화
for c in text_train["sideEffects"]:
    a = text_train["sideEffects"].unique()
    #keys= range(a.shape[0])
    #initiating a dictionary
    diction={}
    for idx,val in enumerate(a):
    #looping through to create the dictionary with mappings
        diction[idx] = a[idx]
    #the above step maps integers to the values in the columnb
    # hence inverting the key-value pairs
    diction = {v: k for k, v in diction.items()}
    #print(diction)
    # creating a dictionary for mapping the values to integers
    text_train_new["sideEffects"] = [diction[item] for item in text_train["sideEffects"]] 
    # converting data type to 'category'
    text_train_new["sideEffects"] = text_train_new["sideEffects"].astype('category')


# In[402]:


sns.countplot(text_train_new["sideEffects"])


# In[748]:


## 효과 카테고리화
for c in text_train["effectiveness"]:
    a = text_train["effectiveness"].unique()
    #keys= range(a.shape[0])
    #initiating a dictionary
    diction={}
    for idx,val in enumerate(a):
    #looping through to create the dictionary with mappings
        diction[idx] = a[idx]
    #the above step maps integers to the values in the columnb
    # hence inverting the key-value pairs
    diction = {v: k for k, v in diction.items()}
    #print(diction)
    # creating a dictionary for mapping the values to integers
    text_train_new["effectiveness"] = [diction[item] for item in text_train["effectiveness"]] 
    # converting data type to 'category'
    text_train_new["effectiveness"] = text_train_new["effectiveness"].astype('category')


# In[404]:


sns.countplot(text_train_new["effectiveness"])


# In[749]:


## ratings를 5점 척도화
text_train_new["rating_5s"] = pd.DataFrame
score5 = pd.Series(text_train["rating"])

for i,j in enumerate(score5):
    if 0 < j and j <= 2:
        text_train_new["rating_5s"][i] = 1
    elif j <= 4:
        text_train_new["rating_5s"][i] = 2
    elif j <= 6:
        text_train_new["rating_5s"][i] = 3
    elif j <= 8:
        text_train_new["rating_5s"][i] = 4
    else :
        text_train_new["rating_5s"][i] = 5
        
        


# In[750]:


sns.countplot(text_train_new["rating"])


# In[664]:


text_train_new["commentsReview"]


# In[751]:


#결측값 여부 확인
text_train_new.isna().sum()


# In[752]:


#결측값 제거
text_train_new = text_train_new.dropna()


# In[753]:


text_train_new.isna().sum()


# In[754]:


#타깃값이 되는 카테고리 변수들과 텍스트 변수들을 분할
text_train_target = text_train_new.iloc[:,3:6] #y 타겟변수들
text_train_new_text = text_train_new.iloc[:,:3] #x 문자열 변수들
text_train_new_5s = text_train_new["rating_5s"] #y ratings 5점 척도


# In[755]:


text_train_new_5s


# In[670]:


## merge한 텍스트 모음(사용하진 않음)

text_train_new_merge = pd.Series()

for i in text_train_new_text.columns:
    text_train_new_merge = text_train_new_merge.append(pd.Series(text_train_new_text[i]))


# In[671]:


##merge한 텍스트모음 벡터화(사용X)
vect5 = CountVectorizer(min_df = 5, stop_words = "english")
text_train_merge = vect5.fit(text_train_new_merge).transform(text_train_new_merge)


# In[797]:


#CountVectorizer
count = 0

with open("text.txt","w") as f:
    with open("text2.txt","w") as ff:
        with open("names.txt","w") as n:
            for i in text_train_new_text.columns:
                count += 1
                f.writelines("cvect"+str(count)+"= CountVectorizer(min_df = 5, stop_words=""'"'english'"'"")\n")
                ff.writelines("text_train_new_text_"+str(i)+"= cvect"+str(count)+".fit(text_train_new_text["'"'+str(i)+'"'"]).transform(text_train_new_text["'"'+str(i)+'"'"])\n")
                n.writelines("text_train_new_text_"+str(i)+"\n")
                


# In[798]:


#tidifVectorizer
count = 0

with open("text.txt","w") as f:
    with open("text2.txt","w") as ff:
        with open("names.txt","w") as n:
            for i in text_train_new_text.columns:
                count += 1
                f.writelines("tvect"+str(count)+"= TfidfVectorizer(min_df = 0, stop_words=""'"'english'"'"")\n")
                ff.writelines("text_train_new_text_"+str(i)+"= tvect"+str(count)+".fit(text_train_new_text["'"'+str(i)+'"'"]).transform(text_train_new_text["'"'+str(i)+'"'"])\n")
                n.writelines("text_train_new_text_"+str(i)+"\n")


# In[799]:


with open("text.txt","r") as f:
    with open("text2.txt","r") as ff:
        with open("names.txt","r") as n:
            text1 = f.readlines()
            text2 = ff.readlines()
            names = n.readlines()
            for i,j,k in zip(text1,text2,names):
                i.replace("\n","")
                j.replace("\n","")
                k.replace("\n","")
                exec(i)
                exec(j)


# In[800]:


def namesopen():
    with open("names.txt","r") as n:
        names = n.read().splitlines()
        return names
    
names = namesopen()


# In[759]:


print(names)
eval(names[0])


# In[51]:


text_train_merge


# In[801]:


#각각의 리뷰를 분할하여 할당
x_train_benefits = text_train_new_text_benefitsReview
x_train_side = text_train_new_text_sideEffectsReview
x_train_comment = text_train_new_text_commentsReview
#타깃값(rating)을 y_train에 저장
text_train = text_train.dropna()
y_train = text_train_target["rating"]
#y_train의 데이터 타입을 "category"로 설정
y_train.astype("category")
#훈련, 테스트 세트 분할
X_train_benefits, X_test_benefits, y_train_benefits, y_test_benefits = train_test_split(x_train_benefits, y_train)
X_train_side, X_test_side, y_train_side, y_test_side = train_test_split(x_train_side, y_train)
X_train_comment, X_test_comment, y_train_comment, y_test_comment = train_test_split(x_train_comment, y_train)


# In[802]:


#방금 변환한 5점척도 타깃값은 5score 변수로 따로 저장

text_train_new_5s = text_train_new_5s.astype("int")

# 훈련-테스트 분할
X_train_benefits_5s, X_test_benefits_5s, y_train_benefits_5s, y_test_benefits_5s = train_test_split(x_train_benefits, text_train_new_5s)
X_train_side_5s, X_test_side_5s, y_train_side_5s, y_test_side_5s = train_test_split(x_train_side, text_train_new_5s)
X_train_comment_5s, X_test_comment_5s, y_train_comment_5s, y_test_comment_5s = train_test_split(x_train_comment, text_train_new_5s)


# In[54]:


# 모델링


# In[762]:


## 로지스틱 회귀(10점 척도, X_train_benefits, BOW
logreg = LogisticRegression()
param_grid = {"C" : [0.001, 0.01, 0.1, 1, 10]}

grid = GridSearchCV(logreg, param_grid, cv=5)
grid.fit(X_train_benefits, y_train_benefits)

print(grid.best_score_)
print(grid.best_params_)

print("테스트 세트 점수 : {}".format(grid.score(X_test_benefits, y_test_benefits)))


# In[790]:


## 로지스틱 회귀(5점 척도),X_train_benefits, BOW
logreg = LogisticRegression()
param_grid = {"C" : [0.001, 0.01, 0.1, 1, 10]}
grid_5score = GridSearchCV(logreg, param_grid, cv=5)
grid_5score.fit(X_train_benefits_5s, y_train_benefits_5s)

print(grid_5score.best_score_)
print(grid_5score.best_params_)

print("테스트 세트 점수: {}".format(grid_5score.score(X_test_benefits_5s,y_test_benefits_5s)))


# In[803]:


## 로지스틱 회귀(5점 척도), X_train_benefits, Tdidf
logreg = LogisticRegression()
param_grid = {"C" : [0.001,0.01,0.1,1,10]}
grid_tdidf = GridSearchCV(logreg, param_grid, cv=5)
grid_tdidf.fit(X_train_benefits_5s, y_train_benefits_5s)

print(grid_tdidf.best_score_)
print(grid_tdidf.best_params_)

print("테스트 세트 점수 : {}".format(grid_tdidf.score(X_test_benefits_5s, y_test_benefits_5s)))


# In[585]:


svm = SVC(kernel = "rbf")
param_grid = [{'C': [0.001, 0.01, 0.1, 1, 10]},{"gamma" : [0.001,0.01,0.1,1,10]}]
scores = GridSearchCV(svm, param_grid, cv=5)
scores.fit(X_train_benefits,y_train_benefits_5s)

print(scores.best_params_)
print(scores.best_score_)


# In[788]:


lm = LinearRegression

models = lm().fit(X_train_benefits_5s, y_train_benefits_5s)
print(models.score(X_test_benefits_5s, y_test_benefits_5s))

