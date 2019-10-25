#!/usr/bin/env python
# coding: utf-8

# In[1]:


from keras import models
from keras.models import Sequential
from keras.models import Model
from keras.models import load_model
from keras import backend as K

from keras import layers
from keras.layers import Layer
from keras.layers import Input,Dense,Flatten,Embedding,Permute,Dot,Reshape
from keras.layers.convolutional import Conv1D,MaxPooling1D,MaxPooling2D
from keras.layers import Dropout
from keras.layers import LSTM,GRU

from keras.preprocessing import sequence
from keras.utils import np_utils

import tensorflow as tf

from sklearn.model_selection import train_test_split
from sklearn.feature_extraction.text import CountVectorizer

import pandas as pd
from pandas import DataFrame, Series
import numpy as np

import re

import matplotlib as mpl
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

import copy


# In[2]:


#토큰화 파일을 불러온다.

rawdata = pd.read_csv("tokenize.csv",engine="python",encoding = "cp949")

morphs = list()

for i in range(0,len(rawdata)):
    morphs.append(list(rawdata.loc[i,:].dropna()))

del morphs[0]


# In[3]:


#색인 사전을 불러온다.

morphsVectored = list()


vocabulary = pd.read_csv("dictionary.csv",engine="python",encoding="cp949")
#del vocabulary["Unnamed: 0"]

vocabulary = vocabulary.to_dict(orient="records")[0]


# In[4]:


#원본 데이터셋을 불러온다.

a = pd.read_csv("beer.csv",engine="python",encoding="cp949")
a = a[a["reviews"].duplicated() == False]
a = a.reset_index()

del a["index"]
del a["target"]

a.head()


# In[151]:


## 데이터셋에서 source가 "r"인것, 즉 리뷰사이트인 ratebeer에서 가져온것이면 "정보성(informational_index)"로, 그 외에는 
##"감정성"(emotional_index)로 각각 타겟변수로 재 설정한다.
## 향후 재 활용때도, 정보성 데이터셋은 "r"로 설정하면 학습이 가능하다.

informational_index = a[a["source"] == "r"].index
emotional_index = a[a["source"] != "r"].index

information_intent = Series([0 for i in range(0,len(informational_index))],index = informational_index)
emotional_intent = Series([1 for i in range(0,len(emotional_index)],index = emotional_index)


# In[152]:


target = pd.concat([information_intent,emotional_intent],axis=0).sort_index()


# In[80]:


#로드한 토큰화 파일에서, 색인사전을 검색하여 토큰화 문장을 숫자 문장으로 바꿔준다.

for i in morphs:
    temporailyList = list()
    for k in i:
        #print(k)
        try:
            temporailyList.append(vocabulary[k])
        except KeyError:
            temporailyList.append(0)
    morphsVectored.append(temporailyList)


# ## EDA

# In[85]:


vectorized_seq = sequence.pad_sequences(morphsVectored,maxlen = 50)


# In[87]:


X_train,X_test,y_train,y_test = train_test_split(vectorized_seq, target)


# In[98]:


y_train = np_utils.to_categorical(y_train)
y_test = np_utils.to_categorical(y_test)


# In[99]:


y_train


# # 모델 적합

# In[100]:


model = Sequential()
model.add(Embedding(len(vocabulary)+1,128,input_length = 50))
model.add(Dropout(0.2))
model.add(Conv1D(256,3,padding="valid",activation="relu",strides=1))
model.add(MaxPooling1D(pool_size = 4))
model.add(LSTM(128))
model.add(Dense(2,activation="softmax"))


# In[101]:


model.summary()
SVG(model_to_dot(model,show_shapes=True).create(prog="dot",format="svg"))


# In[102]:


from keras.callbacks import EarlyStopping

early_stopping = EarlyStopping(patience = 3)


# In[103]:


model.compile(loss="categorical_crossentropy",optimizer="adam",metrics=["accuracy"])

model.fit(X_train,y_train,epochs=5,batch_size = 64,validation_data = (X_test,y_test),callbacks = [early_stopping])


# In[104]:


model.save("chatbot-intention.h5")

