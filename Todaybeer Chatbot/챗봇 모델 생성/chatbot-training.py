#!/usr/bin/env python
# coding: utf-8

# In[ ]:


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


#토큰화 파일을 불러온다.

rawdata = pd.read_csv("tokenize.csv",engine="python",encoding = "cp949")

morphs = list()

for i in range(0,len(rawdata)):
    morphs.append(list(rawdata.loc[i,:].dropna()))
del morphs[0]

#색인 사전을 불러온다.

morphsVectored = list()

vocabulary = pd.read_csv("dictionary.csv",engine="python",encoding="cp949")
#del vocabulary["Unnamed: 0"]

vocabulary = vocabulary.to_dict(orient="records")[0]

#원본 데이터셋을 불러온다.

a = pd.read_csv("beer.csv",engine="python",encoding="cp949")
a = a.reset_index()
del a["index"]

#타겟과 텍스트를 따로 저장한다.
target = a["target"]
text= a["reviews"]

preprocessing_target_from_target = target.unique()
for j,i in enumerate(morphs):
    for k in preprocessing_target_from_target:
        for l in np.where(np.array(i) == k)[0]:
            try:
                del morphs[j][l]
            except IndexError : 
                pass

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


vectorized_seq = sequence.pad_sequences(morphsVectored,maxlen = 50)

print(len(target))

X_train,X_test,y_train,y_test = train_test_split(vectorized_seq, target)

zero_pad_train = np.array([[0,vocabulary[i]] for i in y_train])
zero_pad_test = np.array([[0,vocabulary[i]] for i in y_test])

from sklearn.preprocessing import LabelEncoder

le = LabelEncoder()
y_train = le.fit_transform(y_train)
y_test = le.fit_transform(y_test)

a = le.inverse_transform(y_train)

pd.unique(y_train)

pd.unique(a)

target_dict = {i:j for i,j in zip(pd.unique(y_train),pd.unique(a))}

Series(target_dict).sort_index().to_csv("targets.csv",encoding="cp949")

y_train = np.array([[i,i]for i in y_train])
y_test = np.array([[i,i]for i in y_test])

y_train = np_utils.to_categorical(y_train)
y_test = np_utils.to_categorical(y_test)

## GRU 입력 전 사전처리 모듈(임베딩 -> 컨볼루션 -> 맥스풀링)

inputs = layers.Input(shape=[50],name = "Feed_Sentence")

embed = layers.Embedding(len(vocabulary)+1,128)

embed_i = embed(inputs)
model = layers.Dropout(0.2)(embed_i)
model = layers.Conv1D(256,3,padding="valid",activation="relu",strides=1)(model)
model = layers.MaxPooling1D(pool_size = 4)(model)

inputs_d = layers.Input(shape = [2],name = "Feed_ZeroPad_for_Decoder")
embed_o = embed(inputs_d)
#embed_o = layers.Concatenate(axis=-1,name="embed_o_Doubled")([embed_o,embed_o])


#Bi-GRU 인코더 - 디코더 네트워크
Encoder1 = layers.GRU(128,return_sequences = True,return_state = True,name="Encoder1")
Encoder2 = layers.GRU(128,return_sequences = True,return_state = True,go_backwards = True,name="Encoder2")
attention_matrix1,initial_1 = Encoder1(model)
attention_matrix2,initial_2 = Encoder2(model)
attention_matrix = layers.Concatenate(axis=-1,name = "attention_matrix")([attention_matrix1,attention_matrix2])

Decoder1 = layers.GRU(128,return_sequences = True,name="Decoder1")
Decoder2 = layers.GRU(128,return_sequences = True,name="Decoder2")
Decoder1_output = Decoder1(embed_o,initial_state = initial_1)
Decoder2_output = Decoder2(embed_o,initial_state = initial_2)
Decoder_output = layers.Concatenate(axis=-1,name="Decoder_output")([Decoder1_output,Decoder2_output])
##어텐션 메커니즘 부분

#normalize = True로 켠 상태에서, 코싸인 유사도를 구할 수 있도록 둘을 내적한다.  
Cosine_similarity = layers.dot([Decoder_output,attention_matrix],axes = -1,normalize=True,name="Cosine_similarity")

#유사도 벡터를 softmax층에 통과시켜 총합이 1인 확률로 변환한다. 이를 attention_score로 명명한다.
attention_score_layer = layers.Softmax(axis=-1,name="attention_score_from_Softmax") 
attention_score = attention_score_layer(Cosine_similarity)

#Softmax 변환된 attention_score를 최초의 attention_matrix와 각각 내적한다.
#Transpose_attention_matrix = layers.Permute((2,1),name = "Transpose_attention_matrix")(attention_matrix)
weighted_attention_matrix = layers.Lambda(lambda x: K.dot(x[0],x[1]),name="weighted_attention_matrix")([attention_score,attention_matrix])
#weighted_attention_matrix = layers.multiply([attention_score,Transpose_attention_matrix],name="weighted_attention_matrix")

#확률과 내적한 가중 attention_matrix의 열벡터를 모두 더해 1D 텐서인 context vector를 만들어준다.(1 * 256)
context_vector = layers.Lambda(lambda x: K.sum(x, axis=2),name="Making_context_vector")(weighted_attention_matrix)
#context_vector_reshape = layers.Reshape((1,-1),name="Reshape_to_3D_tensor")(context_vector)

concat = layers.Concatenate(axis=-1,name = "Concatenate_Decoder_O_and_Context_Vector")([Decoder_output,context_vector])

Feed_forward = layers.Dense(512,activation = "tanh",name="Feed_forward")
finally_output = Feed_forward(concat)

predicts = layers.Dense(21,activation="softmax")(finally_output)

GRUs = Model(inputs = [inputs,inputs_d], outputs = [predicts])
GRUs.summary()

from keras.callbacks import EarlyStopping

early_stopping = EarlyStopping(patience = 3)

GRUs.compile(loss="categorical_crossentropy",optimizer="adam",metrics=["accuracy"])

GRUs.fit([X_train,zero_pad_train],y_train,epochs=1,batch_size = 64,validation_data = ([X_test,zero_pad_test],y_test),callbacks = [early_stopping])

GRUs.save("chatbot-attention_weight.h5")

