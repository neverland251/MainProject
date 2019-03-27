#!/usr/bin/env python
# coding: utf-8

# In[47]:


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

np.set_printoptions(suppress=True)


# In[48]:


#탑재용 모듈

GRUs_weight = load_model("chatbot-attention_weight.h5")


# In[51]:


## GRU 입력 전 사전처리 모듈(임베딩 -> 컨볼루션 -> 맥스풀링)

inputs_s = layers.Input(shape=[50],name = "Feed_Sentence")
inputs_z = layers.Input(shape = [1],name = "Feed_Zero")
inputs_u = layers.Input(shape = [128],name = "Feed_User")
#inputs_t = layers.Input(shape = [1],name = "Temporaily_Input")

embed = layers.Embedding(51073,128)

embed_i = embed(inputs_s)
embed_z = embed(inputs_z)
#embed_t = embed(inputs_t)
inputs_r = layers.Reshape((1,-1))(inputs_u)
embed_o = layers.Concatenate(axis=1,name="embed_o_Doubled")([embed_z,inputs_r])

model = layers.Dropout(0.2)(embed_i)
model = layers.Conv1D(256,3,padding="valid",activation="relu",strides=1)(model)
model = layers.MaxPooling1D(pool_size = 4)(model)

#Bi-GRU 인코더 - 디코더 네트워크
Encoder1 = layers.GRU(128,return_sequences = True,return_state = True,name="Encoder1")
Encoder2 = layers.GRU(128,return_sequences = True,return_state = True,go_backwards = True,name="Encoder2")
attention_matrix1,initial_1 = Encoder1(model)
attention_matrix2,initial_2 = Encoder2(model)
attention_matrix = layers.Concatenate(axis=-1,name = "attention_matrix")([attention_matrix1,attention_matrix2])


Decoder1 = layers.GRU(128,return_sequences = True,name="Decoder1")
Decoder2 = layers.GRU(128,return_sequences = True,name="Decoder2")

Decoder1_output = Decoder1(embed_o,initial_state = initial_1)
Decoder2_output = Decoder2(embed_o,initial_state = initial_1)

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

predicts = layers.Dense(22,activation="softmax")(finally_output)

GRUs = Model(inputs = [inputs_s,inputs_z,inputs_u], outputs = [predicts])

GRUs.layers[2].set_weights(GRUs_weight.layers[2].get_weights())
GRUs.layers[5].set_weights(GRUs_weight.layers[4].get_weights())
GRUs.layers[9].set_weights(GRUs_weight.layers[6].get_weights())
GRUs.layers[10].set_weights(GRUs_weight.layers[8].get_weights())
GRUs.layers[11].set_weights(GRUs_weight.layers[9].get_weights())
GRUs.layers[12].set_weights(GRUs_weight.layers[7].get_weights())
GRUs.layers[20].set_weights(GRUs_weight.layers[17].get_weights())
GRUs.layers[21].set_weights(GRUs_weight.layers[18].get_weights())

GRUs = Model(inputs = [inputs_s,inputs_z,inputs_u], outputs = [predicts])

GRUs.compile(loss="categorical_crossentropy",optimizer="adam",metrics=["accuracy"])


# In[52]:


print(GRUs.summary())


# In[42]:


GRUs.compile(loss="categorical_crossentropy",optimizer="adam",metrics=["accuracy"])


# In[53]:


GRUs.save("chatbot-attention_v2.h5")


# In[54]:



