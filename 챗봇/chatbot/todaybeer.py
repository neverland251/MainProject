__all__ = ['todaybeer']

from konlpy.tag import Okt
twitter_tag = Okt()
def vect_tokenizer(text):
    return twitter_tag.nouns(text)
print("tokenizer load")

from keras import models
from keras.preprocessing import sequence
import tensorflow as tf
print("keras load")

from sklearn.feature_extraction.text import CountVectorizer
import pandas as pd
from pandas import DataFrame, Series
import numpy as np
print("pandas, numpy load")

model = models.load_model("chatbot.model")
model.trainable = False
model.compile(loss="categorical_crossentropy", optimizer="adam", metrics=["accuracy"])
global graphs
graphs = tf.get_default_graph()
print("model load")

# 단어-숫자 짝으로 이루어져 있는 색인사전을 불러온다.
vocabulary = pd.read_csv("색인사전.csv", engine="python", encoding="utf-8")
# DataFrame형태인 색인사전을 검색이 용이하게 딕셔너리로 변환해준다.
vocabulary = vocabulary.to_dict(orient="records")[0]
print("vocabulary load")

class todaybeer():
    def __init__(self):
        self.model = model

    def engine(self, corpus):
        ### 문장 판단 부분 ###
        #twitter 형태소 분석기를 통해 사용자가 입력한 문장(corpus)를 형태소 분해하여 tests 리스트에 담아준다.
        self.tests = twitter_tag.nouns(corpus)

        #tests 리스트를 for문으로 단어를 하나하나씩 가져오면서
        for i in self.tests:
            self.corpuslist = list()
            #색인사전(vocabulary)에 단어를 입력하고, 그 단어에 해당하는 숫자값을 temporailyList에 저장한다.
            try:
                self.corpuslist.append(vocabulary[i])
            #색인사전에 없는 단어는 KeyError 예외가 뜨는데, 이 예외가 뜨는 경우 숫자 대신 0를 대신 집어넣어준다.
            except KeyError:
                self.corpuslist.append(0)
        # for문 순회가 끝난 list를 corpusList에 다시 담아준다.
        #입력 문장은 사용자가 입력하는 단어의 수에 따라 천차만별로 차이가 난다. 따라서, 우리가 사전 훈련한 모델의 입력
        #차원에 맞추기 위해, 빈 공간은 임의로 0으로 채우는 pad_sequence 함수를 이용해준다.
        vectorized_seq = sequence.pad_sequences([self.corpuslist], maxlen=50)
        #컨볼루션 LSTM이 예측한 결과값을 return_classes에 담고, 이를 return으로 출력한다.
        return_classes = self.model.predict_classes(vectorized_seq)
        return return_classes

    def save_corpus(self, corpus, return_classes):
        #사용자가 입력한 입력문장을 저장하는 log파일인 corpusDb 파일을 불러온다.
        dbList = pd.read_csv("corpusDb.csv", engine="python", encoding="utf-8")
        #이 부분은 사용자가 (0 : 싫어, 1 : 좋아)를 입력하는 부분으로, 사후 평가에 해당한다.
        userReturn = input("결과")
        #사용자의 입력 문장, LSTM 모형이 예측한 결과값, 유저 사후 평가를 하나로 합치고
        raw = pd.DataFrame([{"text": corpus, "target": return_classes[0], "user_return": userReturn}])
        #기존에 불러온 corpusDb파일에다 이를 concat해서 하나로 합쳐준다.
        dbList = pd.concat([raw, dbList])
        #추가가 끝난 log파일을 다시 corpusDb로 저장해준다.
        dbList.to_csv("corpusDb.csv", encoding="utf-8", index=False)

# 이 부분은 부모 클래스를 상속받는 자식 클래스로, 이 클래스의 역할은 __init__을 통해 패키지들을
#임포트 해주는 것이다.


'''
# 불러올때는 todaybeer가 아닌 todaybeer_main()을 불러와야한다.
beer = todaybeer_main()

#사용자의 문장을 입력받는다.
corpus = "오늘 날씨 어때"
#예측값을 출력받아 return_class에 저장한다.
return_class = beer.engine(corpus)
print(return_class)
#사용자가 입력한 문장과, 출력받은 예측값을 corpusDb log에다 저장하는 save_corpus 함수를 불러온다.
beer.save_corpus(corpus,return_class)
'''