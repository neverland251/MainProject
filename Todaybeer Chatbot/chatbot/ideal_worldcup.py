__all__ = ['ideal_worldcup']

from keras import layers
from keras.models import load_model
import random

import pandas as pd
from pandas import DataFrame, Series
import numpy as np


import copy

import re

np.set_printoptions(suppress=True)

model = load_model("c:/testbot/chatbot/chatbot-attention_weight.h5")
b = model.layers[2].get_weights()
print("weight load complete")

rawdata = pd.read_csv("c:/testbot/chatbot/beer.csv", engine="python", encoding="cp949")
target = rawdata['target']
text = rawdata["reviews"]
print('rawdata load complete')

# 색인 사전을 불러온다.

morphsVectored = list()

vocabulary = pd.read_csv("c:/testbot/chatbot/dictionary.csv", engine="python", encoding="cp949")

vocabulary = vocabulary.to_dict(orient="records")[0]
print("vocabulary load complete")

merged = DataFrame(index=list(range(0, len(b[0][0]))))
for i in target.unique():
    dict_numb = vocabulary[i]
    good = b[0][dict_numb]
    good = DataFrame(good, columns=[i])
    merged = pd.merge(merged, good, left_index=True, right_index=True, how="inner")
print("getting embedding vector complete")


class ideal_worldcup():
    def __init__(self):
        self.vocabulary = vocabulary
        self.model = model

    def cosine_sim_most(self, merged_lists, return_value):
        selected = DataFrame(index=[merged], columns=["value"])
        saved = 0

        for i in merged_lists:
            vector_a = np.array(merged[i])
            norm_vector_a = np.linalg.norm(vector_a)
            vector_b = return_value
            norm_vector_b = np.linalg.norm(vector_b)
            cossine_similarity = np.dot(vector_a, vector_b) / np.dot(norm_vector_a, norm_vector_b)
            if cossine_similarity >= saved:
                saved = cossine_similarity
                return_vector = vector_a
                names = i
            else:
                pass
        return names, return_vector

    def cosine_sim_least(self, merged_lists, return_value):
        selected = DataFrame(index=[merged], columns=["value"])
        saved = 1

        for i in merged_lists:
            vector_a = np.array(merged[i])
            norm_vector_a = np.linalg.norm(vector_a)
            vector_b = return_value
            norm_vector_b = np.linalg.norm(vector_b)
            cossine_similarity = np.dot(vector_a, vector_b) / np.dot(norm_vector_a, norm_vector_b)
            if cossine_similarity <= saved:
                saved = cossine_similarity
                return_vector = vector_a
                names = i
            else:
                pass
        return names, return_vector

    def first_try(self, merged_lists):
        select = random.choice(merged.columns)
        selected_vector = merged[select]
        versus, versus_vector = self.cosine_sim_least(merged_lists, selected_vector)

        print(select, versus)
        answer = input("응답?")
        if answer == "1":
            return_value = selected_vector
            unselected_name = versus
        if answer == "2":
            return_value = versus_vector
            unselected_name = select
        return unselected_name, return_value

    def iteration_try(self, merged_list, return_value, debug=False):
        select, selected_vector = self.cosine_sim_most(merged_list, return_value)
        versus, versus_vector = self.cosine_sim_least(merged_list, return_value)
        print(select, versus)
        answer = input("응답?")
        if answer == "1":
            return_value = ((return_value - selected_vector) / 2 + selected_vector)
            unselected_name = versus
        if answer == "2":
            return_value = ((return_value - versus_vector) / 2 + versus_vector)
            unselected_name = select
        return unselected_name, return_value

    # default iteration = 10
    def ideal_choice(self, iteration=2, debug=False):
        merged_list = np.array(merged.columns)
        tries = 0
        selected_name, return_value = self.first_try(merged_list)
        merged_list = np.delete(merged_list, np.where(merged_list == selected_name)[0][0])
        if debug:
            print(merged_list, len(merged_list))
            print(return_value)
        while tries <= iteration - 1:
            selected_name, return_value = self.iteration_try(merged_list, return_value, debug)
            merged_list = np.delete(merged_list, np.where(merged_list == selected_name)[0][0])
            if debug:
                print(merged_list, len(merged_list))
            tries += 1
        return return_value
