#!/usr/bin/env python
# coding: utf-8

# In[2]:


from plotly.offline import init_notebook_mode, iplot
import plotly.graph_objs as go
import plotly.plotly as py
from plotly import tools
from datetime import date
import pandas as pd
import numpy as np 
import plotly.figure_factory as ff


# In[3]:


#모델링을 위한 모듈
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import lightgbm as lgb


# In[4]:


#그래프를 그리기 위한 모듈
import matplotlib.pyplot as plt
import seaborn as sns
import random 
import warnings
import operator
warnings.filterwarnings("ignore")
init_notebook_mode(connected=True)


# In[11]:


rawdata = pd.read_csv("rawdata.csv", header = 1)


# In[12]:


rawdata

