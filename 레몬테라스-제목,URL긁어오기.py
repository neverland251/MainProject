#!/usr/bin/env python
# coding: utf-8

# In[3]:


import urllib.request
import time
import csv
import json
import ssl
from konlpy.tag import Twitter
from collections import Counter
import requests
import feedparser

import os
from selenium import webdriver
from bs4 import BeautifulSoup as bs
import pandas as pd

import re


# In[5]:


browser= webdriver.Chrome('chromedriver.exe')


# In[6]:


html = browser.get("https://cafe.naver.com/remonterrace")


# In[7]:


# 중요한 내용이 담겨있는 서브프레임을 찾는다.
#find라고 뜨는 프레임이 중요 내용이 담겨있는 프레임이다.

for i,j in enumerate(browser.find_elements_by_tag_name("iframe")):
    browser.switch_to.frame(j)
    text = bs(browser.page_source, "lxml")
    if text.find("a","article") != None:
        print("find",i)
        browser.switch_to.default_content()
    else :
        print("no",i)
        browser.switch_to.default_content()


# In[13]:


#제목을 긁어온다.

browser.switch_to.frame(browser.find_elements_by_tag_name("iframe")[8])

html = browser.page_source
text = bs(html,"lxml")
for i in text.find_all("a","article"):
    print(i.text.lstrip().rstrip())
    

browser.switch_to.default_content()


# In[14]:


#각 제목에 해당하는 게시물들의 접속 URL을 따온다.

browser.switch_to.frame(browser.find_elements_by_tag_name("iframe")[8])
links = browser.find_elements_by_css_selector('div.board-list a.article')

url = [i.get_attribute("href") for i in links]


# In[17]:


for i in url:
    browser.get(i)
    browser.switch_to.frame(browser.find_elements_by_tag_name("ifrmae")[8])
    text = bs(browser.page_source,"lxml")


# In[18]:


url

