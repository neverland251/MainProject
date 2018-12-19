#!/usr/bin/env python
# coding: utf-8

# In[30]:


from selenium import webdriver
from selenium.webdriver import ActionChains as AC

from pandas import Series,DataFrame
from bs4 import BeautifulSoup as bs
import pandas as pd
import numpy as np
import urllib.request as request
import requests
from time import sleep
import re


# In[5]:


def search(keyword,killswitch):
    browser.switch_to.default_content()
    browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
    if killswitch != 1:
        search = browser.find_element_by_id("q0")
        search.send_keys(keyword)
        button = browser.find_element_by_id("suggest_search0")
        AC(browser).move_to_element(button).click().perform()
    else : pass


# In[6]:


def nextpage(startpage):
    browser.switch_to.default_content()
    browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
    for k in browser.find_elements_by_css_selector("a.num_box"):
        if k.text == str(startpage):
            AC(browser).move_to_element(k).click().perform()
            print("complete",startpage)
            break
        else : pass   


# In[7]:


'''
def secondloop(df = df):
    datum = list()
    datum2 = list()
    browser.switch_to.default_content()
    browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
    text = bs(browser.page_source,"lxml")
    for i in text.find_all("tr","list_row_info"):
        datum.append(i.find("a").text)
        print(datum)
    print("end1")
    for j in text.find_all("a","txt_sub"):
        datum2.append(j.text)
        print(datum2)
    print("end2")
    return df.append(pd.concat([DataFrame({"title":datum}),DataFrame({"text":datum2})],axis=1))
'''


# In[131]:


#browser 실행
browser = webdriver.Chrome("chromedriver.exe")


# In[146]:


#주소 입력
browser.get("http://cafe.daum.net/subdued20club/Lp0T")

numpage = 2
lastpage = 100
killswitch = 0
df = DataFrame()

while numpage <= lastpage:
    print("start",numpage)
    sleep(0.6)
    search("여행",killswitch)
    datum = list()
    datum2 = list()
    browser.switch_to.default_content()
    browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
    text = bs(browser.page_source,"lxml")
    for i in text.find_all("tr","list_row_info"):
        datum.append(i.find("a").text)
        #print(datum)
    #print("end1")
    for j in text.find_all("a","txt_sub"):
        datum2.append(j.text)
        #print(datum2)
    #print("end2")
    df = df.append(pd.concat([DataFrame({"title":datum}),DataFrame({"text":datum2})],axis=1))
    nextpage(numpage)
    killswitch = 1
    numpage += 1


# In[137]:


df


# In[147]:


#인덱스를 다시 리셋해서 저장
##파일명
file = "여성시대-자유게시판-여행"
##확장자
extend = ".csv"
df = df.reset_index()
del df["index"]
#del df["Unnamed: 0"]
df.to_csv(str(file)+str(extend))


# In[148]:


text = pd.read_csv(str(file)+str(extend))
#del text["index"]
del text["Unnamed: 0"]
text.to_csv(str(file)+str(extend))


# In[149]:


appendtext = DataFrame(pd.concat([text["title"],text["text"]],sort=True))
appendtext = appendtext.reset_index()
del appendtext["index"]
hangul = re.compile("[^ ㄱ-ㅣ가-힣a-zA-Z]+")
for i,j in enumerate(appendtext[0]):
    appendtext[0][i] = hangul.sub(" ngh",j)
appendtext.to_csv(str(file) + "통합.csv")


# In[145]:


#테스트용 리셋

browser.switch_to.default_content()
browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
text = bs(browser.page_source,"html.parser")


# In[580]:


'''
def search(keyword):
    search = browser.find_element_by_id("q0")
    search.send_keys(keyword)
    button = browser.find_element_by_id("suggest_search0")
    AC(browser).move_to_element(button).click().perform()

def nextpage(startpage):
    browser.switch_to.default_content()
    browser.switch_to.frame(browser.find_elements_by_tag_name("frame")[0])
    for k in browser.find_elements_by_css_selector("a.num_box"):
        if k.text == str(startpage):
            AC(browser).move_to_element(k).click().perform()
            print("complete",startpage)
            break
        else : pass   

def secondloop():
    browser.switch_to.default_content()
    frame = browser.find_elements_by_tag_name("frame")
    for i in frame:
        try:
            browser.switch_to.frame(i)
            text = bs(browser.page_source,"lxml")
            if text.find("tr","list_row_info") != None:
                print(text.find("a").text)
        except:
            browser.switch_to.default_content()
            pass

def firstloop(keyword,killswitch = killswitch,numpage = numpage):
    browser.switch_to.default_content()
    frame = browser.find_elements_by_tag_name("frame")
    for i in frame:
        try:
            browser.switch_to.frame(i)
            text = bs(browser.page_source,"lxml")
            if text.find("td","subject") != None:
                if killswitch != 1:
                    search(keyword)
                    killswitch = 1
                else: pass
                secondloop()
        except:
            browser.switch_to.default_content()
            pass

