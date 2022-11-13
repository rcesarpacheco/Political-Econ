import glob
import os
import re
# import chardet 
import time

import cv2
import numpy as np
import pandas as pd
from deepface import DeepFace
from retinaface import RetinaFace
from transformers import pipeline

wdir = "C:/Users/rcesa/My Drive/Columbia/2nd year/Fall/Political Econ/Psets/Pset 2/"
# model for sentiment analysis
specific_model = pipeline(model="cardiffnlp/twitter-roberta-base-sentiment")

# with open(os.path.join(wdir,"Data","text","FN_tweets.csv"), 'rb') as file:
#     print(chardet.detect(file.read()))


################################
#                              #
#        Image analyzer        #
#                              #
################################

file_list_nyt = glob.glob(os.path.join(wdir,"Data",'images',"nyt","*.jpg"))
file_list_fn = glob.glob(os.path.join(wdir,"Data",'images',"fn","*.jpg"))
file_list =file_list_nyt+file_list_fn

# function to get extract faces
def get_faces(file):
    faces =  RetinaFace.extract_faces(img_path = file, align = True)
    return faces
# function to predit age and gender
def gender_age_prediction(img_face):
    obj = DeepFace.analyze(img_face, actions = ['age', 'gender','race'], enforce_detection=False)
    return obj['age'], obj['gender'], obj['dominant_race']
    
# data frame to store the results
df = pd.DataFrame(columns=['source','file', 'gender', 'age','race','face_detected'])
total_images = len(file_list)
i=1
for file in file_list:
    print(file)
    print('\n')
    print(i/total_images*100)
    i=i+1
    source = file.split('\\')[-2]
    file_name = file.split('\\')[-1]
    size = os.path.getsize(file)
    if size != 0:
        faces = get_faces(file)
        if len(faces)>0:
            for face in faces:
                age,gender,race = gender_age_prediction(face)
                res = {'source':source, 'file': file_name, 'gender' : gender, 'age' : age,'race':race,'face_detected':'detected_face'}
                df = pd.concat([df,pd.DataFrame([res])],ignore_index = True)
        else:
            res = {'source':source, 'file': file_name, 'gender' : np.nan, 'age' : np.nan,'race' : np.nan,'face_detected':'no_face'}
            df = pd.concat([df,pd.DataFrame([res])],ignore_index = True)
    else:
        res = {'source':source, 'file': file_name, 'gender' : np.nan, 'age' : np.nan,'race' : np.nan,'face_detected':'problem_reading_image'}
        df = pd.concat([df,pd.DataFrame([res])],ignore_index = True)

df.to_csv(os.path.join(wdir,'Results','images_results.csv'),index=False)

################################
#                              #
#        Text Analyzer         #
#                              #
################################

data_text_nyt = pd.read_csv(os.path.join(wdir,"Data","text","NYT_tweets.csv"),encoding="Windows-1252")
data_text_fn = pd.read_csv(os.path.join(wdir,"Data","text","FN_tweets.csv"),encoding="Windows-1252")

data_text = pd.concat([data_text_nyt, data_text_fn])
def clean_text(text):
    clean_text = re.sub(r'http\S+', '', text).strip()
    return clean_text

def sentimental_analysis(text,model):
    result = specific_model(clean_text(text))
    labels = {'LABEL_0':'Negative','LABEL_1':'Neutral','LABEL_2':'Positive'}
    return labels[result[0]['label']], result[0]['score']

data_text['label_sentiment'],data_text['score']  = zip(*data_text['Text'].map(lambda x: sentimental_analysis(x,specific_model)))
data_text.to_csv(os.path.join(wdir,'Results','text_results.csv'),index=False)



