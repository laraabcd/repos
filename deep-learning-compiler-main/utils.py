import shutil
import os
import pandas as pd
def make_testing_set():
    for i in range(35000, 45567):
        img = str(i) + ".jpg"
        direc = './driving_dataset/' + img
        #os.makedirs(direc, exist_ok=True)
        direc2 = './testing_data/'
        shutil.move(direc, direc2)

def txt_to_csv(txt, csvfile, delimeter=','):
    df = pd.read_csv(txt, sep=delimeter)
    df.to_csv(csvfile, index=False)

#make_testing_set()
txt_to_csv("./annotations.txt", "./annotations.csv")
txt_to_csv("./annotations_testing.txt", "./annotations_testing.csv")
