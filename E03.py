# -*- coding: utf-8 -*-
"""
HFAD EX03 - Time Reaction Experiment

@author: Maximilian Schrapel
"""

import threading as th  

from tkinter import *
import random
from datetime import datetime, timedelta
import winsound
import pandas as pd
import os

ts0 = datetime.now()
ts1=ts0
isRunning = False

colors={
            "Enable":True,
            "Begin":"green",
            "Event":"red",
            "Finished":"grey"
    }

sounds={
            "Enable":True,
            "Frequency":1500, # in Hz
            "Duration_ms":250 # in ms
    }

times = {
            "Time_ms":10000,    # maximum time till trigger
            "Time_ms_min":1000  # minimum time till trigger
    }
 

# start new trial
def newtrial(event):  
   global isRunning, ts0
   isRunning=True
   text["text"]="Experiment running..."
   text2["text"]=""
   S1 = th.Timer( random.randint(times["Time_ms_min"], times["Time_ms"])/1000, eventTriggered)
   S1 = th.Timer( 1, eventTriggered)
   canvas["bg"]=colors["Begin"]
   ts0 = datetime.now()
   S1.start()
   window.bind('<Return>', show_msg)
    
# trigger red event
def eventTriggered():
    global isRunning, ts1
    if isRunning: # catch early inputs
        if colors["Enable"]:
            text["text"]="!WARNING!"
            canvas["bg"]=colors["Event"]
            ts1 = datetime.now() # timestamp
        if sounds["Enable"]:
            winsound.Beep(sounds["Frequency"], sounds["Duration_ms"])
            
# show final message
def show_msg(event):
    global isRunning, ts0, ts1
    isRunning=False
    earlyinteraction=False
    delta = datetime.now() - ts1
    delta = round(delta.total_seconds()*1000,2)
    if ts0 >= ts1: # too early!
        text["text"]="Error: You reacted too early!"
        earlyinteraction=True
    else:
        text["text"]="Your time: "+str(delta) +"ms "

    text2["text"]="Press again to restart!"
    canvas["bg"]=colors["Finished"]
    
    data={
        "Start":ts0.strftime('%Y-%m-%d %H:%M:%S'),
        "Stop":ts1.strftime('%Y-%m-%d %H:%M:%S'),
        "Reaction":delta,
        "Fail":earlyinteraction,
        "MinimumTime":times["Time_ms_min"],
        "MaximumTime":times["Time_ms"],
        "WarncolorEnable":colors["Enable"],
        "Warncolor":colors["Event"],
        "NormalOperationColor":colors["Begin"],
        "WarnsoundEnable":sounds["Enable"],
        "WarnsoundFrequency":sounds["Frequency"],
        "WarnsoundDuration":sounds["Duration_ms"]
        }
    userDataHandler(data)
    window.bind('<Return>', newtrial)
    
def userDataHandler(data):
    # first prepare table
    # data cols in table
    cols = ["UserID","Start","Stop","Reaction","Fail","MinimumTime","MaximumTime","WarncolorEnable","Warncolor","NormalOperationColor","WarnsoundEnable","WarnsoundFrequency","WarnsoundDuration"]
    df = pd.DataFrame(columns=cols)
    userID = datetime.now().timestamp() # pseudonym
    if os.path.exists("ReactionTimeExperiment.csv"): 
        df = pd.read_csv('ReactionTimeExperiment.csv',sep=";",index_col=0)
        if(len(df)>0):
            userID = df.loc[0]["UserID"]
    entry = [userID]
    for col in cols:
        if not col == "UserID":
            entry.extend([data[col]])
    df=pd.concat([df, pd.DataFrame([entry],columns=cols)], ignore_index=True)
    df.to_csv("ReactionTimeExperiment.csv", sep=";")
    
    
window=Tk()
window.title('Time Reaction Experiment') # name
window.geometry("1024x768+10+20") # size of the window

text=Label(window, text="Start by hitting enter key!" ,font="TkMenuFont 24")
text.pack(pady=30)
text2=Label(window, text="" ,font="TkMenuFont 24")
text2.pack(pady=30)

canvas= Canvas(window, bg=colors["Finished"])
canvas.pack()

window.bind('<Return>', newtrial) # on return newtrial() is executed




window.mainloop() # endless loop