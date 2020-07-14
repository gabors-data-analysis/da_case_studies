####################################################
# Import packages
####################################################
import numpy as np

####################################################
# Define global vars
####################################################
color = ["#3a5e8cFF", "#10a53dFF", "#541352FF", "#ffcf20FF", "#2f9aa0FF"]


####################################################
# Define helper functions
####################################################
def seq(start,stop,by,round_n=3):
    return [round(x,round_n) for x in list(np.arange(start,stop,by))]

def skew(l, round_n=3):
    return round((np.mean(l) - np.median(l)) / np.std(l), round_n)

