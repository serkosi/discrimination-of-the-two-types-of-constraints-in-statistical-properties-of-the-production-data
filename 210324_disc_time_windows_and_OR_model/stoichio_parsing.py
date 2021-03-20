# -*- coding: utf-8 -*-
"""
Created on Sat Apr  4 17:57:28 2020

@author: serha
"""
import numpy as np
import cobra.test
import os
from os.path import join
import cobra.test
import os
from os.path import join
import pandas as pd
#import matplotlib.pyplot as plt


#import the metabolic network
data_dir = cobra.test.data_dir
mymodel = cobra.io.load_json_model(join(data_dir, "C:/Users/serha/OneDrive/Masaüstü/MyRepo/master_thesis_MMT003/210324_disc_time_windows_and_OR_model/iAT_PLT_636.json"))


# parsing the stoichiometric matrix from network and having transpose of it
stoic=cobra.util.array.create_stoichiometric_matrix(mymodel, array_type="DataFrame")
stoic_transpose = stoic.transpose()
stoic.to_csv("C:/Users/serha/OneDrive/Masaüstü/MyRepo/master_thesis_MMT003/210324_disc_time_windows_and_OR_model/iAT_PLT_636_stoichiomat.csv",index=True)

# obtaining normalized adjacency matrices for reaction-centric and metabolite-centric seperately
Adjacency_R = stoic_transpose.dot(stoic)
Adjacency_R_normalized = Adjacency_R.where(Adjacency_R >= 0, 1)
Adjacency_R_normalized = Adjacency_R.where(Adjacency_R_normalized <= 0, 1)


Adjacency_M = stoic.dot(stoic_transpose)
Adjacency_M_normalized = Adjacency_M.where(Adjacency_M >= 0, 1)
Adjacency_M_normalized = Adjacency_M.where(Adjacency_M_normalized <= 0, 1)


# adding degree (k) numbers and counts of them (Nk) as seperated columns at the end of the matrix
Adjacency_M_normalized.loc[:,'k values'] = Adjacency_M_normalized.sum(axis=1)
Adjacency_M_normalized['N_k values'] = Adjacency_M_normalized.groupby(['k values'])['k values'].transform('count')
Adjacency_R_normalized.loc[:,'k values'] = Adjacency_R_normalized.sum(axis=1)
Adjacency_R_normalized['N_k values'] = Adjacency_R_normalized.groupby(['k values'])['k values'].transform('count')


# adding probability (p_k) numbers for each node as a seperate column at the end of the matrix
Adjacency_M_normalized['P_k values'] = Adjacency_M_normalized['N_k values'].div(len(Adjacency_M_normalized))
Adjacency_R_normalized['P_k values'] = Adjacency_R_normalized['N_k values'].div(len(Adjacency_R_normalized))


# sorting values acc. to ascending k values and erase duplicated ones
Adjacency_M_normalized = Adjacency_M_normalized.sort_values(by=['k values'])
data_M = Adjacency_M_normalized[['k values', 'P_k values']].drop_duplicates()
Adjacency_R_normalized = Adjacency_R_normalized.sort_values(by=['k values'])
data_R = Adjacency_R_normalized[['k values', 'P_k values']].drop_duplicates()


# creating cdf and reverse cdf values for each degree (k) value
data_M['cdf'] = data_M['P_k values'].cumsum()
data_M['reverse cdf'] = 1 - data_M['cdf']
data_R['cdf'] = data_R['P_k values'].cumsum()
data_R['reverse cdf'] = 1 - data_R['cdf']


# plotting reverse CDF vs. P_k values
ax = data_M.plot(x ='k values', y ='reverse cdf', legend=False, logx=True, figsize=(7,7),fontsize=15)
ax.set_title('Metabolite Centric Connectivity Distributions of Helicobacter pylori 26695', fontsize=15)
ax.set_xlabel("k (degree)", fontsize=15)
ax.set_ylabel("P(k)", fontsize =15)


ax = data_R.plot(x ='k values', y ='reverse cdf', legend=False, loglog=True, figsize=(7,7),fontsize=15)
ax.set_title('Reaction Centric Connectivity Distributions of Helicobacter pylori 26695', fontsize=15)
ax.set_xlabel("k (degree)", fontsize=15)
ax.set_ylabel("P(k)", fontsize =15)













