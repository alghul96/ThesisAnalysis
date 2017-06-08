import numpy as np
import pandas as pd
import pylab
from matplotlib import cm
from mixture_models import VBBMM

#data = np.array(pd.read_csv('clusterdata2.csv')) # reading the csv from R

# initialise bernoulli mixture model with 3 components

nclusters = int(nclusters)
nsimulations= int(nsimulations)


bmm = VBBMM(n_components = nclusters, n_init = nsimulations, c = 0.2, d = 0.7, compute_score = True, verbose = False)
bmm = bmm.fit(data)
modelclust = bmm.predict(data)
clustprob = bmm.predict_proba(data)
prototype = bmm.cluster_prototype()
