import numpy as np
import pandas as pd
import pylab
import matplotlib.pyplot as plt
from matplotlib import cm
from mixture_models import VBBMM

data = np.array(pd.read_csv('clusterdata2.csv')) # reading the csv from R

# initialise bernoulli mixture model with 3 components

bmm = VBBMM(n_components = 3, n_init = 1000, c = 0.2, d = 0.7, compute_score = True, verbose = True) # initializing the object

bmm = bmm.fit(data)
print bmm

modelclust = bmm.predict(data)
print modelclust

clustprob = bmm.predict_proba(data)
# print clustprob[:, 0]
# print clustprob[:, 1]
# print clustprob[:, 2]


prototype = bmm.cluster_prototype()

# print prototype[:, 0] # prototype of student of cluster 1
# print prototype[:, 1] # prototype of student of cluster 2
# print prototype[:, 2] # prototype of student of cluster 3


plt.hist(modelclust)
plt.show()
