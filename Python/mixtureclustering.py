import numpy as np
import pandas as pd
import pylab
import matplotlib.pyplot as plt
from matplotlib import cm
from mixture_models import VBBMM

data = np.array(pd.read_csv('clusterdata.csv'))

# initialise bernoulli mixture model with 3 components
n_comoponents = 3;
bmm = VBBMM(n_components = n_comoponents, c = 0.6, d = 0.4, compute_score = True, verbose = True)

bmm = bmm.fit(data)
print bmm

modelclust = bmm.predict(data)
