import time
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import statsmodels.api as sm
from patsy.highlevel import dmatrices

time_begin = time.time()

data = pd.read_parquet("data/dados.parquet")

data = data.rename(columns={'Defeito do Tubo Neural': 'Defeito_do_Tubo_Neural'})

print(data.head())

print(f'Run time: {round(((time.time()-time_begin)/60), 3)} mins')