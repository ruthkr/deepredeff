from math import sqrt

import tensorflow as tf
from tensorflow.keras.models import load_model
import tensorflow.keras.backend as K
from tensorflow.keras import models

import numpy as np
# import pandas as pd
import gc
import glob

def pysqrtplus(x):
  return(pysqrt(x) + 10)

def pysqrt(x):
  return(sqrt(x))

def tf_constant(message):
  return(tf.constant(message))
