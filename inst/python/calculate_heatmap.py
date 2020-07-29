import tensorflow as tf
from tensorflow.keras.models import load_model
import tensorflow.keras.backend as K
from tensorflow.keras import models

import numpy as np
import gc
import glob

@tf.function
def get_heatmap_with_tf_function(dataset, nth_data, model, layer):

    # Get the single encoded sequence data
    data = tf.convert_to_tensor(dataset[nth_data:nth_data+1, :, :])

    # Get layer output
    get_layer = model.get_layer(layer)
    heatmap_model = models.Model([model.input], [get_layer.output, model.output])

    # Get gradient of the winner class w.r.t. the output of the (last) conv. layer
    with tf.GradientTape() as gtape:
        conv_output, predictions = heatmap_model([data])
        loss = predictions[:, 0]
        grads = gtape.gradient(loss, conv_output)[0]
        pooled_grads = K.mean(grads, axis=(0, 1))

    heatmap = tf.reduce_mean(tf.multiply(pooled_grads, conv_output), axis=-1)
    heatmap = tf.maximum(heatmap, 0)
    max_heat = tf.keras.backend.max(heatmap)
    heatmap /= max_heat

    return(heatmap)
    
def loading_model(model_path):
  model = load_model(model_path)
  return(model)
    
def get_append_heatmap(data_name, model, layer, from_sample, sample_length):
  

    dataset = data_name
    print("Data shape: " + str(dataset.shape[0]))

    min_sample = from_sample
    max_sample = np.min([from_sample + sample_length, dataset.shape[0]])
    result = []

    
    print("")
    # For loop to get the heatmap for each matrix
    for nth_sample in range(min_sample, max_sample):
        # print("Getting the heatmap for the data: sample " + str(nth_sample) + "/" + str(max_sample-1))
        heatmap = get_heatmap_with_tf_function(dataset, nth_sample, model, layer)

        # Put all of the reults together
        result.append(heatmap)
        
        # Free memory (useless)
        gc.collect()

    # Change the list to numpy array
    all_matrices = np.array(result)

    print("")
    # print("Saving results in", saving_path)
    # np.save(saving_path, all_matrices)
    return(all_matrices)
    
def get_sum_heatmap(data):

    sum_all_matrices = np.sum(data, axis = 0)
    
    # Calculate heatmap
    heatmap = np.maximum(sum_all_matrices, 0)
    heatmap /= np.max(heatmap)
    heatmap = np.reshape(heatmap, data.shape[2])

    return(heatmap)
    
    
def get_key(mydict, element):
    key = list(mydict.keys())[list(mydict.values()).index(element)]
    return(key)

amino = ['R', 'K', 'D', 'E', 'Q', 'N', 'H', 'S', 'T', 'Y', 'C', 'W', 'A', 'I', 'L', 'M', 'F', 'V', 'P', 'G']
token_index = dict(zip(range(1, (len(amino)+1)), amino))

def get_encoding(mydata, max_length):
    results = np.zeros((len(mydata), max_length, max(token_index.keys())), dtype=np.float32)
    for i, sample in enumerate(mydata):
        for j, character in enumerate(sample):
            if character in token_index.values():
                index = get_key(token_index, character) - 1
                results[i, j, index] = 1. 
            else:
                results[i, j, :] = results[i, j, :]
    return results
    
    
def get_smooth(data, threshold):
    

    # transform real data to the frequency domain (Fourier transform)
    real_ft = np.fft.rfft(data)
    
    # remove undesired frequencies
    real_ft[threshold:] = 0 
    
    # move back to the time domain. (Inverse fourier transform)
    smooth_data = np.fft.irfft(real_ft)
    
    return(smooth_data)

