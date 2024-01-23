# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python Docker image: https://github.com/kaggle/docker-python

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the read-only "../input/" directory

import os
for dirname, _, filenames in os.walk('/kaggle/input/basicshapes/shapes/shapes'):
    for filename in filenames:
        if filename == filenames[1]:
                print(os.path.join(dirname, filename))

# You can write up to 20GB to the current directory (/kaggle/working/) that gets preserved as output when you create a version using "Save & Run All" 
# You can also write temporary files to /kaggle/temp/, but they won't be saved outside of the current session

!pip install transformers

from transformers import ViTImageProcessor, ViTModel
from PIL import Image
import requests
#import PIL

#PIL.ImageFile.LOAD_TRUNCATED_IMAGES = True

url = '/kaggle/input/basicshapes/shapes/shapes/circles/drawing(1).png'
image = Image.open(url)

processor = ViTImageProcessor.from_pretrained('facebook/dino-vitb16')
model = ViTModel.from_pretrained('facebook/dino-vitb16')

inputs = processor(images=image, return_tensors="pt")
outputs = model(**inputs)
last_hidden_states = outputs.last_hidden_state


n = 60 # set number of images to include

from transformers import ViTImageProcessor, ViTModel
from PIL import Image
import requests

# imgs <- list.files(path = "../input", pattern = "*.png", full.names = TRUE, recursive = TRUE)

#imgs_cir = list.files(path = "../input/basicshapes/shapes/circles/", pattern = "*.png", full.names = FALSE, recursive = FALSE)
#imgs_sq = list.files(path = "../input/basicshapes/shapes/squares/", pattern = "*.png", full.names = FALSE, recursive = FALSE)
#imgs_tri = list.files(path = "../input/basicshapes/shapes/triangles/", pattern = "*.png", full.names = FALSE, recursive = FALSE)

processor = ViTImageProcessor.from_pretrained('facebook/dino-vitb16')
model = ViTModel.from_pretrained('facebook/dino-vitb16')
tensors = list()

for dirname, _, filenames in os.walk('/kaggle/input/basicshapes/shapes/shapes'): # fixed the path
    for filename in filenames[0:20]:
        url = os.path.join(dirname, filename)
        image = Image.open(url)
        print(os.path.join(dirname, filename))
        inputs = processor(images=image, return_tensors="pt")
        outputs = model(**inputs)
        last_hidden_states = outputs.last_hidden_state
        tensors.append(last_hidden_states)


# save the tensors to use with R
# reshape 3d tensor to 1d vector

# this selects every element except the third
smaller = tensors[0].reshape(-1,3)[:,:2].reshape(-1)
smaller

# this halves the size
smaller = tensors[0].reshape(-1,3)[:,:1].reshape(-1)
smaller.shape

tensors_small = list()
for tensor in range(len(tensors)):
    tensors_small.append(tensors[tensor].reshape(-1,3)[:,:1].reshape(-1))
    
for tensor in range(len(tensors_small)):
    tensors_small[tensor] = tensors_small[tensor].detach().numpy()
    
df = pd.DataFrame(np.vstack(tensors_small))

df.to_csv("testfile",index=False) #save to file
