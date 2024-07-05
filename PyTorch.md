# `PyTorch`

Deep learning in Python that runs on the GPU.

## Introduction

### Definitions

1. Deep learning: program infers relationships between user-defined *inputs* and *outputs* (supervised learning)
2. Deep learning is good for 
    * problems with long lists of rules that would be difficult to hardcode
    * continually changing environments 
    * large unstructured datasets with unclear patterns
3. Deep learning is mostly achieved through a multilayered neural network *(hence deep)*
4. Neural networks work by
    1. **Input layer**: *inputs* are numerically encoded into multi-dimensional vectors
    2. **Hidden layer**: vectors are processed by the many hidden layers, program finds patterns in the vectors
    3. **Output layer**: *output* is returned in the form of multi-dimensional vectors
5. There are multiple types of neural networks
    * convolutional neural network (images)
    * transformer (NLP)
6. There are a few kinds of learning
    * supervised learning: BOTH *inputs* and *outputs* are specified for the program
    * unsupervised / self-supervised learning: ONLY *input* is specified for the program
    * transfer learning: one program's *output* is FED to another program as *input*
    * reinforcement learning: program is REWARDED for *ideal* behaviour and discouraged from *unideal* behaviour
7. General workflow of building a deep learning program is
    1. convert raw data to *input tensors*
    2. build a model
        * pick a loss function and optimizer
        * create the training loop
    3. tweak the model to fit the *data* and make a prediction with *output tensors*
    4. evaluate the model
    5. improve the model through iterative experimentation
    6. save and reload
8. Tensor: any numerical representation of data *(most commonly multi-dimensional vectors)*
9. There are different kinds of tensors
   1. scalar: a single number of *0 dimensions*
   2. vector: a number with a direction of *1 dimension*
   3. matrix: a *2-dimensional* array of numbers
   4. tensor: a *n-dimensional* array of numbers
10. Random tensors: important because neural networks take in tensors full of *random numbers* and then adjust those numbers to **better represent** data

## Quickstart

```py
# ----- QUICKSTART -----
    # torch.__version__ => current PyTorch version
    # torch.tensor() => initialises a tensor object
    # torch.rand() => initialises a random tensor object of the specified size
    # .item() => called on a tensor object, which is then returned as a value literal (integer, list literal etc.)
    # .ndim => called on a tensor object to return the number of dimensions a given tensor has
        # observe that the rule of thumb is one dimension is added for every degree of [] square bracket nesting within a tensor object
    # .shape => recursive call on a tensor object to return the number of list elements within a given tensor

# --- DATA SCIENCE PACKAGES TO IMPORT ---

import torch 
import pandas
import numpy
import matplotlib.pyplot as p

print(torch.__version__) # display the current PyTorch version

# --- USER-DEFINED TENSORS ---

scalar = torch.tensor(7) # initialise a scalar tensor object
scalar.item() # returns 7
scalar.ndim # returns 0 dimensions
scalar.shape # returns torch.Size([]) to indicate that there are no list elements

vector = torch.tensor([7, 7]) # intialise a vector tensor object
vector.item() # returns [7, 7]
vector.ndim # returns 1 dimension
vector.shape # returns torch.Size(2) to indicate 2 elements

# - NOTE -
    # by convention, scalar and vector variables are declared in lowercase while matrix and tensor variables are declared in UPPERCASE

MATRIX = torch.tensor(
    [[7, 8], 
    [9, 10]]
)
MATRIX.ndim() # returns 2 dimensions
MATRIX.shape # returns torch.Size([2, 2]) to indicate 2 list elements each containing 2 elements

TENSOR = torch.tensor(
    [[[1, 2, 3],
    [4, 5, 6],
    [7, 8, 9]]]
)
TENSOR.ndim # returns 3 dimensions
TENSOR.shape # returns torch.Size([1, 3, 3]) to indicate 1 list element which contains 3 list elements which contain 3 elements each

WATERMELON = torch.tensor(
    [[[[1, 2],
    [3, 4],
    [5, 6],
    [7, 8],
    [9, 10],
    [11, 12]]]]
)
WATERMELON.ndim # returns 4 dimensions
WATERMELON.shape # returns torch.Size([1, 1, 6, 2]) to indicate 1 list element that contains 1 list element that contains 6 list elements which then contains 2 elements each

# --- RANDOM TENSOR ---

RANDOM_TENSOR = torch.rand(3, 4) # initialises a random tensor of torch.Size([3, 4])
RANDOM_TENSOR.ndim # returns 2 dimensions


```

> continue from 1:08:052 of [this video](https://youtu.be/Z_ikDlimN6A?si=40CGjign3YYuEN3D)

## More on

### Core resources

* [install pytorch locally](https://pytorch.org/get-started/locally/)
* [getting setup with pytorch](https://github.com/mrdbourke/pytorch-deep-learning/blob/main/SETUP.md)
* [pytorch.org](https://pytorch.org/)
* [pytorch documentation](https://pytorch.org/docs/stable/index.html)
* [papers with code trends](https://paperswithcode.com/trends)
* [why pytorch over tensorflow](https://www.reddit.com/r/MLQuestions/comments/112sege/pytorch_vs_tensorflow/)
* [zero to mastery: learn pytorch for deep learning](https://www.learnpytorch.io/)

### Additional resources

* [colab.google](https://colab.google/)
* [tensorflow.org](https://www.tensorflow.org/)
* [keras.io](https://keras.io/)
