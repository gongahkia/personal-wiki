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
10. Random tensors: important because neural networks take in tensors full of *random numbers* and then adjust those numbers via tensor operations (addition, subtraction, simple, element and matrix multiplication, division) to **better represent** data

### Quickstart

```py
# ----- QUICKSTART -----
    # %%time => CPU time and Wall time for the execution of a given Jupyter notebook cell
    # torch.__version__ => current PyTorch version
    # torch.tensor() => initialises a tensor object literal, and can receive additional arguments
        # dtype => specifies the datatype of each element of the tensor
            # None
            # .bool => True, False
            # .float16
            # .float32 (assigned by default)
            # .float64
            # .complex32
            # .complex64
            # .complex128
            # .int8
            # .int16
            # .int32
            # .int64
            # .uint8
            # .uint16
            # .uint32
            # .uint64
            # .quint8
            # .qint8
            # .qint32
            # .quint4x2
            # .float8_e4m3fn
            # .float8_e5m2
        # device => specifies the device each tensor lives on
            # cpu
            # cuda
            # mps
            # xpu
            # xla
            # meta
        # requires_grad => specifies whether PyTorch should track the gradient of a tensor when it undergoes numerical calculations
            # True
            # False
    # torch.rand() => initialises a random tensor object of the specified torch.Size()
    # torch.zeros() => initialises a tensor of all zeros of the specified torch.Size(), most commonly used to create a mask
    # torch.ones() => initialises a tensor of all ones of the specified torch.Size(), most commonly used to create a mask
    # torch.zeros_like => initialises a tensor of all zeros of the torch.Size() from another specified tensor
    # torch.ones_like => initialises a tensor of all ones of the torch.Size() from another specified tensor
    # torch.arange(start, end, step) # initialises a tensor object literal from a range created from the specified start, end and step
    # .item() => called on a tensor object, which is then returned as a value literal (integer, list literal etc.)
    # .ndim => called on a tensor object to return the number of dimensions a given tensor has
        # observe that the rule of thumb is one dimension is added for every degree of [] square bracket nesting within a tensor object
    # .shape => recursive call on a tensor object to return the number of list elements within a given tensor
    # .dtype => method that returns the datatype of the specified variable it is called upon, PyTorch assigns the default datatype of .float32 if unspecified
    # .device => method that returns the current device of a given tensor object

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

RANDOM_IMAGE_SIZE_TENSOR = torch.rand(size=(224, 224, 3)) # initialises a random tensor with a similar shape to an image tensor, specifying the height, width and color channel
RANDOM_IMAGE_SIZE_TENSOR.ndim # returns 3 dimensions as we specified above
RANDOM_IMAGE_SIZE_TENSOR.shape # returns torch.Size([224, 224, 3]) as we specified above

# --- TENSOR OF ALL 0s ---

ZERO_TENSOR = torch.zeros(size=(3, 4)) # initialises a tensor of all zeros of the torch.Size([3, 4])

# --- TENSOR OF ALL 1s ---

ONE_TENSOR = torch.ones(size=(3, 4)) # initialises a tensor of all ones of the torch.Size([3, 4])

# --- RANGE TENSOR ---

zero_to_nine = torch.arange(0, 10) # initialises the tensor object literal tensor([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])
two_to_eleven = torch.arange(start=2, end=11, step=1) # initialises the tensor object literal tensor([2, 3, 4, 5, 6, 7, 8, 9, 10, 11])

# --- TENSORS LIKE ---

ten_zeroes = torch.zeros_like(input=zero_to_nine) # initialises a zero tensor of the same shape as the specified input tensor
ten_ones = torch.ones_like(input=zero_to_nine) # initialises a one tensor of the same shape as the specified input tensor
```

### Tensor operations

```py
# ----- TENSOR OPERATIONS -----
    # + => addition applied to each element of the tensor
    # - => subtraction applied to each element of the tensor
    # * => simple multiplication of a matrix against a scalar number
    # / => division applied to each element of the tensor
    # * => ELEMENT multiplication of two matrices, where each element of a matrix is multipled against its corresponding element in the other matrix
    # torch.matmul() => MATRIX multiplication that finds the DOT PRODUCT of two specified matrices by multiplying them together
        # observe that matrix multiplication must satisfy the following 2 rules
            # 1. inner dimensions of the two matrices must match
                # torch.matmul(torch.rand(2, 3), torch.rand(2, 3)) WON'T work
                # torch.matmul(torch.rand(2, 3), torch.rand(3, 2)) WILL work
                # torch.matmul(torch.rand(3, 2), torch.rand(2, 3)) WILL work
            # 2. result matrix must have the shape of the outer dimensions
                # torch.matmul(torch.rand(2, 3), torch.rand(3, 2)) results in torch.Size([2, 2]) so this WILL work
                # torch.matmul(torch.rand(3, 2), torch.rand(2, 3)) results in torch.Size([3, 3]) so this WILL work
    # .T => method that tranposes the shape of the specified tensor by switching its dimensions (axis), particularly useful for when tensor shape errors occur
    # torch.min() => aggregator method that finds the element with the minimum value in a given tensor 
    # torch.max() => aggregator method that finds the element with the maximum value in a given tensor 
    # torch.mean() => aggregator method that finds the average value of all elements within a given tensor, note the element datatype must be a floating or complex type
    # torch.sum() => aggregator method that finds the sum of all elements within a given tensor
    # torch.argmin() => aggregator method that finds the index of the element with the minimum value in a given tensor 
    # torch.argmax() => aggregator method that finds the index of the element with the maximum value in a given tensor 

# - NOTE -
    # recall that we have to reassign the result of a tensor operation to a variable for the value to be stored, similar to anywhere else in Python and most other programming languages really

# intialisation of tensor object literals
tensor = torch.tensor([1, 2, 3]) 
another_tensor = torch.tensor(
    [[7, 8],
    [9, 10],
    [11, 12]]
)

tensor + 10 # addition that evaluates to the tensor object literal tensor([101, 102, 103])
tensor - 10 # subtraction that evaluates to the tensor object literal tensor([-9, -8, -7])
tensor * 10 # simple multiplication evaluates to the tensor object literal tensor([10, 20, 30])
tensor / 10 # divison evaluates to the tensor object literal tensor([0.1, 0.2, 0.3])

tensor * tensor # element multiplication evaluates to the tensor object literal tensor([1, 4, 9])
torch.matmul(tensor, tensor) # matrix multiplication evalutes to the dot product value torch(14)

another_tensor.T # transpose operation that evaluates to the tensor object literal tensor([[7, 9, 11], [8, 10, 12]])
another_tensor.T.shape # transpose operation means this will now return the torch.Size([2, 3])
```

> continue from 2:02:59 of [this video](https://youtu.be/Z_ikDlimN6A?si=40CGjign3YYuEN3D) and add code above here

## Doing actual things with Tensors

Enough yapping, I want to build something.

### Encode an Image to a Tensor

> FUA continue adding here later when its covered in the video

1. Split the image into its RGB *(red green blue)* color channels
2. Represent that as a tensor with the shape *(`color_channels`, `image_height`, `image_width`)*

```py

```

## More on

### Core resources

* [install pytorch locally](https://pytorch.org/get-started/locally/)
* [getting setup with pytorch](https://github.com/mrdbourke/pytorch-deep-learning/blob/main/SETUP.md)
* [pytorch.org](https://pytorch.org/)
* [pytorch documentation](https://pytorch.org/docs/stable/index.html)
* [papers with code trends](https://paperswithcode.com/trends)
* [why pytorch over tensorflow](https://www.reddit.com/r/MLQuestions/comments/112sege/pytorch_vs_tensorflow/)
* [zero to mastery: learn pytorch for deep learning](https://www.learnpytorch.io/)

### Prerequisite knowledge 

* [learn python in y minutes](https://learnxinyminutes.com/docs/python/)
* [how to find dot product](https://www.mathsisfun.com/algebra/matrix-multiplying.html)

### Additional resources

* [colab.google](https://colab.google/)
* [tensorflow.org](https://www.tensorflow.org/)
* [keras.io](https://keras.io/)
