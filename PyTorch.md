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

![](https://www.researchgate.net/publication/363930739/figure/fig2/AS:11431281224920472@1708517958416/Venn-Diagram-relationship-between-artificial-intelligence-machine-learning-and-deep.jpg)

4. Neural networks work by
    1. **Input layer**: *inputs* are numerically encoded into multi-dimensional vectors
    2. **Hidden layer**: vectors are processed by the many hidden layers, program finds patterns in the vectors
    3. **Output layer**: *output* is returned in the form of multi-dimensional vectors

![](https://www.researchgate.net/publication/332158639/figure/fig1/AS:743347375312897@1554239233244/Simple-neural-network-diagram.png)

5. There are multiple types of neural networks
    * convolutional neural network (images)
    * transformer (NLP)
6. There are a few kinds of learning
    * supervised learning: BOTH *inputs* and *outputs* are specified for the program
    * unsupervised / self-supervised learning: ONLY *input* is specified for the program
    * transfer learning: one program's *output* is FED to another program as *input*
    * reinforcement learning: program is REWARDED for *ideal* behaviour and discouraged from *unideal* behaviour

![](https://www.researchgate.net/publication/343748539/figure/fig6/AS:963538214465538@1606736820183/Supervised-unsupervised-and-reinforcement-learning-165-171.png)

7. Tensor: any numerical representation of data *(most commonly multi-dimensional vectors)*
8. General workflow of building a deep learning program is
    1. convert raw data to *input tensors*
    2. build a model
        * pick a loss function and optimizer
        * create the training loop
    3. tweak the model to fit the *data* and make a prediction with *output tensors*
    4. evaluate the model
    5. improve the model through iterative experimentation
    6. save and reload

![](https://www.scaler.com/topics/images/this-detailed-resource.webp)

## Quickstart

> continue from 1:08:052 of [this video](https://youtu.be/Z_ikDlimN6A?si=40CGjign3YYuEN3D)

```py

```

## More on

### Core resources

* [pytorch.org](https://pytorch.org/)
* [pytorch documentation](https://pytorch.org/docs/stable/index.html)
* [papers with code trends](https://paperswithcode.com/trends)
* [why pytorch over tensorflow](https://www.reddit.com/r/MLQuestions/comments/112sege/pytorch_vs_tensorflow/)
* [zero to mastery: learn pytorch for deep learning](https://www.learnpytorch.io/)

### Additional resources

* [colab.google](https://colab.google/)
* [tensorflow.org](https://www.tensorflow.org/)
* [keras.io](https://keras.io/)