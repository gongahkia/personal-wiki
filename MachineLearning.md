# `Machine learning`

**Understand** theoretical basis for neural networks and **practise** tools to build deep learning models.

Covers...

* Feed Forward Neural Networks
* Convolutional Neural Networks
* Recurrent Neural Networks
* Autoencoders
* Reinforcement Learning
* Attention (through transformers)

## Definitions

1. Neural network: model comprised of many *artificial neurons* that takes in *training examples* as input and **infers rules** to arrive at a specified output (accuracy increases as the sample size of training examples grows larger)
2. Artifical neuron: basic building block of neural networks, of which there are 2 main types
    1. Perceptron
        * older model developed in 1950s to 1960s by Frank Rosenblatt
        * each perceptron receives a **binary** input and returns a single **binary** output
        * binary output is determined by whether the weighted sum surpasses a designated *threshold value*
        * perceptron's model too simplistic since even a small $\Delta$ in a perceptron's weight could result in a large $\Delta$ flip of its binary output
    2. Sigmoid neuron
        * deep learning models required an artificial neuron that allowed a small $\Delta$ in its weight to result in a corresponding small $\Delta$ in its output
        * each sigmoid neuron receives one or more inputs of **floating-point** value and returns a single **floating-point** output
        * output is calculated by mapping the [sigmoid function](https://www.learndatasci.com/glossary/sigmoid-function/) onto each input training example
3. Weights: real number ($\mathbb{R}$) that expresses the importance a given *input* has to its corresponding *output*
4. Bias: negative threshold value
5. Input layer: first layer of neurons in a neural network that are fed as *input* to the model
6. Hidden layer(s): any number of intermediary layers of neurons in a neural network whose *outputs* are fed as *inputs* to the next layer of neurons
7. Output layer: final layer in a neural network, where a single neuron's *output* is the returned value of the entire model
8. Feed forward neural network: *output* from one layer is *input* for another layer, modelled mathematically as $f(g(h(x)))$ where information is only **fed forward**
    * more useful for deep learning models
9. Recurrent neural network: *output* from one layer is fed as staggered *input* to the **same layer**, modelled mathematically as $f(f(f(x)))$ where recursive feedback loops are allowed
    * less useful for deep learning models
    * more accurately simulates how the human brain handles and reinforces information
10. Machine learning: process by which machines *learn* to perform tasks they were not explicitly programmed to, of which there are 4 variants
    * supervised: model takes in known *input* and purposefully **predicts** a desired *output*
    * unsupervised: model takes in known *input* and derives/describes **patterns** observed from the *input*
    * parametric: model takes in a *fixed* number of input parameters
    * non-parametric: model takes in an *unspecified, possibly infinite* number of input parameters

> [!NOTE]  
> Machine learning models are either Parametric OR Non-parametric *and* Supervised OR Unsupervised.  

11. Mean squared error (MSE): measures **degree of inaccuracy** a predicted *output* has compared to the actual *output*
12. Gradient descent: attributes error by **allocating blame** for a non-zero MSE value to a specific neuron's *weight*, then tweaking that *weight* to decrease the MSE in one of three ways  
    1. Full gradient descent: neural network calculates the AVERAGE *weights* over the entire training example dataset for a minimum MSE, and weights are tweaked after the FULL AVERAGE has been computed
    2. Stochastic gradient descent: repeatedly iterates through the entire training example dataset, tweaking weights for EACH *input* value based on the MSE, until a weight configuration that works for ALL training examples is arrived at  
    3. Batch gradient descent: a BATCH size of $n$ is specified beforehand, and the neural network updates the *weights* after $n$ training examples have been fed to the model as *input*

> [!TIP]  
> Most situations designed to train deep learning models can be modelled with matrices using [NumPy](https://numpy.org/) and [pandas](https://pandas.pydata.org/).  

13. Natural language processing (NLP): parses text for the following three purposes
    1. Label a region of text *(speech tagging, sentiment classification, named-entity recognition)*
    2. Link 2 or more regions of text *(co-reference)*
    3. Fill in missing information based on context

## More on 

* [No bullshit guide to math and physics](https://www.amazon.com/No-bullshit-guide-math-physics/dp/0992001005) by Ivan Savov
* [No bullshit guide to linear algebra](https://www.amazon.sg/No-Bullshit-Guide-Linear-Algebra/dp/0992001021) by Ivan Savov
* [Data Science from Scratch: First Principles with Python](https://www.amazon.sg/Data-Science-Scratch-Principles-Python/dp/1492041130) by Joel Grus
* [The StatQuest Illustrated Guide To Machine Learning](https://www.amazon.sg/StatQuest-Illustrated-Guide-Machine-Learning/dp/B0BLM4TLPY) by Josh Starmer
* [Neural Networks and Deep Learning](https://neuralnetworksanddeeplearning.com/index.html) by Michael Nielsen  
* [grokking Deep Learning](https://edu.anarcho-copy.org/Algorithm/grokking-deep-learning.pdf) by Andrew W Trask  
* [The Hundred-page Machine Learning Book](http://ema.cri-info.cm/wp-content/uploads/2019/07/2019BurkovTheHundred-pageMachineLearning.pdf) by Andriy Burkov  
* [Stanford CS25: V2 Introduction to Transformers](https://youtu.be/XfpMkf4rD6E?si=x--zvoBHV1X9IohG) by Andrej Karpathy  
* [How to learn machine learning as a complete beginner: a self-study guide](https://youtu.be/0F2paWV4eEA?si=cX3M7lJHLuOZJqKJ)  
