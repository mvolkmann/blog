---
eleventyNavigation:
  key: PyTorch library
  order: 9
  parent: Python
layout: topic-layout.njk
---

<!-- markdownlint-disable MD013 -->

## Overview

{% aTargetBlank "https://pytorch.org/", "PyTorch" %} is a
deep learning library for Python.
It is a port of {% aTargetBlank "http://torch.ch/", "Torch" %}
which is implemented in C and Lua.
Many PyTorch operations are primarily implemented in C++ and
{% aTargetBlank "https://developer.nvidia.com/cuda-zone", "CUDA" %}.
CUDA is a language created by NVIDIA that is similar to C++
and supports massive parallelism on graphical processing units (GPUs).
Both are frameworks for implementing deep learning algorithms.
The first release of PyTorch was in January, 2017.

Deep learning is a subcategory of artificial intelligence (AI).
It involves training neural networks using large amounts of data
referred to as a "training set".
The result is a function that accepts input similar to the training set data
and outputs something about it.
A classic example is taking a photo of a dog and identifying the breed.

Many applications of deep learning involve image recognition.
Other examples include
bioinformatics,
customer relationship management,
demographic predictions
fraud detection,
game playing,
natural language processing,
recommendation systems, and
speech recognition

Data is supplied using NumPy data structures.

"Feature engineering" involves manually
determining significant input features to measure,
implementing their detection and measurement,
and writing an algorithm to combine feature measurements
in order to classify inputs.
Contrast this to deep learning where feature detection,
measurement, and combining measurements is done
automatically through training on large sets of inputs.
These two approaches, feature engineering and deep learning,
can be combined.

Deep learning typically computes a numerical score for
a set of inputs (such as the pixels of an image)
and determines the difference between that score and the expected score.
Training serves to incrementally lower these differences.

Currently the most popular alternative to PyTorch is TensorFlow.
The code written to use PyTorch tends to be more "pythonic" (idiomatic Python).
PyTorch is also regarded as easier to learn than TensorFlow.
Their features sets overlap significantly.

PyTorch provides the "tensor" data structure which is a multidimensional array
similar to arrays in NumPy. And just like NumPy,
PyTorch implements highly optimized operations on this data structure,
that have an API similar to NumPy.
This operations are especially fast
when run on graphical processing units (GPUs), often
providing a speed improvement in the neighborhood of 50 times.

PyTorch provides a `DataLoader` class that can load data in the background
in preparation for use by the training loop.
Each iteration of the training loop
evaluates the current model using data from the `DataLoader`.
Model outputs are compared to target outputs using a loss function.
The PyTorch "autograd" engine then modifies the model
in order to produce outputs that are closer to targets.

TorchScript can be used to compile models ahead of time.
This results in a set of instructions that can be executed
in environments that do not use Python such as
C++ applications or mobile devices.

Full training for complex models and large datasets
typically require access to a CUDA-capable GPU
in order to complete in a reasonable amount of time
(hours versus days).

Some cloud platforms provide online access to Jupyter Notebooks
that have PyTorch preinstalled and can process code using GPUs.
One example is
{% aTargetBlank "https://colab.research.google.com", "Colabortory" %}.

## Conventions

| Variable Name Suffix | Type        |
| -------------------- | ----------- |
| `_a`                 | NumPy array |
| `_g`                 | GPU memory  |
| `_t`                 | tensor      |
