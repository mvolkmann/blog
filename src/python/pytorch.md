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
Note that the GPUs in current Apple laptops do not support CUDA
and precompiled versions of PyTorch for macOS only utilize CPUs.
Currently, CUDA support on macOS is only available
by building PyTorch from source

Some cloud platforms provide online access to Jupyter Notebooks
that have PyTorch preinstalled and can process code using GPUs.
One example is
{% aTargetBlank "https://colab.research.google.com", "Colabortory" %}.

## Installing PyTorch

To install PyTorch using pip, enter
`pip install torch torchvision`.

To install PyTorch using Anaconda, enter
`conda install pytorch torchvision -c pytorch`

To get the version of PyTorch that is installed,
run the following code:

```python
import torch
print(torch.__version__)
```

## Using PyTorch from Jupyter Lab

1. Run the Anaconda-Navigator app.
   (For details on installing this, see [here](/blog/python/anaconda/).)
1. Click the "Launch" button for JupyterLab.
1. It will open in a new tab in your default web browser.
1. In a cell of a notebook, enter the code above to
   determine the version of PyTorch available from the notebook.
1. Press shift-enter to execute the cell.

## Using a pre-trained model

Creating a deep learning model is very time consuming.
A large amount of training data must be acquired and prepared.
A model must be designed.
Training the model can take many hours.

Another option it to use a pre-trained model created by someone else.
{% aTargetBlank "https://pytorch.org/hub/", "PyTorch Hub" %}
provides some of these.
Another source is the
{% aTargetBlank "http://github.com/pytorch/vision", "TorchVision" %} project.
Three example included in TorchVision are
{% aTargetBlank "http://mng.bz/lo6z", "AlexNet" %},
{% aTargetBlank "http://arxiv.org/pdf/1512.03385.pdf", "ResNet" %},
and {% aTargetBlank "http://arxiv.org/pdf/1512.00567.pdf", "Inception v3" %}.

AlexNet won a 2012 ImageNet Large Scale Visual Recognition Competition (ILSVRC).
It achieved a top-5 test error rate of 15.4% which means that
it failed to include the correct label in its top five predictions
only 15.4% of the time.
It 2020 top-5 error rates can be 3% or lower.

Here is code to perform image recognition using
a ResNet 101-layer convolutional neural network:

```python
from PIL import Image
import torch
from torchvision import models, transforms
import torch.nn.functional as F

# Create an instance of a neural network.
# This has been trained to recognize only the 1000 things
# listed in the file imagenet_classes.txt file
# that is read in later.
resnet = models.resnet101(pretrained=True)

# Put the network into "eval" mode because we want to
# evaluate input rather than perform training.
resnet.eval()

# Load an image.
img_path = 'my-dog.jpg'
img = Image.open('./' + img_path)

# Open the image in the default image app (ex. Preview).
#img.show()

# Prepare the image for input to the network.
preprocess = transforms.Compose([
    # Resize the image to reduce the number of pixels to be processed.
    transforms.Resize(256),

    # Crop the image to a smaller size about its center,
    # removing unnecessary pixels at the edges.
    transforms.CenterCrop(224),

    # Convert the image data to a tensor object.
    transforms.ToTensor(),

    # Normalize the red/green/blue values of the pixels
    # to have specific mean and standard deviation values
    # which somewhat improves the recognition.
    # But why these values?
   transforms.Normalize(
       mean=[0.485, 0.456, 0.406], # [red, green, blue]
       std=[0.229, 0.224, 0.225] # [red, green, blue]
   )
])
img_t = preprocess(img)
#print(img_t.size()) # torch.Size([3, 224, 224])

# Create a 1D tensor object from the image
# with the data starting at index zero.
# Why isn't zero the default?
batch_t = torch.unsqueeze(img_t, 0)
#print(batch_t.size()) # torch.Size([1, 3, 224, 224])

# Run the inference engine to get predicted classes.
# "out" is set to a tensor that contains percentage predictions
# for each of the 1000 possible labels.
out = resnet(batch_t)
#print('out size =', out.size()) # torch.Size([1, 1000])

# Get the 1000 possible labels from a text file.
with open('./dlwpt-code-master/data/p1ch2/imagenet_classes.txt') as f:
    labels = [line.strip() for line in f.readlines()]

# Find the index with the highest score.
# _ is set to the highest value, but we don't need that.
_, index = torch.max(out, 1)
# We can also get the tensor value at this index with
# out.detach().numpy()[0][index])

# Compute the percentage certainty from each tensor value.
percentages = F.softmax(out, dim=1)[0] * 100

# Output the label and percentage associated
# with the index of the highest score.
i = index[0]
label = labels[i]
pct = percentages[i].item()
print(f'I am {round(pct, 2)}% sure this is a {label}.\n')

# Output the top predictions.
_, indices = torch.sort(out, descending=True)
n = 5
print('My top', n, 'predictions are:')
for i in indices[0][:n]:
    label = labels[i]
    pct = percentages[i].item()
    print(f'{label} - {round(pct, 2)}%')
```

Here is sample output from the code above:

```text
I am 95.97% sure this is a whippet.

My top 5 predictions are:
whippet - 95.97%
Italian greyhound - 2.44%
basenji - 0.59%
Ibizan hound, Ibizan Podenco - 0.52%
toy terrier - 0.15%
```

The first time this code is run, it downloads the model
which takes several seconds. Where does it go?

## Data sources

{% aTargetBlank "http://imagenet.stanford.edu", "ImageNet" %}
hosts a collection of over 14 million labeled images (with nouns)
that can be used as input to deep learning models.
It is maintained by Stanford University.

## Image recognition

When deep learning is used for image recognition,
images used for training and images used for recognition
are converted to one-dimensional arrays of pixel values
represented by a `torch.Tensor` object.
Typically the images are fairly small,
to avoid excessively long training times.
The output is a set of predicted result classes
that each have a human-readable label and a score,
sorted from most to least likely match.

## Conventions

| Variable Name Suffix | Type        |
| -------------------- | ----------- |
| `_a`                 | NumPy array |
| `_g`                 | GPU memory  |
| `_t`                 | tensor      |
