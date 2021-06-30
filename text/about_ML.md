# The Science Behind the NC12 Flood CamML

## Overview:

The NC12 Flood CamML is a supervised image recognition (whole image classification) model for detecting flooding in traffic webcam imagery. It consists of a neural network trained 'end to end' in an extremely discriminative approach that explicitly maps the classes to the image features, and optimized to extract the features that explicitly predict the class. The network works by linking an image feature extractor to a classifying head, such that feature extraction is limited to only those that help predict the class. The feature extraction therefore results in classification directly.

The model is based on the popular MobilenetV2 feature extraction model with a custom distillation head (a max pooling layer), and a classifying head (a dense layer with dropout). The model is retrained with data from the scratch, but feature extractor is initialized using imagenet weights. There are a number of hyperparameters, listed below. The items with a * are decided automatically using a model tuner (see below)

1. Image size (we use 224 x 224 pixels)
2. Proportion of data to use for validation (we use 0.5)
3. Batch size (we use 16)
4. Dropout rate*
5. Learning rate*
6. Number of dense neurons*


## Classification targets:
* 0 = Bad Image
* 1 = Not Flooding
* 2 = Not Sure
* 3 = Flooding


## Summary of approach:

1. Use transfer learning to initialize a mobilenet-v2 model with pre-trained weights (from imagenet) as feature extractor
2. Add a classification head with dropout regularization
3. Unfreeze all layers and train from scratch (both the feature extractor and the classification head)
4. Train with 50% of the data for validation, 50% for training
5. Use keras-tuner for hyperparameter (learning rate, dropout rate, and number of classifying densely connected neurons) optimization
6. Use of more than 2 classes, and one-hot encoded labels, enables use of categorical cross-entropy softmax scores to be used as independent probabilities of prediction
7. Models are saved as human-readable json files, with h5 weights, plus a pb version of the model for deployment using R/shiny

Use of automated hyperparameter tuning means this approach may be more adaptable to different datasets and classification targets.

## Why deep learning?

Neural networks excel at classification tasks, but there are two main drawbacks with neural networks consisting of only dense layers for classifying images:

*  Too many parameters means even simple networks on small and low-dimensional imagery take a long time to train.
*  There is no concept of spatial distance, so the relative proximity of image features or that with respect to the classes, is not considered.

For both the reasons above, Convolutional Neural Networks or CNNs have become popular for working with imagery. CNNs train in a similar way to "fully connected" ANNs that use Dense layers throughout.

CNNs can handle multidimensional data as inputs, meaning we can use 3- or more band imagery, which is typical in the geosciences. Also, each neuron isn't connected to all others, only those within a region called the receptive field of the neuron, dictated by the size of the convolution filter used within the layer to extract features from the image. By linking only subsets of neurons, there are far fewer parameters for a given layer

So, CNNS take advantage of both multidimensionality and local spatial connectivity. CNNs tend to take relatively large imagery and extract smaller and smaller feature maps. This is achieved using pooling layers, which find and compress the features in the feature maps outputted by the CNN layers, so images get smaller and features are more pronounced.

## Why MobileNets?
MobileNets were the first model to use "depthwise separable convolution" to reduce the complexity and size of the network, suitable to Mobile devices, or other devices with low computational power.

MobileNetV2 (Sandler et al., 2018) improves on V1 by 1) using linear bottlenecks between the layers, where the last convolution of a residual block has a linear output before it’s added to the initial activations, and 2) using residual connections between the bottlenecks which helps with training convergence and performance.

Here we use it as an all-purpose image feature extractor. We use it with weights learned on the imagenet dataset (or "imagenet weights") for model initialization. It is relatively small, so quick to train (even from scratch, as we will see) and fine-tune. It is compiled for certain sized imagery but works with any size within the range (and the keras implementation automatically detects which weights to use).

## Hyperparameter search (auto-tuning)
We use the hyperband algorithm (Li et al., 2017) implemented in keras-tuner, searching the following hyperparameter space:

1. Number of neurons [min=128, max=512, step=32]
2. Dropout rate [.4,.5,.6]
3. Learning rate [1e-4, 1e-5, 1e-6])


## References

Li, L., Jamieson, K., DeSalvo, G., Rostamizadeh, A. and Talwalkar, A., 2017. Hyperband: A novel bandit-based approach to hyperparameter optimization. The Journal of Machine Learning Research, 18(1), pp.6765-6816.

Sandler, M., Howard, A., Zhu, M., Zhmoginov, A. and Chen, L.C., 2018. Mobilenetv2: Inverted residuals and linear bottlenecks. In Proceedings of the IEEE conference on computer vision and pattern recognition (pp. 4510-4520).