# Day 4 session

Gabriel Rodrigues Palma discussed various machine learning algorithms, focusing on KNN, perceptron, multi-layer perceptron, and decision trees. He explained KNN's use of Euclidean distance to select neighbors and the impact of hyperparameter tuning on classification accuracy. The perceptron algorithm was described as creating a hyperplane to classify data points, with weights and bias adjusted using gradient descent. The multi-layer perceptron was introduced as multiple hyperplanes, each with its own weights and biases. Decision trees were explained as recursive binary splitting to create a tree for classification, with cost complexity pruning to optimize the tree's complexity. Practical examples and implementation using SK learn were emphasized.:

[🔊 Play Session Review](https://notebooklm.google.com/notebook/b6e3eb9a-31d0-4de2-926d-654803a0ee12/audio)

The audio was created using [Notebooklm](https://notebooklm.google.com)

Action Items
- [ ] Implement the KNN algorithm from scratch and explore the impact of the value of K.
- [ ] Implement the Perceptron algorithm from scratch and understand the role of the weights and bias.
- [ ] Implement the Multi-Layer Perceptron algorithm from scratch and observe the effect of the number of layers.
- [ ] Explore the application of Decision Trees, including the process of recursive binary splitting and cost complexity pruning.

# Outline 
The summary was generated using the live transcription of the morning sessions' content with the help of [Otter.ai](https://otter.ai/).


## KNN Algorithm Overview
- Gabriel Rodrigues Palma introduces the KNN (K-Nearest Neighbors) algorithm, mentioning its characteristics and learning process.
- He explains the concept of estimating data points by collecting K neighbors and ranking them based on Euclidean distance.
- Gabriel discusses the practical application of KNN, using a sample dataset with temperature range, humidity, precipitation, wind, and species presence/absence.
- He highlights the use of KNN as a binary explanatory variable for classification tasks.

## Detailed KNN Algorithm Explanation
- Gabriel elaborates on the process of selecting K neighbors and the tie-breaking methods used in KNN.
- He explains the concept of hyperparameter tuning in KNN, specifically focusing on the value of K.
- Gabriel mentions the use of SK learn for implementing and practicing KNN algorithms.
- He provides a detailed explanation of how KNN works in high-dimensional spaces and the importance of Euclidean distance.

## Perceptron Algorithm Introduction
- Gabriel introduces the perceptron algorithm, describing it as an analogy of a neuron.
- He explains the creation of a hyperplane by the perceptron algorithm to distinguish between classes.
- Gabriel discusses the input variables (X) and the weights (W) used in the perceptron algorithm.
- He explains the concept of a linear combination and the application of a function G to produce an output.

## Perceptron Algorithm Details
- Gabriel delves into the mathematical details of the perceptron algorithm, including the use of a threshold (eta) to classify inputs.
- He explains the concept of a loss function and its role in updating the weights and bias.
- Gabriel discusses the use of gradient descent for learning in the perceptron algorithm.
He provides a step-by-step explanation of how the weights and bias are updated iteratively.

## Multi-Layer Perceptron Algorithm
- Gabriel introduces the multi-layer perceptron (MLP) algorithm, describing it as multiple hyperplanes.
- He explains the concept of a hidden layer in MLP, which consists of multiple neurons.
- Gabriel discusses the linear combination and activation function applied to each neuron in the hidden layer.
- He explains the process of combining the outputs of the hidden layer neurons to produce an output.

## Multi-Layer Perceptron Implementation
- Gabriel provides a detailed explanation of how to code the multi-layer perceptron from scratch.
- He discusses the impact of the number of layers on the input space and the creation of multiple hyperplanes.
- Gabriel explains the process of updating the weights and bias in MLP using gradient descent.
He mentions the use of SK learn for implementing and practicing MLP algorithms.

## Decision Trees Overview
- Gabriel introduces decision trees, explaining their use in creating decisions based on explanatory variables.
- He provides an example of a decision tree using years and hits as explanatory variables.
- Gabriel explains the concept of recursive binary splitting to grow a large tree on the training data.
- He discusses the use of cost complexity pruning to obtain a sequence of best-pruned trees.

## Decision Trees Implementation
- Gabriel explains the use of K-fold cross-validation to choose the best alpha value for decision trees.
- He provides a detailed explanation of how to grow and prune a tree with respect to an alpha value.
- Gabriel discusses the use of SK learn for implementing and practicing decision trees.
- He mentions the applications of decision trees and the importance of choosing the right alpha value.

## Conclusion and Future Topics
- Gabriel concludes the presentation by summarizing the main points covered in the session.
- He mentions the focus on statistical learning for the next session.
- Gabriel encourages participants to practice the algorithms discussed and to explore more details in future sessions.
- He expresses hope that the participants enjoyed the presentation and looks forward to the next session.

