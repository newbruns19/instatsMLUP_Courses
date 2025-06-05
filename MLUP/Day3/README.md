# Day 3 session

Gabriel Rodrigues Palma discussed the fundamentals of machine learning, emphasising practical applications and theoretical foundations. He shared his journey starting in 2017, focusing on vegetational type classification in Brazil. Key concepts included supervised learning, unsupervised learning, reinforcement learning, and semi-supervised learning. Gabriel demonstrated Python libraries like NumPy, Pandas, Sklearn, Matplotlib, and Seaborn, highlighting their use in data manipulation, classification, and visualization. He explained metrics such as accuracy, precision, recall, and F1 scores, and used examples like pollinator activity and soil quality classification to illustrate machine learning principles and algorithms:

[🔊 Play Session Review](https://notebooklm.google.com/notebook/27f5f15c-3d78-4343-9bd7-ff72f80a3c41/audio)

The audio was created using [Notebooklm](https://notebooklm.google.com)

Action Items
- [ ] Write a function to analyze species observation data.
- [ ] Implement error handling in the species observation analysis function.
- [ ] Create a class to handle species observation data and analysis.

# Outline 
The summary was generated using the live transcription of the morning sessions' content with the help of [Otter.ai](https://otter.ai/).


## Machine learning fundamentals explained
- Gabriel Rodrigues Palma discusses foundational machine learning elements, focusing on definitions, types, and Python libraries, with a personal journey starting in 2017.
- Gabriel Palma recommends books on machine learning, including "Machine Learning: A Practical Approach" and "The Nature of Statistical Learning Theory."
- Gabriel Palma explains using NumPy, Pandas, SK learn, and visualization tools to define Euclidean distance and create a KNN classifier for assigning classes to new points based on nearest neighbors.

## Machine learning classifies bird species
- Gabriel Rodrigues Palma demonstrates creating a syntactic data set using NumPy for bird species classification
- He splits the data set, scales columns, and applies operations for accurate classification, also mentioning vegetational type analysis for Brazil plot classification.
- Gabriel Palma uses machine learning to classify unknown data points based on temperature, humidity, precipitation, wind, vapour, species presence, and vegetation type.
- Gabriel Rodrigues Palma discusses specific tasks in machine and deep learning, from neural networks to large language models.

## AI history and algorithms
- In 1956, Dartmouth summer research project marked the beginning of AI and machine learning, with discussions on natural language processing, robotics, and machine vision.
- The "AI winter" in the late 60s and early 70s occurred due to the perceptron algorithm's inability to solve complex problems, leading to a decline in neural network research until the 80s.
- Gabriel Rodrigues Palma discussed the evolution of backpropagation algorithm from perception to multi-layer perceptrons and neural networks in the mid 80s.
- A confusion matrix was introduced to evaluate prediction accuracy in binary problems, with metrics like true positive rate, false positive rate, and accuracy.
- Gabriel showed KNN classification implementation in scikit learn, splitting data into train and tests.
- Trained algorithm with arguments for random stage, max depth, and minimal sample, making predictions on unseen data.

## Machine learning concepts explained
- Gabriel Rodrigues Palma explains computing a confusion matrix for pollinators' activity, calculating true positive rate, false positive rate, and accuracy.
- He also discusses creating a receiver operating characteristic (ROC) curve for evaluating predicted values with different thresholds.
- Gabriel Palma explains generalization in machine learning using books as an analogy: training set vs test set
- Supervised learning (labels), unsupervised learning (no labels), reinforcement learning (maximizing rewards), semi-supervised learning (combining labeled and unlabeled data)

## Array operations and machine learning
- He discusses convenience of using NumPy and Scikit-learn for array creation, vectorized operations, and machine learning pipelines.

## Data analysis with multiple algorithms
- Speaker discusses various algorithms: SVM, Gaussian process, decision trees, random forest, neural networks, Naive Bayes
- Discusses pre-processing techniques: standard scaler, min-max scaler, PCA, K-means clustering
- Introduces pipelines for multiple steps: decision trees, SVM, KNN, cross validation scores and model selection
