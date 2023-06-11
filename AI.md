# K-means

Sure, here's a simple implementation of the K-means clustering algorithm in JavaScript without using libraries. Please note that this implementation assumes that you are familiar with basic JavaScript array manipulation methods and syntax.

```javascript
function calculateEuclideanDistance(point1, point2) {
  let distance = 0;
  for (let i = 0; i < point1.length; i++) {
    distance += Math.pow(point1[i] - point2[i], 2);
  }
  return Math.sqrt(distance);
}

function findClosestCentroid(point, centroids) {
  let minDistance = Infinity;
  let closestCentroidIndex = -1;

  for (let i = 0; i < centroids.length; i++) {
    let distance = calculateEuclideanDistance(point, centroids[i]);
    if (distance < minDistance) {
      minDistance = distance;
      closestCentroidIndex = i;
    }
  }
  return closestCentroidIndex;
}

function updateCentroids(data, assignments, centroids) {
  const newCentroids = Array.from({length: centroids.length}, _ => []);
  const counters = Array(centroids.length).fill(0);

  for (let i = 0; i < data.length; i++) {
    const centroidIndex = assignments[i];
    counters[centroidIndex]++;

    for (let j = 0; j < data[i].length; j++) {
      if (newCentroids[centroidIndex][j] === undefined) {
        newCentroids[centroidIndex][j] = 0;
      }
      newCentroids[centroidIndex][j] += data[i][j];
    }
  }

  for (let i = 0; i < newCentroids.length; i++) {
    for (let j = 0; j < newCentroids[i].length; j++) {
      newCentroids[i][j] /= counters[i];
    }
  }
  return newCentroids;
}

function kMeansClustering(data, k, maxIterations = 100) {
  const centroids = data.slice(0, k);
  let assignments = Array(data.length).fill(-1);

  for (let iter = 0; iter < maxIterations; iter++) {
    let updated = false;
    for (let i = 0; i < data.length; i++) {
      const closestCentroidIndex = findClosestCentroid(data[i], centroids);
      if (assignments[i] !== closestCentroidIndex) {
        assignments[i] = closestCentroidIndex;
        updated = true;
      }
    }

    if (!updated) {
      break;
    }
    centroids = updateCentroids(data, assignments, centroids);
  }

  return { centroids, assignments };
}

// Example usage:
const sampleData = [
  [1, 1],
  [2, 2],
  [1, 2],
  [4, 4],
  [5, 5],
  [4, 5]
];

const k = 2;
const result = kMeansClustering(sampleData, k);
console.log("Centroids:", result.centroids);
console.log("Assignments:", result.assignments);
```

Here's the explanation of the code:

1. `calculateEuclideanDistance`: A helper function that calculates the Euclidean distance between two points.
2. `findClosestCentroid`: A helper function that finds the index of the closest centroid to a data point.
3. `updateCentroids`: A function used to update the centroid positions based on the mean of the data points assigned to each centroid.
4. `kMeansClustering`: The main K-means clustering function that takes input data, the desired number of clusters (k) and the maximum number of iterations.

The example usage at the end of the code demonstrates how you could run the K-means algorithm on a simple dataset, and print the cluster centroids and assignments of data points to the clusters.

# Naive Bayes 

A Naive Bayesian Classifier is a machine learning algorithm used to classify objects into classes using the Bayes' Theorem. It assumes that the presence of a feature in a class is unrelated to the presence of any other feature. In simple terms, it believes every feature contributes independently to the probability of the class.

Here's the JavaScript code for a simple Naive Bayesian Classifier:

```javascript
class NaiveBayesianClassifier {
  constructor() {
    this.classes = {};
    this.totalExamples = 0;
  }

  // Train the model with new example and its assigned class
  train(example, className) {
    this.totalExamples++;
    if (!this.classes.hasOwnProperty(className)) {
      this.classes[className] = {
        count: 0,
        features: {},
      };
    }

    const classData = this.classes[className];
    classData.count++;

    for (const feature of example) {
      if (!classData.features.hasOwnProperty(feature)) {
        classData.features[feature] = 0;
      }

      classData.features[feature]++;
    }
  }

  // Classify the given example using the trained model
  classify(example) {
    const probabilities = [];

    for (const className in this.classes) {
      const classData = this.classes[className];
      let probability = Math.log(classData.count / this.totalExamples);

      for (const feature of example) {
        const featureCount = classData.features.hasOwnProperty(feature)
          ? classData.features[feature]
          : 0;
        const featureProbability =
          (featureCount + 1) / (classData.count + Object.keys(classData.features).length);
        probability += Math.log(featureProbability);
      }

      probabilities.push({ className, probability });
    }

    probabilities.sort((a, b) => b.probability - a.probability);
    return probabilities[0].className; // Return the class with the highest probability
  }
}

```

Usage:
```javascript
const classifier = new NaiveBayesianClassifier();
classifier.train(["weather=sunny", "wind=low"], "play");
classifier.train(["weather=rainy", "wind=high"], "not_play");
classifier.train(["weather=sunny", "wind=high"], "not_play");
classifier.train(["weather=sunny", "wind=low"], "play");

console.log(classifier.classify(["weather=sunny", "wind=high"])); // Output: not_play
```

In this example, we've created a `NaiveBayesianClassifier` class with two methods, `train()` for training our model and `classify()` to classify new examples. An example is an array of features.

In the usage example, we train the classifier with weather and wind features to decide whether to play or not. Then, we test the classifier on new data to see if we should play or not.

The reason we add 1 to the feature count is to apply a technique called "Laplace smoothing" or "additive smoothing". It's a technique used to handle cases where a given feature has not been observed in the training data for a particular class.

In a Naive Bayesian Classifier, when we calculate the probability of each class for a given example, we multiply the probabilities of each feature occurring in that class. If a feature is not present in the training data of a class, its count would be 0, and hence the probability would be 0. This would lead to the entire probability of the class being 0, which is not desirable as it eliminates the entire class based on a single missing feature.

By adding 1 to the feature count, we ensure that there is no zero probability for any feature. This smoothens the probability distribution, preventing the classifier from being overly influenced by the absence of a single feature in the training data for a particular class.

It is also worth noting that we add the total number of unique features seen in the denominator part of the featureProbability calculation `(classData.count + Object.keys(classData.features).length)`. This is done to balance the addition of 1 in the numerator and to maintain the probability distribution correctly.
