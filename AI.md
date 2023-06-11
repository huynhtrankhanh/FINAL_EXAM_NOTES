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
