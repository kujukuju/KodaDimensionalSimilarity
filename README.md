
## K-Means Jai

A simple fuzzy k-means implementation.

The api of most k-means implementations I could find was horrible. On top of this, windows and linux typically weren't both entirely supported.

This library solves that problem, and although it is multirhread, it doesn't implement GPU or SIMD support.

By default this library also finds optimized starting cluster centroids by way of k-means++. If you have external data that would allow you to pick better centroids, too bad.