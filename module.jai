
#module_parameters(PARALLEL := true);

// this algorithm consideres 0 to be invalid data and ignores it as a sparse cell
sparse_n_dimensional_distance :: (a: [] float, b: [] float) -> float {
    assert(a.count == b.count, "You cannot compare different numbers of dimensions.");

    // TODO parallel

    total: float64;
    count: int;

    valid: int;
    for i: 0..a.count - 1 {
        if a[i] != 0 && b[i] != 0 {
            valid += 1;
        }
    }

    for i: 0..a.count - 1 {
        if a[i] != 0 && b[i] != 0 {
            // sum { (b - a)^2 / count }
            delta: float64 = (b[i] - a[i]);
            delta *= delta;
            total += delta / valid;
            // total += delta;
        }
    }

    return cast(float) sqrt(total);
}

fuzzy_means :: (points: [] [] float, $K: int, max_iterations: int = 1000, initial_clusters: [] [] float, fuzzification: float = 2, allocator: Allocator = .{}) -> clusters: [K] [] float, iterations: int {
    clusters, iterations := fuzzy_means(points, K, sparse_n_dimensional_distance, max_iterations, initial_clusters, fuzzification, allocator);
    return clusters, iterations;
}

// points: an array of points where it's [number of points] [number of dimensions] value
// k: the number of fuzzy clusters
// distance_function: calculates the distance between two n-dimensional points
// max_iterations: the number of max iterations
// initial_clusters: the initial cluster centers if known
// fuzzification: the weight of a point to a specific cluster is (1 / distance) ^ fuzzification
fuzzy_means :: (points: [] [] float, $K: int, $distance_function: (a: [] float, b: [] float) -> float, max_iterations: int = 1000, initial_clusters: [] [], fuzzification: float = 2, allocator: Allocator = .{}) -> clusters: [K] [] float, iterations: int {
    assert(initial_clusters.count == K, "You must have k initial clusters.");
    
    if points.count == 0 {
        empty: [K] [] float;
        return empty, 0;
    }

    dimensions := points[0].count;
    membership := NewArray(points.count, [K] float, allocator = temp);

    intermediary_flat_clusters64 := NewArray(K * dimensions, float64, allocator = temp);
    intermediary_clusters64: [K] [] float64;
    for i: 0..K - 1 {
        intermediary_clusters64[i].data = intermediary_flat_clusters64.data + i * dimensions;
        intermediary_clusters64[i].count = dimensions;
    }

    intermediary_flat_clusters32 := NewArray(K * dimensions, float32, allocator = temp);
    intermediary_clusters32: [K] [] float32;
    for i: 0..K - 1 {
        intermediary_clusters32[i].data = intermediary_flat_clusters32.data + i * dimensions;
        intermediary_clusters32[i].count = dimensions;
    }
    
    flat_clusters := NewArray(K * dimensions, float, allocator = allocator);
    clusters: [K] [] float;
    for i: 0..K - 1 {
        clusters[i].data = flat_clusters.data + i * dimensions;
        clusters[i].count = dimensions;
    }

    for i: 0..K - 1 {
        memcpy(clusters[i].data, initial_clusters[i].data, dimensions * size_of(float));
    }

    iterations := 0;
    for i: 0..max_iterations - 1 {
        iterations += 1;

        print("On iteration % of %.\n", iterations, max_iterations);

        // calculate the membership weights for each point
        for point, a: points {
            if a % 1000 == 0 {
                print("On point % of %.\n", a + 1, points.count);
            }
            for cluster, b: clusters {
                distance := distance_function(point, cluster);
                membership[a][b] = pow(1.0 / (distance + 1), fuzzification);
            }

            membership_sum: float;
            for b: 0..K - 1 {
                membership_sum += membership[a][b];
            }
            for b: 0..K - 1 {
                membership[a][b] /= membership_sum;
            }

            // this is my attempt to match the wiki algorithm... it just looks wrong
            // distances: [K] float;
            // for cluster, b: clusters {
            //     distance[b] = distance_function(point, cluster);
            // }

            // for cluster, b: clusters {
            //     w: float64;
            //     for c: 0..K - 1 {
            //         if distance[c] < 0.0000001 {
            //             membership[a][b] = 0.0;
            //             continue cluster;
            //         }

            //         w += pow(distance[b] / distance[c], 2 / (fuzzification - 1));
            //     }
            //     w := 1.0 / w;
            //     membership[a][b] = cast(float) (pow(w, fuzzification) * distances[b] * distances[b]);
            // }

            // test
            // #if true {
            //     membership_sum := 0.0;
            //     for b: 0..membership[a].count - 1 {
            //         membership_sum += membership[a][b];
            //     }

            //     assert(membership_sum)
            // }
        }

        convergence := true;

        // relculate cluster centroids based on weighted point cluster memberships
        memset(intermediary_flat_clusters64.data, 0, K * dimensions * size_of(float64));
        memset(intermediary_flat_clusters32.data, 0, K * dimensions * size_of(float32));
        for a: 0..K - 1 {
            print("On cluster % of %.\n", a + 1, K);
            total_membership_weight: float64;
            for point_membership, point_index: membership {
                membership_weight := point_membership[a];
                total_membership_weight += membership_weight;

                for dimension: 0..dimensions - 1 {
                    intermediary_clusters64[a][dimension] += membership_weight * points[point_index][dimension];
                }
            }

            divisor := total_membership_weight; // * points.count;
            for dimension: 0..dimensions - 1 {
                intermediary_clusters32[a][dimension] = cast(float) (intermediary_clusters64[a][dimension] / divisor);
            }

            // for i: 0..dimensions - 1 {
            //     if intermediary_clusters32[a][i] > 0.0000001 {
            //         print("% ", intermediary_clusters32[a][i]);
            //     }
            // }

            dist := distance_function(intermediary_clusters32[a], clusters[a]);
            print("Dist between old and new clusters %\n", dist);
            if convergence && dist >= 0.0000001 {
                convergence = false;
            }

            for dimension: 0..dimensions - 1 {
                clusters[a][dimension] = intermediary_clusters32[a][dimension];
            }
        }

        if convergence {
            break;
        }
    }

    return clusters, iterations;
}

classify_points :: (points: [] [] float, clusters: [$K] [] float, allocator: Allocator = .{}) -> points: [] [K] float {
    return classify_points(points, clusters, sparse_n_dimensional_distance, allocator);
}

classify_points :: (points: [] [] float, clusters: [$K] [] float, $distance_function: (a: [] float, b: [] float) -> float, allocator: Allocator = .{}) -> points: [] [K] float {
    classifications := NewArray(points.count, [K] float, allocator = allocator);

    for point, index: points {
        for i: 0..K - 1 {
            distance := distance_function(point, clusters[i]);
            classifications[index][i] = 1 / (distance + 1);
        }
    }

    return classifications;
}

// kmeans_plus_plus :: (points: [] $T, clusters: *[$K] [] float) -> [] T {
//     // TODO fill in clusters
//     assert(false, "k-means++ isn't supported yet.");
// }

// this function generates the starting clusters by doing a wave form on the dimensional array such that
// treating it as a 1 dimensional array, it selects K evenly distributed points and generates a wave form
// so on the given point the value of that dimension is 1 and it waves down to 0 at each adjacent point
evenly_distributed_maximum_boundaries :: (points: [] [] float, clusters: *[$K] [] float) {
    get_dimensional_distance_for_cluster :: (i: int, dimensions: int) -> float {
        progress := cast(float) i / K;
        return progress * dimensions;
    }

    wave_function :: (distance: float) -> float {
        distance = clamp(distance, -1, 1);
        return cos(distance * PI) * 0.5 + 0.5;
    }

    dimensions := points[0].count;
    dimension_index_half_width := 1.0 / K * dimensions;

    for i: 0..K - 1 {
        dimensions_index_float := get_dimensional_distance_for_cluster(i, dimensions);

        cluster_dimensions := clusters.*[i];
        for a: 0..dimensions - 1 {
            distance := (dimensions_index_float - a) / dimension_index_half_width;

            cluster_dimensions[a] = wave_function(distance);
        }
    }
}

#scope_module

roundi :: (v: float) -> int {
    return cast(int) floor(value + 0.5);
}

#import "Basic";
#import "Math";