
#module_parameters(PARALLEL := true);

// this algorithm considers 0 to be invalid data and ignores it as a sparse cell
sparse_n_dimensional_distance :: (a: [] float, b: [] float) -> float {
    assert(a.count == b.count, "You cannot compare different numbers of dimensions.");

    total: float64;
    count: int;

    valid: int;
    for i: 0..a.count - 1 {
        if a[i] != 0 && b[i] != 0 {
            valid += 1;
        }
    }

    if valid == 0 {
        return FLOAT32_MAX;
    }

    for i: 0..a.count - 1 {
        if a[i] != 0 && b[i] != 0 {
            // sum { (b - a)^2 / count }
            delta := b[i] - a[i];
            delta *= delta;
            total += delta / valid;
        }
    }

    return cast(float) sqrt(total);
}

n_dimensional_distance :: (a: [] float, b: [] float) -> float {
    assert(a.count == b.count, "You cannot compare different numbers of dimensions.");

    total: float64;

    for i: 0..a.count - 1 {
        delta := b[i] - a[i];
        delta *= delta;
        total += delta;
    }

    return cast(float) sqrt(total);
}

cosine_similarity :: (a: [] float, b: [] float) -> float {
    dot: float64;
    a_mag: float64;
    b_mag: float64;

    for i: 0..a.count - 1 {
        dot += a[i] * b[i];
        a_mag += a[i] * a[i];
        b_mag += b[i] * b[i];
    }

    if a_mag * b_mag < 0.000001 {
        return 0.0;
    }

    return cast(float) dot / (sqrt(a_mag) * sqrt(b_mag));
}

sparse_cosine_similarity :: (a: [] float, b: [] float) -> float {
    dot: float64;
    a_mag: float64;
    b_mag: float64;

    for i: 0..a.count - 1 {
        if a[i] == 0 || b[i] == 0 {
            continue;
        }

        dot += a[i] * b[i];
        a_mag += a[i] * a[i];
        b_mag += b[i] * b[i];
    }

    if a_mag * b_mag < 0.000001 {
        return 0.0;
    }

    return cast(float) dot / (sqrt(a_mag) * sqrt(b_mag));
}

cosine_distance :: (a: [] float, b: [] float) -> float {
    return 1 - cosine_similarity(a, b);
}

sparse_cosine_distance :: (a: [] float, b: [] float) -> float {
    return 1 - sparse_cosine_similarity(a, b);
}

popularity :: (dimensions: [] float) -> float {
    length_squared: float64;

    for dimension: dimensions {
        length_squared += dimension * dimension;
    }

    return cast(float) sqrst(length_squared);
}

fuzzy_means :: (points: [] [] float, $K: int, max_iterations: int = 1000, initial_clusters: [] [] float, fuzzification: float = 2, allocator: Allocator = .{}) -> clusters: [K] [] float, membership: [] [K] float, iterations: int {
    clusters, membership, iterations := fuzzy_means(points, K, sparse_n_dimensional_distance, max_iterations, initial_clusters, fuzzification, allocator);
    return clusters, membership, iterations;
}

// points: an array of points where it's [number of points] [number of dimensions] value
// k: the number of fuzzy clusters
// distance_function: calculates the distance between two n-dimensional points
// max_iterations: the number of max iterations
// initial_clusters: the initial cluster centers if known
// fuzzification: the weight of a point to a specific cluster is (1 / distance) ^ fuzzification
fuzzy_means :: (points: [] [] float, $K: int, $distance_function: (a: [] float, b: [] float) -> float, max_iterations: int = 1000, initial_clusters: [] [] float, fuzzification: float = 2, allocator: Allocator = .{}) -> clusters: [K] [] float, membership: [] [K] float, iterations: int {
    assert(initial_clusters.count == K, "You must have k initial clusters.");
    
    if points.count == 0 {
        empty: [K] [] float;
        return empty, .[], 0;
    }

    dimensions := points[0].count;
    membership := NewArray(points.count, [K] float, allocator = allocator);

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
        parallel_for(0, points.count, (a: int, thread: int, using data: struct {points: [] [] float; clusters: [K] [] float; membership: [] [K] float; fuzzification: float;}) {
            point := points[a];
            if a % 1000 == 0 {
                print("On point % of %.\n", a + 1, points.count);
            }

            // this is my attempt to match the wiki algorithm...
            distances: [K] float;
            for cluster, b: clusters {
                distances[b] = distance_function(point, cluster);
            }

            for b: 0..K - 1 {
                sum: float64;
                for c: 0..K - 1 {
                    if distances[c] < 0.000001 {
                        sum = 1.0 / 0.000001;
                        break;
                    }

                    sum += pow(distances[b] / distances[c], 2 / (fuzzification - 1));
                }
                // print("final sum is %\n", sum);
                sum = 1.0 / sum;
                membership[a][b] = cast(float) sum;
            }
        }, .{points, clusters, membership, fuzzification});

        convergence := true;

        // relculate cluster centroids based on weighted point cluster memberships
        memset(intermediary_flat_clusters64.data, 0, K * dimensions * size_of(float64));
        memset(intermediary_flat_clusters32.data, 0, K * dimensions * size_of(float32));
        parallel_for(0, K, (a: int, thread: int, using data: struct {
            points: [] [] float;
            clusters: [K] [] float;
            membership: [] [K] float;
            intermediary_clusters64: [K] [] float64;
            intermediary_clusters32: [K] [] float;
            dimensions: int;
            fuzzification: float;
            convergence: *bool;}) {

            print("On cluster % of %.\n", a + 1, K);
            total_membership_weight: float64;
            for point_membership, point_index: membership {
                membership_weight := pow(point_membership[a], fuzzification);
                total_membership_weight += membership_weight;

                for dimension: 0..dimensions - 1 {
                    intermediary_clusters64[a][dimension] += membership_weight * points[point_index][dimension];
                }
            }

            for dimension: 0..dimensions - 1 {
                intermediary_clusters32[a][dimension] = cast(float) (intermediary_clusters64[a][dimension] / total_membership_weight);
            }

            dist := distance_function(intermediary_clusters32[a], clusters[a]);
            print("Dist between old and new clusters %\n", dist);
            if convergence.* && dist >= 0.00001 {
                convergence.* = false;
            }

            for dimension: 0..dimensions - 1 {
                clusters[a][dimension] = intermediary_clusters32[a][dimension];
            }
        }, .{points, clusters, membership, intermediary_clusters64, intermediary_clusters32, dimensions, fuzzification, *convergence});

        if convergence {
            break;
        }
    }

    return clusters, membership, iterations;
}

classify_points :: (points: [] [] float, clusters: [$K] [] float, fuzzification: float = 2, allocator: Allocator = .{}) -> points: [] [K] float {
    return classify_points(points, clusters, sparse_n_dimensional_distance, fuzzification, allocator);
}

classify_points :: (points: [] [] float, clusters: [$K] [] float, $distance_function: (a: [] float, b: [] float) -> float, fuzzification: float = 2, allocator: Allocator = .{}) -> points: [] [K] float {
    membership := NewArray(points.count, [K] float, allocator = allocator);

    // calculate the membership weights for each point
    parallel_for(0, points.count, (a: int, thread: int, using data: struct {points: [] [] float; clusters: [K] [] float; membership: [] [K] float; fuzzification: float;}) {
        point := points[a];

        // this is my attempt to match the wiki algorithm...
        distances: [K] float;
        for cluster, b: clusters {
            distances[b] = distance_function(point, cluster);
        }

        for b: 0..K - 1 {
            sum: float64;
            for c: 0..K - 1 {
                if distances[c] < 0.000001 {
                    sum = 1.0 / 0.000001;
                    break;
                }

                sum += pow(distances[b] / distances[c], 2 / (fuzzification - 1));
            }
            sum = 1.0 / sum;
            membership[a][b] = cast(float) sum;
        }
    }, .{points, clusters, membership, fuzzification});

    return membership;
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

evenly_distributed_wave_boundaries :: (points: [] [] float, clusters: *[$K] [] float) {
    dimensions := points[0].count;
    dimension_index_half_width := 1.0 / K * dimensions;

    for i: 0..K - 1 {
        k_progress := cast(float) i / K;
        wave_center := k_progress * 2 * PI;

        cluster_dimensions := clusters.*[i];
        for a: 0..dimensions - 1 {
            a_progress := cast(float) a / dimensions;
            a_sign_input := a_progress * PI * 2 * K;
            // distance := a * K;

            cluster_dimensions[a] = cos(a_sign_input - wave_center) * 0.5 + 0.5;
        }
    }
}

random :: (points: [] [] float, clusters: *[$K] [] float) {
    // dimensions := points[0].count;

    for a: 0..K - 1 {
        for b: 0..clusters.*[a].count - 1 {
            clusters.*[a][b] = random_get_zero_to_one();
        }
    }
}

#scope_module

roundi :: (v: float) -> int {
    return cast(int) floor(value + 0.5);
}

#import "Basic";
#import "Math";
#import "Random";
#import "JaiParallel";
