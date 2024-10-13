
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

    return cast(float) (dot / (sqrt(a_mag) * sqrt(b_mag)));
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

    return cast(float) sqrt(length_squared);
}

point_distances :: (points: [] [] float, allocator: Allocator = .{}) -> [] [] float {
    return point_distances(points, cosine_distance, allocator);
}

point_distances :: (points: [] [] float, $distance_function: (a: [] float, b: [] float) -> float, allocator: Allocator = .{}) -> [] [] float {
    similarities := NewArray(points.count, [] float,, allocator = allocator);
    for i: 0..similarities.count - 1 {
        similarities[i] = NewArray(points.count, float,, allocator = allocator);
    }

    parallel_for(0, points.count, (i: int, thread: int, using data: struct {points: [] [] float; similarities: [] [] float;}) {
        // I could only fill upper triangular matrix?
        for a: 0..i {
            distance := distance_function(points[i], points[a]);
            similarities[i][a] = distance;
            if i != a {
                similarities[a][i] = distance;
            }
        }
    }, .{points, similarities});

    return similarities;
}

// point_distances: [number of points] [number of points] float
// categories: [number of points] [number of categories] bool (whether or not this point is in this category)
// weights: [number of points] [number of categories] float (weight of this category)
category_weights :: (point_distances: [] [] float, categories: [] [] bool, allocator: Allocator = .{}) -> weights: [] [] float {
    assert(point_distances.count > 0, "Cannot have an empty list of points.");
    category_count := categories[0].count;

    weights := NewArray(point_distances.count, [] float,, allocator = allocator);
    for i: 0..weights.count - 1 {
        weights[i] = NewArray(category_count, float,, allocator = allocator);
    }

    category_sums := NewArray(get_thread_count(), [] float64,, allocator = temp);
    for i: 0..category_sums.count - 1 {
        category_sums[i] = NewArray(category_count, float64,, allocator = temp);
    }
    category_divisors := NewArray(get_thread_count(), [] int,, allocator = temp);
    for i: 0..category_divisors.count - 1 {
        category_divisors[i] = NewArray(category_count, int,, allocator = temp);
    }

    parallel_for(0, point_distances.count, (i: int, thread: int, using data: struct {
            point_distances: [] [] float;
            categories: [] [] bool;
            weights: [] [] float;
            category_count: int;
            category_sums: [] [] float64;
            category_divisors: [] [] int;}) {

        category_sum := category_sums[thread];
        memset(category_sum.data, 0, category_sum.count * size_of(float64));

        category_divisor := category_divisors[thread];
        memset(category_divisor.data, 0, category_divisor.count * size_of(int));

        for point_distance, point_index: point_distances[i] {
            for valid_category, category_index: categories[point_index] {
                point_similarity := 1.0 / (point_distance + 1.0);
                category_value := ifx valid_category then 1 else 0;
                category_sum[category_index] += category_value * point_similarity;
                category_divisor[category_index] += category_value;
            }
        }

        for a: 0..category_count - 1 {
            if category_divisor[a] == 0 {
                weights[i][a] = 0;
            } else {
                weights[i][a] = cast(float) (category_sum[a] / category_divisor[a]);
            }
        }
    }, .{point_distances, categories, weights, category_count, category_sums, category_divisors});

    return weights;
}

// point_distances: [number of points] [number of points] float
// points: [number of points] [number of values] float
// K: the number of similar points to consider
// recommendations: [number of points] [number of values] float
point_recommendations :: (point_distances: [] [] float, points: [] [] float, $K: int = 0, allocator: Allocator = .{}) -> recommendations: [] [] float {
    assert(points.count > 0, "Cannot have an empty points list.");
    value_count := points[0].count;
    
    recommendations := NewArray(points.count, [] float,, allocator = allocator);
    for i: 0..recommendations.count - 1 {
        recommendations[i] = NewArray(value_count, float,, allocator = allocator);
    }

    fuzzification :: 2;

    #if K == 0 {
        parallel_for(0, points.count, (i: int, thread: int, using data: struct {point_distances: [] [] float; points: [] [] float; recommendations: [] [] float; value_count: int;}) {
            for value_index: 0..value_count - 1 {
                total_membership: float;
                for point_index: 0..points.count - 1 {
                    distance := point_distances[i][point_index];
                    if distance < 0.000001 {
                        // it's either an identical point or the points are entirely 0 or something so don't include it
                        continue;
                    }

                    // this is raised to the power of 8 because I want to basically ignore dissimilar players
                    membership := 1.0 / distance;
                    membership *= membership * membership * membership * membership * membership * membership * membership;
                    // this is square rooted because I don't want just flat high playtime games to outrank everything
                    // maybe theres a better way to solve the high playtime problem
                    recommendations[i][value_index] += membership * points[point_index][value_index];
                    total_membership += membership;
                }

                if total_membership >= 0.000001 {
                    recommendations[i][value_index] /= total_membership;
                } else {
                    recommendations[i][value_index] = 0;
                }
            }
        }, .{point_distances, points, recommendations, value_count});
    } else {
        quick_select_memory_raw := NewArray(get_thread_count() * points.count, float,, allocator = temp);
        quick_select_memory := NewArray(get_thread_count(), [] float,, allocator = temp);
        for i: 0..quick_select_memory.count - 1 {
            quick_select_memory[i].data = quick_select_memory_raw.data + i * points.count;
            quick_select_memory[i].count = points.count;
        }

        parallel_for(0, points.count, (i: int, thread: int, using data: struct {point_distances: [] [] float; points: [] [] float; recommendations: [] [] float; quick_select_memory: [] [] float; value_count: int;}) {
            maximum_distance_consideration: float;
            if K >= point_distances[i].count {
                for distance: point_distances[i] {
                    maximum_distance_consideration = max(maximum_distance_consideration, distance);
                }
            } else {
                maximum_distance_consideration = quick_select(point_distances[i], K, *quick_select_memory[thread]);
            }

            // TODO this could be sped up quite a lot
            // because quick_select semi-sorts a partition of the original array
            // if I map this data to the original index values, I could use the partitioned memory section
            // to reduce the amount of iterations by 90+%
                
            for value_index: 0..value_count - 1 {
                total_membership: float;
                for point_index: 0..points.count - 1 {
                    distance := point_distances[i][point_index];
                    if distance < 0.000001 || distance > maximum_distance_consideration {
                        // it's either an identical point or the points are entirely 0 or something so don't include it
                        continue;
                    }

                    // this is raised to the power of 8 because I want to basically ignore dissimilar players
                    membership := 1.0 / distance;
                    membership *= membership * membership * membership * membership * membership * membership * membership;
                    // this is square rooted because I don't want just flat high playtime games to outrank everything
                    // maybe theres a better way to solve the high playtime problem
                    recommendations[i][value_index] += membership * points[point_index][value_index];
                    total_membership += membership;
                }

                if total_membership >= 0.000001 {
                    recommendations[i][value_index] /= total_membership;
                } else {
                    recommendations[i][value_index] = 0;
                }
            }
        }, .{point_distances, points, recommendations, quick_select_memory, value_count});
    }

    return recommendations;
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
    membership := NewArray(points.count, [K] float,, allocator = allocator);

    intermediary_flat_clusters64 := NewArray(K * dimensions, float64,, allocator = temp);
    intermediary_clusters64: [K] [] float64;
    for i: 0..K - 1 {
        intermediary_clusters64[i].data = intermediary_flat_clusters64.data + i * dimensions;
        intermediary_clusters64[i].count = dimensions;
    }

    intermediary_flat_clusters32 := NewArray(K * dimensions, float32,, allocator = temp);
    intermediary_clusters32: [K] [] float32;
    for i: 0..K - 1 {
        intermediary_clusters32[i].data = intermediary_flat_clusters32.data + i * dimensions;
        intermediary_clusters32[i].count = dimensions;
    }
    
    flat_clusters := NewArray(K * dimensions, float,, allocator = allocator);
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
    membership := NewArray(points.count, [K] float,, allocator = allocator);

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

// selects the smallest_index value from the semi-sorted array efficiently
quick_select :: (values: [] float, smallest_index: int, memory: *[] float = null) -> float {
    if !memory {
        memory.* = NewArray(values.count, float,, allocator = temp);
    }
    memcpy(memory.data, values.data, values.count * size_of(float));

    return quick_select(values, smallest_index, 0, values.count - 1, memory);
}

quick_select :: (values: [] float, smallest_index: int, left: int, right: int, memory: *[] float) -> float {
    partition :: (memory: [] float, left: int, right: int) -> int {
        x := memory[right];
        i := left;
        for j: left..right - 1 {
            if memory[j] <= x {
                memory[i], memory[j] = memory[j], memory[i];
                i += 1;
            }
        }
        memory[i], memory[right] = memory[right], memory[i];

        return i;
    }

    if left == 0 && right == 0 {
        return 0;
    }

    if values.count == 0 {
        return 0;
    }

    while true {
        index := partition(memory.*, left, right);

        if index - left == smallest_index - 1 {
            return memory.*[index];
        }

        if index - left > smallest_index - 1 {
            right = index - 1;
            continue;
        }

        smallest_index = smallest_index - index + left - 1;
        left = index + 1;
    }

    return 0;
}

#scope_module

roundi :: (v: float) -> int {
    return cast(int) floor(value + 0.5);
}

#import "Basic";
#import "Math";
#import "Random";
#import "JaiParallel";
