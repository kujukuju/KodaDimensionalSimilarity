
use_serializer :: true;
K :: 16;

main :: () {
    requested_game_id: int;
    args := get_command_line_arguments();
    if args.count == 2 {
        requested_game_id = parse_int(*args[1], int);
        print("Going to return games similar to %.\n", requested_game_id);
    }

    entire_file, entire_file_success := read_entire_file("data.txt");
    assert(entire_file_success);

    print("Done reading data file.\n");

    data: [..] [3] int;
    array_reserve(*data, 65536);

    next_line_index: int;
    while next_line_index < entire_file.count {
        current_line: string;
        current_line.data = entire_file.data + next_line_index;
        current_line.count = entire_file.count - next_line_index;

        for i: 0..current_line.count - 1 {
            if current_line[i] == #char "\n" {
                current_line.count = i;
                next_line_index += i + 1;
                break;
            }
        }

        steam_id_string: string;
        steam_id_string.data = current_line.data;
        steam_id_string.count = current_line.count;
        for i: 0..steam_id_string.count - 1 {
            if steam_id_string[i] == #char " " {
                steam_id_string.count = i;
                break;
            }
        }

        game_id_string: string;
        game_id_string.data = steam_id_string.data + steam_id_string.count + 1;
        game_id_string.count = current_line.count - steam_id_string.count - 1;
        for i: 0..game_id_string.count - 1 {
            if game_id_string[i] == #char " " {
                game_id_string.count = i;
                break;
            }
        }

        playtime_string: string;
        playtime_string.data = game_id_string.data + game_id_string.count + 1;
        playtime_string.count = current_line.count - steam_id_string.count - game_id_string.count - 2;

        // print("Prasing % = % |%| |%| |%| |%|\n", next_line_index, entire_file.count, current_line, steam_id_string, game_id_string, playtime_string);

        steam_id, steam_id_success := parse_int(*steam_id_string, int);
        game_id, game_id_success := parse_int(*game_id_string, int);
        playtime, playtime_success := parse_int(*playtime_string, int);

        assert(steam_id_success, "Could not parse steam_id.");
        assert(game_id_success, "Could not parse game_id.");
        assert(playtime_success, "Could not parse playtime.");

        array_add(*data, .[steam_id, game_id, playtime]);
    }

    print("Done parsing all data.\n");

    players: Table(int, int);
    players_id_lookup: Table(int, int);
    player_count: int;
    games: Table(int, int);
    game_id_lookup: Table(int, int);
    game_count: int;
    for entry: data {
        if !table_contains(*players, entry[0]) {
            table_set(*players, entry[0], player_count);
            table_set(*players_id_lookup, player_count, entry[0]);
            player_count += 1;
        }
        if !table_contains(*games, entry[1]) {
            table_set(*games, entry[1], game_count);
            table_set(*game_id_lookup, game_count, entry[1]);
            game_count += 1;
        }
    }

    print("There are % players.\n", player_count);
    print("There are % games.\n", game_count);

    // player_highest_score: Table(int, float);
    // for entry: data {
    //     if !table_contains(*player_highest_score, entry[0]) {
    //         table_set(*player_highest_score, entry[0], cast(float) entry[2]);
    //     } else {
    //         highest_score := table_find_pointer(*player_highest_score, entry[0]).*;
    //         if cast(float) entry[2] > highest_score {
    //             table_set(*player_highest_score, entry[0], cast(float) entry[2]);
    //         }
    //     }
    // }

    print("Done calculating all players highest scores.\n");

    game_scores_raw := NewArray(player_count * game_count, float);
    game_scores := NewArray(game_count, [] float);
    for *game_score, index: game_scores {
        game_score.data = game_scores_raw.data + player_count * index;
        game_score.count = player_count;
    }

    for entry: data {
        player_index := table_find_pointer(*players, entry[0]).*;
        game_index := table_find_pointer(*games, entry[1]).*;

        // highest_score := table_find_pointer(*player_highest_score, entry[0]).*;
        // if highest_score == 0 {
        //     print("Skipping player who has no playtime at all.\n");
        //     continue;
        // }

        game_scores[game_index][player_index] = sqrt(cast(float) entry[2]);
    }

    print("Done organizing all game scores with players dimensions.\n");

    player_scores_raw := NewArray(player_count * game_count, float);
    player_scores := NewArray(player_count, [] float);
    for *player_score, index: player_scores {
        player_score.data = player_scores_raw.data + game_count * index;
        player_score.count = game_count;
    }

    for entry: data {
        player_index := table_find_pointer(*players, entry[0]).*;
        game_index := table_find_pointer(*games, entry[1]).*;

        // highest_score := table_find_pointer(*player_highest_score, entry[0]).*;
        // if highest_score == 0 {
        //     print("Skipping player who has no playtime at all.\n");
        //     continue;
        // }

        player_scores[player_index][game_index] = sqrt(cast(float) entry[2]);
    }

    print("Done organizing all player scores with games dimensions.\n");

    player_similarities: [] [] float;
    
    if use_serializer && file_exists("player_similarities.txt") {
        #if use_serializer {
            #import,file "../../JaiSerializer/module.jai";

            serialized_string := read_entire_file("player_similarities.txt");
            serialized_bytes: [] u8;
            serialized_bytes.data = serialized_string.data;
            serialized_bytes.count = serialized_string.count;
            serializer_read(serialized_bytes, *player_similarities);

            print("Parsed player similarities from serialized data.\n");
        }
    } else {
        player_similarities = point_distances(player_scores, distance_function = cosine_distance);

        #if use_serializer {
            #import,file "../../JaiSerializer/module.jai";

            bytes: [..] u8;
            serializer_write(*bytes, player_similarities);
            bytes_string: string;
            bytes_string.data = bytes.data;
            bytes_string.count = bytes.count;
            wrote_player_similarities := write_entire_file("player_similarities.txt", bytes_string);

            assert(wrote_player_similarities, "Could not write player similarities data.\n");

            print("Wrote player similarities to player_similarities.txt.\n");
        }
    }

    // test code to normalize all games
    print("Starting game playtime normalization.\n");
    game_playtimes := NewArray(game_count, float);
    highest_playtime := 0.0;
    for scores, player_index: player_scores {
        for score, game_index: scores {
            game_playtimes[game_index] += score;
            highest_playtime = max(highest_playtime, game_playtimes[game_index]);
        }
    }
    print("Done with playtime normalization.\n");

    highest_playtime_per_player := 0.0;
    game_playtimes_per_player := NewArray(game_count, float);
    for game_index: 0..game_count - 1 {
        played_count := 0;

        for player_index: 0..player_count - 1 {
            if player_scores[player_index][game_index] > 0 {
                played_count += 1;
            }
        }

        game_playtimes_per_player[game_index] = game_playtimes[game_index] / played_count;
        highest_playtime_per_player = max(highest_playtime_per_player, game_playtimes_per_player[game_index]);
    }

    recommendations: [] [] float;
    
    if use_serializer && file_exists("recommendations.txt") {
        #if use_serializer {
            #import,file "../../JaiSerializer/module.jai";

            serialized_string := read_entire_file("recommendations.txt");
            serialized_bytes: [] u8;
            serialized_bytes.data = serialized_string.data;
            serialized_bytes.count = serialized_string.count;
            serializer_read(serialized_bytes, *recommendations);

            print("Parsed recommendations from serialized data.\n");
        }
    } else {
        recommendations = point_recommendations(player_similarities, player_scores, 1000);

        #if use_serializer {
            #import,file "../../JaiSerializer/module.jai";

            bytes: [..] u8;
            serializer_write(*bytes, recommendations);
            bytes_string: string;
            bytes_string.data = bytes.data;
            bytes_string.count = bytes.count;
            wrote_recommendations := write_entire_file("recommendations.txt", bytes_string);

            assert(wrote_recommendations, "Could not write recommendation data.\n");

            print("Wrote recommendations to recommendations.txt.\n");
        }
    }

    games_ranked_by_playtime := array_copy(game_playtimes);
    quick_sort(games_ranked_by_playtime, (a: float, b: float) -> float {
        return a - b;
    });
    games_playtime_to_rank: Table(float, float);
    for score, index: games_ranked_by_playtime {
        table_set(*games_playtime_to_rank, score, cast(float) (index + 1) / games_ranked_by_playtime.count);
    }
    game_ranks := NewArray(game_count, float);
    for game_playtime, game_index: game_playtimes {
        game_ranks[game_index] = table_find_pointer(*games_playtime_to_rank, game_playtime).*;
    }

    requested_player_id := requested_game_id;
    if requested_player_id {
        player_index := table_find_pointer(*players, requested_player_id).*;

        highest_relative_value: float;
        highest_scored_value: float;
        for i: 0..recommendations[player_index].count - 1 {
            game_playtime := game_playtimes[i];
            highest_relative_value = max(highest_relative_value, recommendations[player_index][i] / game_playtime);
            highest_scored_value = max(highest_scored_value, recommendations[player_index][i] / highest_playtime);
        }

        best_game_rating: float;
        best_game_index: int = -1;
        best_game_rating_maximum: float;
        for i: 0..19 {
            for score, game_index: recommendations[player_index] {
                game_playtime := game_playtimes[game_index];

                // normalized_score := score / game_playtime / highest_relative_value + game_playtime / highest_playtime;
                // normalized_score := score / game_playtime / highest_relative_value + game_playtimes_per_player[game_index] / highest_playtime_per_player + game_ranks[game_index] + score / highest_playtime / highest_scored_value * 2;
                // normalized_score := (score / game_playtime / highest_relative_value + 1.5) * (game_playtimes_per_player[game_index] / highest_playtime_per_player * 0.25 + game_ranks[game_index] * 0.5 + score / highest_playtime / highest_scored_value *4);

                // this pow is the popularity rating kind of
                normalized_score := pow(min(game_ranks[game_index], 0.8), 4) * (game_playtimes_per_player[game_index] / highest_playtime_per_player + 2) * (pow(score, 1.2) / game_playtime / highest_relative_value);

                // normalized_score := min(game_ranks[game_index], 0.8) * game_playtimes_per_player[game_index] / highest_playtime_per_player * pow(score, 1.5) / game_playtime / highest_relative_value;
                // normalized_score := game_ranks[game_index] * score * (score / game_playtime / highest_relative_value);
                // normalized_score := score * (score / game_playtime / highest_relative_value);
                // normalized_score := score;

                if best_game_rating_maximum > 0 && normalized_score >= best_game_rating_maximum {
                    continue;
                }

                if player_scores[player_index][game_index] > 0 {
                    continue;
                }

                if normalized_score > best_game_rating {
                    best_game_rating = normalized_score;
                    best_game_index = game_index;
                }
            }

            game_playtime := game_playtimes[best_game_index];
            game_index := best_game_index;
            score := recommendations[player_index][best_game_index];
            print("Scores for % are %, %, %, %.\n", i + 1, score / game_playtime / highest_relative_value, game_playtimes_per_player[game_index] / highest_playtime_per_player * 0.25, game_ranks[game_index] * 0.5, score / highest_playtime / highest_scored_value * 4);

            best_game_id := table_find_pointer(*game_id_lookup, best_game_index).*;
            print("Game %. % with a score of %. https://steamdb.info/app/%/\n", i + 1, best_game_id, best_game_rating, best_game_id);

            best_game_index = -1;
            best_game_rating_maximum = best_game_rating;
            best_game_rating = 0;
        }
    }

    if requested_player_id {
        player_index := table_find_pointer(*players, requested_player_id).*;

        best_match_rating: float;
        best_match_index: int = -1;
        best_match_rating_minimum: float;
        for i: 0..9 {
            for index: 0..player_scores.count - 1 {
                similarity := player_similarities[player_index][index];
                if similarity < 0.0000001 {
                    continue;
                }
                if best_match_rating_minimum > 0 && similarity <= best_match_rating_minimum {
                    continue;
                }

                if best_match_rating == 0 || similarity < best_match_rating {
                    best_match_rating = similarity;
                    best_match_index = index;
                }
            }

            best_match_id := table_find_pointer(*players_id_lookup, best_match_index).*;
            print("Player %. % with a score of %. https://steamcommunity.com/profiles/%/games/?sort=playtime&tab=all\n", i + 1, best_match_id, best_match_rating, best_match_id);

            best_match_index = -1;
            best_match_rating_minimum = best_match_rating;
            best_match_rating = 0;
        }

        // for i: 0..player_similarities[player_index].count - 1 {
        //     if player_similarities[player_index][i] > best_match_rating {
        //         best_match_rating = player_similarities[player_index][i];
        //         best_match_index = i;
        //     }
        // }

        // print("Best match player is % with a score of %.\n", table_find_pointer(*players_id_lookup, best_match_index).*, best_match_rating);
    }

    // clusters: [K] [] float;
    // classifications: [] [K] float;

    // if use_serializer && file_exists("clusters.txt") {
    //     #if use_serializer {
    //         #import,file "../../JaiSerializer/module.jai";

    //         serialized_string := read_entire_file("clusters.txt");
    //         serialized_bytes: [] u8;
    //         serialized_bytes.data = serialized_string.data;
    //         serialized_bytes.count = serialized_string.count;
    //         serializer_read(serialized_bytes, *clusters);

    //         print("Parsed clusters from serialized data.\n");
    //     }
    // } else {
    //     initial_clusters_raw := NewArray(K * player_count, float);
    //     initial_clusters: [16] [] float;
    //     for *initial_cluster, index: initial_clusters {
    //         initial_cluster.data = initial_clusters_raw.data + player_count * index;
    //         initial_cluster.count = player_count;
    //     }

    //     // evenly_distributed_maximum_boundaries(game_scores, *initial_clusters);
    //     // evenly_distributed_wave_boundaries(game_scores, *initial_clusters);
    //     random(game_scores, *initial_clusters);
    //     // builder: String_Builder;
    //     // for cluster: initial_clusters {
    //     //     for value: cluster {
    //     //         append(*builder, tprint("% ", value));
    //     //     }
    //     //     append(*builder, "\n");
    //     // }
    //     // write_entire_file("initial_clusters.txt", builder_to_string(*builder));

    //     iterations: int;
    //     clusters, classifications, iterations = fuzzy_means(game_scores, K, max_iterations = 40, initial_clusters = initial_clusters, fuzzification = 8);

    //     print("Completed fuzzy means in % iterations.\n", iterations);

    //     #if use_serializer {
    //         #import,file "../../JaiSerializer/module.jai";

    //         bytes: [..] u8;
    //         serializer_write(*bytes, clusters);
    //         bytes_string: string;
    //         bytes_string.data = bytes.data;
    //         bytes_string.count = bytes.count;
    //         wrote_clusters := write_entire_file("clusters.txt", bytes_string);

    //         assert(wrote_clusters, "Could not write cluster data. Fuck this takes a long time to run.\n");

    //         print("Wrote clusters to clusters.txt.\n");
    //     }
    // }

    // if use_serializer && file_exists("classifications.txt") {
    //     #if use_serializer {
    //         #import,file "../../JaiSerializer/module.jai";

    //         serialized_string := read_entire_file("classifications.txt");
    //         serialized_bytes: [] u8;
    //         serialized_bytes.data = serialized_string.data;
    //         serialized_bytes.count = serialized_string.count;
    //         serializer_read(serialized_bytes, *classifications);

    //         print("Parsed classifications from serialized data.\n");
    //     }
    // } else {
    //     classifications = classify_points(game_scores, clusters, fuzzification = 8);

    //     print("Completed game classifications.\n");

    //     #if use_serializer {
    //         #import,file "../../JaiSerializer/module.jai";
    //         bytes: [..] u8;
    //         serializer_write(*bytes, classifications);
    //         bytes_string: string;
    //         bytes_string.data = bytes.data;
    //         bytes_string.count = bytes.count;
    //         wrote_classifications := write_entire_file("classifications.txt", bytes_string);

    //         assert(wrote_classifications, "Could not write classifications data.\n");

    //         print("Wrote classifications to classifications.txt.\n");
    //     }
    // }

    // literal game similarities
    // works
    // if requested_game_id {
    //     requested_game_clusters: [] float;
    //     game_index_pointer := table_find_pointer(*games, requested_game_id);
    //     assert(!!game_index_pointer, "Requested game doesn't exist.");
    //     game_index := game_index_pointer.*;

    //     requested_scores := game_scores[game_index];

    //     best_game_rating: float;
    //     best_game_index: int = -1;
    //     best_game_rating_minimum: float;
    //     for i: 0..9 {
    //         for scores, index: game_scores {
    //             similarity := sparse_n_dimensional_distance(requested_scores, scores);
    //             if similarity < 0.0000001 {
    //                 continue;
    //             }
    //             if best_game_rating_minimum > 0 && similarity <= best_game_rating_minimum {
    //                 continue;
    //             }

    //             if best_game_rating == 0 || similarity < best_game_rating {
    //                 best_game_rating = similarity;
    //                 best_game_index = index;
    //             }
    //         }

    //         best_game_id := table_find_pointer(*game_id_lookup, best_game_index).*;
    //         print("Game %. % with a score of %.\n", i + 1, best_game_id, best_game_rating);

    //         best_game_index = -1;
    //         best_game_rating_minimum = best_game_rating;
    //         best_game_rating = 0;
    //     }
    // }

    // game classification similarities
    // doesnt work
    // if requested_game_id {
    //     requested_game_clusters: [] float;
    //     game_index_pointer := table_find_pointer(*games, requested_game_id);
    //     assert(!!game_index_pointer, "Requested game doesn't exist.");
    //     game_index := game_index_pointer.*;

    //     requested_classifications := classifications[game_index];

    //     best_game_rating: float;
    //     best_game_index: int = -1;
    //     best_game_rating_maximum: float;
    //     for i: 0..9 {
    //         for classification, index: classifications {
    //             similarity := calculate_similarity(requested_classifications, classification);
    //             // similarity := cosine_similarity(requested_classifications, classification);
    //             if best_game_rating_maximum > 0 && similarity >= best_game_rating_maximum {
    //                 continue;
    //             }

    //             if similarity > best_game_rating {
    //                 best_game_rating = similarity;
    //                 best_game_index = index;
    //             }
    //         }

    //         best_game_id := table_find_pointer(*game_id_lookup, best_game_index).*;
    //         print("Game %. % with a score of %.\n", i + 1, best_game_id, best_game_rating);

    //         best_game_index = -1;
    //         best_game_rating_maximum = best_game_rating;
    //         best_game_rating = 0;
    //     }
    // }
}

// does this really check for similarity?
calculate_similarity :: (a: [$K] float, b: [K] float) -> float {
    sum: float64;
    for i: 0..K - 1 {
        // print("% = %\n", a[i], b[i]);
        if abs(a[i] - b[i]) < 0.000001 {
            sum += 1.0 / (0.000001 * 0.000001);
        } else {
            sum += 1.0 / ((a[i] - b[i]) * (a[i] - b[i]));
        }
    }

    return cast(float) sqrt(sum);
}

// sparse_n_dimensional_distance :: (a: [] float, b: [] float) -> float {
//     assert(a.count == b.count, "You cannot compare different numbers of dimensions.");

//     // TODO parallel

//     total: float64;
//     count: int;

//     valid: int;
//     for i: 0..a.count - 1 {
//         if a[i] != 0 && b[i] != 0 {
//             valid += 1;
//         }
//     }

//     for i: 0..a.count - 1 {
//         if a[i] != 0 && b[i] != 0 {
//             // sum { (b - a)^2 / count }
//             delta: float64 = (b[i] - a[i]);
//             delta *= delta;
//             total += delta / valid;
//             // total += delta;
//         }
//     }

//     return cast(float) sqrt(total);
// }

#import,file "../module.jai";

#import "Basic";
#import "File";
#import "File_Utilities";
#import "String";
#import "Hash_Table";
#import "Random";
#import "Math";
#import "Sort";
