-module(graph_stats).
-export([start/3, partA/2, partB/2]).
-record(partition, {
    partition,
    ids,
    colors,
    edges
}).

start(Input_file_path, Part_a_output_file_path, Part_b_output_file_path) ->
    %% Read the file, parse its lines and store them in the variable `Lists`.
    {ok, Lines} = file:read_file(Input_file_path),
    Lists = string:split(string:trim(binary_to_list(Lines)), "\r\n", all),

    %% Define a function to parse the file data and return a list of partitions.
    ParsePartitions = fun
        %% If there's no data left to parse, return the accumulated partitions.
        Parse([], Acc) ->
            Acc;
        %% Parse the header and the details of a partition from the list of lines.
        Parse([H1, H2, H3, H4 | T], Acc) ->
            [_Prefix, PartitionStr] = string:tokens(H1, " "),
            Paritition = list_to_integer(PartitionStr),
            Ids = [list_to_integer(Id) || Id <- string:tokens(H2, ",")],
            Colors = string:tokens(H3, ","),
            Edges = [
                {list_to_integer(A), list_to_integer(B)}
             || [A, B] <- [string:tokens(Edge, ",") || Edge <- string:tokens(H4, " ")]
            ],
            %% Construct the partition record and continue parsing.
            Partition = #partition{
                partition = Paritition, ids = Ids, colors = Colors, edges = Edges
            },
            Parse(T, [Partition | Acc])
    end,

    %% Parse the partitions from the file data.
    ParsedPartition = ParsePartitions(Lists, []),
    %% Calculate the total number of parsed partitions.
    PartitionsLength = length(ParsedPartition),

    %% Create ActorA processes to calculate color counts for each partition.
    PIDA = self(),
    io:format("Node: ~p~n", [PIDA]),
    Nodes_A = nodes(),
    %io:format("Nodes ~p~n", [Nodes]),
    lists:foreach(
        fun(Index) ->
            Partition = lists:nth(Index, ParsedPartition),
            %io:format("Data: ~p~n", [Partition]),
            TNodeA = lists:nth(Index, Nodes_A),
            %io:format("tNode: ~p~n", [TNode]),

            spawn(TNodeA, graph_stats, partA, [Partition, PIDA])
        end,
        lists:seq(1, PartitionsLength)
    ),
    %% Gather and aggregate FinishedList from ActorA processes.
    getPartAResults(PartitionsLength, #{}, #{}, Part_a_output_file_path),

    %% Create partB processes to calculate id degrees for each partition.
    PIDB = self(),
    Nodes_B = nodes(),
    % Spawning actorB processes and collecting their PIDs
    Pids = lists:map(
        fun(Index) ->
            Partition = lists:nth(Index, ParsedPartition),
            TNodeB = lists:nth(Index, Nodes_B),
            spawn(TNodeB, graph_stats, partB, [Partition, PIDB])
        end,
        lists:seq(1, PartitionsLength)
    ),

    %io:format("hreger:~p~n", [Pids]),

    lists:foreach(
        fun(Index) ->
            Pid = lists:nth(Index, Pids),

            Pid ! {all_actorB_pids, Pids}
        end,
        lists:seq(1, PartitionsLength)
    ),

    %% Gather and aggregate FinishedList from partB processes.
    getPartBResults(PartitionsLength, #{}, #{}, Part_b_output_file_path).

partA(Partitions, FromPid) ->
    %% Extract IDs and edges from the given partition data.
    Ids = Partitions#partition.ids,
    Edges = Partitions#partition.edges,
    %% Create a map to associate each ID with its corresponding color.
    ColorAndId = lists:zip(Ids, Partitions#partition.colors),

    %% Define a function to update a count for a given color in a map.
    NumOfColor = fun
        (Color, CountMap) when Color =/= undefined ->
            maps:update_with(Color, fun(Count) -> Count + 1 end, 1, CountMap);
        (_, CountMap) ->
            CountMap
    end,

    %% Define a function to count colors for given items based on a provided color extraction function.
    ColorCount = fun(Items, ColorFunc) ->
        lists:foldl(
            fun(Item, Acc) ->
                Colors = ColorFunc(Item),
                lists:foldl(
                    fun(Color, AccInner) ->
                        NumOfColor(Color, AccInner)
                    end,
                    Acc,
                    Colors
                )
            end,
            #{},
            Items
        )
    end,

    %% Count colors for IDs and for edges.
    IdCount = ColorCount(Ids, fun(Id) -> [proplists:get_value(Id, ColorAndId)] end),
    DegreeCount = ColorCount(Edges, fun({IdA, IdB}) ->
        [proplists:get_value(IdA, ColorAndId), proplists:get_value(IdB, ColorAndId)]
    end),

    %% Send the counted colors for IDs and edges to the collector process.
    FromPid ! {child_result, {IdCount, DegreeCount}}.
%% Actor responsible for processing id degree information.
partB(Partitions, FromPidB) ->
    %% Call partB with an initial, empty degree accumulator.
    partB(Partitions, FromPidB, #{}).

partB(Partitions, FromPidB, AccDegreeMap) ->
    %% Extract the internal IDs and edges from the given partition data.
    InsideIds = Partitions#partition.ids,
    Edges = Partitions#partition.edges,

    %% Actor message reception loop.
    receive
        %% Received a message containing all partB process IDs.
        {children_pids, PartBPids} ->
            CurrPid = self(),
            %% Send a request for id degree information to all other partB processes.
            lists:foreach(
                fun(OtherActorBPid) ->
                    if
                        OtherActorBPid =/= CurrPid ->
                            OtherActorBPid ! {request_ids, getOutsideIds(Partitions), self()};
                        true ->
                            ok
                    end
                end,
                PartBPids
            ),
            %% Recurse to continue receiving messages.
            partB(Partitions, FromPidB, AccDegreeMap);
        %% Received a request for id degree information from another partB process.
        {request_ids, OutsideIds, RequestingPid} ->
            %% Identify overlapping IDs between the requester's Outside IDs and this actor's internal IDs.
            OverlappingIds = lists:filter(
                fun(Id) -> lists:member(Id, InsideIds) end, OutsideIds
            ),

            %% Calculate id degree for the overlapping IDs.
            OverlappingIdDegrees = lists:foldl(
                fun(Id, Acc) ->
                    Degree = lists:foldl(
                        fun({A, B}, Count) ->
                            case {A, B} of
                                {Id, _} -> Count + 1;
                                {_, Id} -> Count + 1;
                                _ -> Count
                            end
                        end,
                        0,
                        Edges
                    ),
                    maps:put(Id, Degree, Acc)
                end,
                #{},
                OverlappingIds
            ),

            %% Send the computed degrees to the requesting partB process.
            RequestingPid ! {id_degrees, OverlappingIdDegrees},
            partB(Partitions, FromPidB, AccDegreeMap);
        %% Received id degree information from another partB process.
        {id_degrees, Degrees} ->
            %% Combine the received degree map with the accumulator.
            NewAccDegreeMap = merge_degree_maps(AccDegreeMap, Degrees),
            OutsideIds = getOutsideIds(Partitions),
            SizeNewAcc = maps:size(NewAccDegreeMap),
            SizeOutside = length(OutsideIds),

            %% Calculate id degree for edges internal to this actor's partition.
            InsideEdgeDegrees = lists:foldl(
                fun({A, B}, Acc) ->
                    Acc1 =
                        case lists:member(A, InsideIds) of
                            true -> maps:update_with(A, fun(Count) -> Count + 1 end, 1, Acc);
                            false -> Acc
                        end,
                    case lists:member(B, InsideIds) of
                        true -> maps:update_with(B, fun(Count) -> Count + 1 end, 1, Acc1);
                        false -> Acc1
                    end
                end,
                #{},
                Edges
            ),

            %% If the accumulated degree map covers all Outside IDs, process and send the most influential id data.
            if
                SizeNewAcc == SizeOutside ->
                    TotalDegrees = merge_degree_maps(NewAccDegreeMap, InsideEdgeDegrees),
                    HighestDegreesMap = higehst_degree(TotalDegrees),
                    FromPidB !
                        {higehst_degree, HighestDegreesMap, Partitions#partition.partition};
                true ->
                    ok
            end,
            partB(Partitions, FromPidB, NewAccDegreeMap);
        %% Handle any unexpected messages.
        Else ->
            io:format("~p unknown message: ~p~n", [self(), Else])
    end.

%% Process and aggregate FinishedList for partA.
%% When all partA FinishedList are received, process and write them to a file.
getPartAResults(0, AddedIds, AddedEdges, Part_a_output_file_path) ->
    %% Merge ID and Edge maps.
    AggregatedMap = maps:fold(
        fun(Key, Value, Acc) ->
            EdgeCount = maps:get(Key, AddedEdges, 0),
            maps:put(Key, {Value, EdgeCount}, Acc)
        end,
        #{},
        AddedIds
    ),

    %% Write the aggregated map to a file.
    {ok, File} = file:open(Part_a_output_file_path, [write]),
    Data = maps:to_list(AggregatedMap),
    lists:foreach(
        fun({Color, {IdCount, EdgeCount}}) ->
            io:format(File, "~s, ~p, ~p~n", [Color, IdCount, EdgeCount])
        end,
        Data
    ),
    file:close(File);
%% When still waiting for more FinishedList, accumulate the incoming FinishedList.
getPartAResults(N, AddedIds, AddedEdges, Part_a_output_file_path) when N > 0 ->
    receive
        {child_result, {IdMap, EdgeMap}} ->
            %% Aggregate received ID map.
            NewIdAggregated = maps:fold(
                fun(Key, Value, Acc) ->
                    maps:update_with(Key, fun(Count) -> Count + Value end, Value, Acc)
                end,
                AddedIds,
                IdMap
            ),
            %% Aggregate received Edge map.
            NewEdgeAggregated = maps:fold(
                fun(Key, Value, Acc) ->
                    maps:update_with(Key, fun(Count) -> Count + Value end, Value, Acc)
                end,
                AddedEdges,
                EdgeMap
            ),
            getPartAResults(N - 1, NewIdAggregated, NewEdgeAggregated, Part_a_output_file_path)
    end.

%% Process and aggregate FinishedList for partB.
%% When all partB FinishedList are received, process and write them to a file.
getPartBResults(0, PartitionDegreeMap, WholeGraphDegreeMap, File) ->
    %% Convert the aggregated FinishedList into a display format.
    FinishedList = lists:map(
        fun({Paritition, Ids}) ->
            io_lib:format("partition ~p: ~s~n", [
                Paritition, string:join([integer_to_list(Id) || Id <- lists:usort(Ids)], ",")
            ])
        end,
        maps:to_list(PartitionDegreeMap)
    ),
    InfluentialG = higehst_degree(WholeGraphDegreeMap),
    G = io_lib:format("G: ~s~n", [
        string:join([integer_to_list(Id) || Id <- lists:usort(maps:keys(InfluentialG))], ",")
    ]),

    %% Write the FinishedList to a file.
    Content = string:join([Res || Res <- FinishedList], "") ++ G,
    ok = file:write_file(File, Content);
%% When still waiting for more FinishedList, accumulate the incoming FinishedList.
getPartBResults(NumPartitions, PartitionDegreeMap, WholeGraphDegreeMap, File) ->
    receive
        {higehst_degree, HighestDegreesMap, PartitionNumber} ->
            %% Aggregate and combine received FinishedList.
            NewCombinedMap = merge_degree_maps(WholeGraphDegreeMap, HighestDegreesMap),
            NewAggregatedMaps = PartitionDegreeMap#{
                PartitionNumber => maps:keys(HighestDegreesMap)
            },
            getPartBResults(NumPartitions - 1, NewAggregatedMaps, NewCombinedMap, File);
        Else ->
            %% Handle unexpected messages.
            io:format("Unknown message received in B: ~p~n", [Else]),
            getPartBResults(NumPartitions, PartitionDegreeMap, WholeGraphDegreeMap, File)
    end.

%% Function to combine two degree maps.
merge_degree_maps(M1, M2) ->
    maps:fold(
        fun(Key, Val, Acc) ->
            maps:update_with(Key, fun(Existing) -> Existing + Val end, Val, Acc)
        end,
        M1,
        M2
    ).

%% Function to identify Outside IDs for a given partition.
getOutsideIds(Partitions) ->
    InsideIds = Partitions#partition.ids,
    Edges = Partitions#partition.edges,
    AllIdsFromEdges = lists:flatten([[A, B] || {A, B} <- Edges]),
    OutsideIds = lists:subtract(lists:usort(AllIdsFromEdges), InsideIds),
    OutsideIds.

%% Function to identify the ids with the highest degree.
higehst_degree(Map) when is_map(Map), map_size(Map) > 0 ->
    MaxValue = maps:fold(fun(_, V, Max) -> erlang:max(V, Max) end, 0, Map),
    maps:filter(fun(_, Value) -> Value == MaxValue end, Map);
higehst_degree(_) ->
    #{}.
