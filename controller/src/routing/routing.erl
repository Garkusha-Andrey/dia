-module(routing).

-export([init/0, update/0,
	%% for test
	 test_update/1]).

-include("../controller_app.hrl").

-record(instanceChunks, {id,
                         weight,
                         chunks=[],
                         chunks_n=0,
                         chunks_diff=0}).

-define(NUM_CHUNKS, 8).
-define(FULL_MASK, "255.255.255.255").

init()->
	case ets:info(table) of
		undefined ->
			ets:new(table, [set, named_table,public]);
		_ ->
			do_nothing
	end,

    ets:insert(table, {instances, []}),
    ets:insert(table, {freeChunks,
		       [{X,0} || X <- lists:seq(0, ?NUM_CHUNKS-1)]}),

    restconf:table_delete(0),

    {IpInt, MaskInt} = controller_lib:get_ovsIp(),
    {IpExt, MaskExt} = controller_lib:get_publicIp(),

    lists:foreach(fun(A) -> restconf:flow_send(A) end,
		  flows:defaults(arp,
				[IpInt,MaskInt,
				 IpExt,MaskExt])),

    lists:foreach(fun(A) -> restconf:flow_send(A) end,
		  flows:defaults(ip,
				[IpInt,IpExt,
				 controller_lib:get_ovsMac(),
				 controller_lib:get_extGwMac()])),

    update().

update()->
    test_update(2).
test_update(_Iteration)->

    %% LOAD
    [{instances, Instances}] = ets:lookup(table, instances),
    [{freeChunks, FreeChunks}] = ets:lookup(table, freeChunks),


    %% Weights = dia_stubs:instance_weights_get(Iteration),
    Weights = controller_lib:instance_weights_get(),

    %% set the new weights
    Instances1 = update_weights(Instances, Weights),
    %% reallocate chunks
    Instances2 = update_chunk_n(Instances1),
    {Instances3, FreeChunks2} = take_chunks(Instances2, FreeChunks),
    %% remove instances with weight 0
    Instances4 = lists:filter(fun(Instance) ->
				      Instance#instanceChunks.weight /= 0
			      end, Instances3),
    io:format("chunks taken; ~w~nFree: ~w~n", [Instances4, FreeChunks2]),
    {Instances5, FreeChunks3} = give_chunks(Instances4, FreeChunks2),
    io:format("chunks given; ~w~nFree: ~w~n", [Instances5, FreeChunks3]),

    %% there may be free chunks only if there are no instances
    %%true == length(Instances5) > 0 xor length(FreeChunks3) > 0,

    %% make and send exception flows
    lists:foreach(fun(Connection) ->
                          restconf:flow_send(
                            exception_for_connection(Connection, Instances5))
                  end, controller_lib:get_connections()),

    %% TODO remove obsolete exceptions

    %% make and send basic flows
    lists:foreach(fun(Instance) ->
                          lists:foreach(fun(Chunk) ->
                                          restconf:flow_send(
                                            flows:make(
                                              basic,
					      element(1,controller_lib:get_publicIp()),
                                              %% TODO3 add support for
                                              %%       arbitrary chunk number
                                              {"0.0.0." ++
					       integer_to_list(
						 element(1,Chunk)),
					       "0.0.0." ++
					       integer_to_list(?NUM_CHUNKS-1)},
					      noport,
                       controller_lib:get_instance_mac(Instance#instanceChunks.id)))
                                  end, Instance#instanceChunks.chunks)
                  end, Instances5),

    %% remove obsolete basic flows (if no instances are alive)
    lists:foreach(fun(FreeChunk) ->
			  restconf:flow_send(flows:make(
					       basic,
					       element(1,controller_lib:get_publicIp()),
					       %% TODO3 add support for
					       %%       arbitrary chunk number
					       {"0.0.0." ++
						integer_to_list(
						  element(1,FreeChunk)),
						"0.0.0." ++
						integer_to_list(?NUM_CHUNKS-1)},
					       noport,
					       drop))
		  end, FreeChunks3),

    %% SAVE
    ets:insert(table, {instances, Instances5}),
    ets:insert(table, {freeChunks, reset_chunks_owner(FreeChunks3)}),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% AUX functions
%%
set_chunks_owner(Chunks, Owner) ->
    [{X,Owner} || {X,_} <- Chunks].
reset_chunks_owner(Chunks) ->
    set_chunks_owner(Chunks, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @func exception_for_connection/2
%% @description
%%     for given @Connection, make an exception flow if needed
%%
exception_for_connection(Connection, Instances) ->
	{_Port, IpAddress} = Connection#servers.portIpAddr,
    {ok, {_,_,_,Lsb}} = inet:parse_ipv4_address(IpAddress),
    Chunk = Lsb band (?NUM_CHUNKS-1),

    %%io:format("Connection ~p Chunk ~p Instances ~p~n", [Connection, Chunk, Instances]),
    FilteredInstances = lists:filter(fun(Instance) ->
					     Instance#instanceChunks.id == Connection#servers.nodeId
				     end, Instances),
    case FilteredInstances of
	[] ->
	    %% no such instance in our list. strange but ok
	    noflow;

	[TheInstance] ->
	    %% normal case, one instance with this ID
	    case lists:filter(fun(InstChunk) ->
				      element(1, InstChunk) == Chunk
			      end, TheInstance#instanceChunks.chunks) of
		[] ->
			{Port, IpAddress} = Connection#servers.portIpAddr,
		    flows:make(exception,
			       element(1,controller_lib:get_publicIp()),
			       {IpAddress, ?FULL_MASK},
			       Port,
			       controller_lib:get_instance_mac(Connection#servers.nodeId));
		_ ->
		    noflow
	    end
         %% several instances with this ID is definitely not ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @func update_weights/2
%% @description
%%     for all the instances in the @InstanceList:
%%      update weight according to @Weights
%%        if instance is not in @Weights, set weight to 0;
%%      calculate the total (sum) weight
%%
update_weights(InstanceList, Weights) ->

    %% reset all weights
    InstanceList2 = lists:map(fun(Instance) ->
		   #instanceChunks{
		     id=Instance#instanceChunks.id,
		     weight=0,
		     chunks=Instance#instanceChunks.chunks,
		     chunks_n=Instance#instanceChunks.chunks_n,
                     chunks_diff=Instance#instanceChunks.chunks_diff}
			   end, InstanceList),

    %%iterate through weights and update instances
    int_update_weights(InstanceList2, Weights, [], 0).

int_update_weights(OldInstanceList, [Weight | RWeights],
                 NewInstanceList, PrevTotalWeight) ->
    CompareFunc = fun(Instance) ->
                Instance#instanceChunks.id == Weight#instanceWeight.dianodeId
                  end,

    TotalWeight = PrevTotalWeight + Weight#instanceWeight.weight,

    case lists:filter(CompareFunc, OldInstanceList) of
        [] ->
	    %% no such ID in the old instance list (new instance booted) - add
	    ExistingInstance = null,
            NewInstance = #instanceChunks{id=Weight#instanceWeight.dianodeId,
                                          weight=Weight#instanceWeight.weight};

        [ExistingInstance] ->
	    %% such ID already in the old instance list, just update weight
            NewInstance = #instanceChunks{
              id=ExistingInstance#instanceChunks.id,
              weight=Weight#instanceWeight.weight,
              chunks=ExistingInstance#instanceChunks.chunks,
              chunks_n=ExistingInstance#instanceChunks.chunks_n,
              chunks_diff=ExistingInstance#instanceChunks.chunks_diff}
    end,

    case ExistingInstance of
	null ->
	    int_update_weights(OldInstanceList, RWeights,
				 [NewInstance|NewInstanceList], TotalWeight);
	_ ->
	    %% the instance that we copied to new instance list,
	    %%  has to be removed from the old instance list
	    int_update_weights(lists:delete(ExistingInstance,
					      OldInstanceList),
				 RWeights,
				 [NewInstance|NewInstanceList], TotalWeight)
    end;

%% now we have iterated through all weights and
%%  at this point, NewInstanceList contains:
%%   - remaining instances (weights updated)
%%   - new instances (booted)
%%  DeletedInstances contains the instances which do not exist anymore
%%   their weights are 0 and we merge them into the resulting instance list
%%   to be able to take chunks from them
%%
%%  normalize weights so they make 1 in total
%%   (unless all instances are deleted and TotalWeight is 0)
int_update_weights(DeletedInstances, [], [], 0) ->
    DeletedInstances;

int_update_weights(DeletedInstances, [], NewInstanceList, TotalWeight) ->
    lists:map(fun(Instance) ->
		      #instanceChunks{
		   id=Instance#instanceChunks.id,
		   weight=Instance#instanceChunks.weight / TotalWeight,
		   chunks=Instance#instanceChunks.chunks,
		   chunks_n=Instance#instanceChunks.chunks_n,
		   chunks_diff=Instance#instanceChunks.chunks_diff}
	      end, DeletedInstances ++ lists:reverse(NewInstanceList)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @func update_chunk_n/1
%% @description
%%     for all the instances in the InstanceList:
%%      compute the chunks share according to the new weight;
%%      update chunks_n, chunks_diff
%%
%%   Share algorithm:
%%     8 chunks to 3 instances with weights (100,100,95)
%%     normalized weights: ( 0,34 0,34 0,32 )
%%     x NUM_CHUNKS:       ( 2,71 2,71 2,58)
%%     basic share:        ( 2 2 2 )
%%     remaining chunks: 2
%%     remaining weight:   ( 0,71 0,71 0,58 )
%%     extra share:  max(remaining weight) until all shared
%%     final share:        ( 3 3 2 )

trunc_h(A) ->
    A - trunc(A).

update_chunk_n(InstanceList) ->
    int_update_chunk_n(InstanceList, 0, []).

%% there are no instances
int_update_chunk_n([], 0, []) ->
    [];

%% basic chunk share function:
%%   instance_weight [0,1] * NUM_CHUNKS, round down to integer
int_update_chunk_n([Instance|OldInstanceList], SharedChunks, NewInstanceList) ->
    Chunks_n = trunc(Instance#instanceChunks.weight * ?NUM_CHUNKS),
    Chunks_diff = Chunks_n - Instance#instanceChunks.chunks_n,
    NewInstance = #instanceChunks{id=Instance#instanceChunks.id,
				      weight=Instance#instanceChunks.weight,
				      chunks=Instance#instanceChunks.chunks,
				      chunks_n=Chunks_n,
				      chunks_diff=Chunks_diff},

    int_update_chunk_n(OldInstanceList, SharedChunks + Chunks_n,
		       [NewInstance|NewInstanceList]);

%% all the chunks were shared by the basic share function
int_update_chunk_n([], ?NUM_CHUNKS, InstanceList) ->
    lists:reverse(InstanceList);

%% there are still chunks to share.
%%   share to instances with the larger "remaining weight"
int_update_chunk_n([], SharedChunks, InstanceList) ->
    SortedInstanceList = lists:sort(fun(A,B) ->
			trunc_h(A#instanceChunks.weight * ?NUM_CHUNKS) >
			trunc_h(B#instanceChunks.weight * ?NUM_CHUNKS)
				    end, InstanceList),
    extra_share(SortedInstanceList, SharedChunks, []).

%% extra_share is done
extra_share(OldInstanceList, ?NUM_CHUNKS, NewInstanceList) ->
    NewInstanceList ++ OldInstanceList;

%% remaining weights are 0 - done
extra_share([Instance= #instanceChunks{}|OldInstanceList],
	    SharedChunks, NewInstanceList)
  when Instance#instanceChunks.weight == 0 ->
    %% no chunks could have been shared
    0 = SharedChunks,
    NewInstanceList ++ [Instance|OldInstanceList];

%% add 1 chunk to instance
extra_share([Instance|OldInstanceList], SharedChunks, NewInstanceList) ->
    NewInstance = #instanceChunks{id=Instance#instanceChunks.id,
			  weight=Instance#instanceChunks.weight,
			  chunks=Instance#instanceChunks.chunks,
			  chunks_n=Instance#instanceChunks.chunks_n+1,
			  chunks_diff=Instance#instanceChunks.chunks_diff + 1},

    extra_share(OldInstanceList, SharedChunks+1, [NewInstance|NewInstanceList]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @func take_chunks/2
%% @description
%%     for all the instances in the InstanceList:
%%      take the excess chunks to the FreeChunks list
%%
take_chunks(InstanceList, FreeChunks) ->
    int_take_chunks(InstanceList, FreeChunks, []).

int_take_chunks([Instance= #instanceChunks{}|OldInstanceList],
		FreeChunks, NewInstanceList)
               when Instance#instanceChunks.chunks_diff < 0 ->

    {Chunks_excess, Chunks} = lists:split(
				-Instance#instanceChunks.chunks_diff,
				Instance#instanceChunks.chunks),

    Chunks_excess2 =
	set_chunks_owner(Chunks_excess, Instance#instanceChunks.id),

    UpdatedInstance = #instanceChunks{id=Instance#instanceChunks.id,
				      weight=Instance#instanceChunks.weight,
				      chunks=Chunks,
				      chunks_n=Instance#instanceChunks.chunks_n,
				      chunks_diff=Instance#instanceChunks.chunks_diff},

   int_take_chunks(OldInstanceList,
		   FreeChunks ++ Chunks_excess2,
		   [UpdatedInstance|NewInstanceList]);

%% instance has no excess chunks
int_take_chunks([Instance|OldInstanceList], FreeChunks, NewInstanceList) ->
    int_take_chunks(OldInstanceList, FreeChunks, [Instance|NewInstanceList]);

int_take_chunks([], FreeChunks, NewInstanceList) ->
    {lists:reverse(NewInstanceList), FreeChunks}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @func give_chunks/2
%% @description
%%     for all the instances in the InstanceList:
%%      take chunks from the FreeChunks list
%%      according to chunks_diff
%%
give_chunks(InstanceList, FreeChunks) ->
    int_give_chunks(InstanceList, FreeChunks, []).

%% instance needs to be given more chunks
int_give_chunks([Instance = #instanceChunks{}|OldInstanceList],
            FreeChunks, NewInstanceList)
            when Instance#instanceChunks.chunks_diff > 0 ->

    {NewChunks, NewFreeChunks} = lists:split(
                                   Instance#instanceChunks.chunks_diff,
                                   FreeChunks),

    FMoveChunk = fun(Chunk) ->
        io:format("give_chunks(): chunk(~w) - instance [~w]-->[~w]~n",
                  [element(1,Chunk), element(2, Chunk),
		   Instance#instanceChunks.id])
		 end,

    lists:foreach(FMoveChunk, NewChunks),

    NewInstance = #instanceChunks{id=Instance#instanceChunks.id,
                    weight=Instance#instanceChunks.weight,
                    chunks=Instance#instanceChunks.chunks ++ NewChunks,
                    chunks_n=Instance#instanceChunks.chunks_n,
                    chunks_diff=0},
    int_give_chunks(OldInstanceList,
		    NewFreeChunks, [NewInstance|NewInstanceList]);

%% instance does not need any more chunks
int_give_chunks([Instance|OldInstanceList], FreeChunks, NewInstanceList) ->
    int_give_chunks(OldInstanceList, FreeChunks, [Instance|NewInstanceList]);

int_give_chunks([], FreeChunks, []) ->
    {[], FreeChunks};

int_give_chunks([], FreeChunks, NewInstanceList) ->
    %% verify there are no free chunks left if we have any instances
    [] = FreeChunks,
    {lists:reverse(NewInstanceList), []}.
