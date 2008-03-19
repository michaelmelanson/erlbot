%%%-------------------------------------------------------------------
%%% File    : irc_channel.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-19 by Michael Melanson
%%%-------------------------------------------------------------------
-module(irc_channel).

-behaviour(gen_server).

%% API
-export([start_link/0, message/2, topic/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {messages = [], topic = ""}).

-define(SERVER, ?MODULE).

-define(BACKLOG_SIZE, 15).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

message(Pid, Text) ->
    gen_server:cast(Pid, {message, Text}).
    
topic(Pid) ->
    gen_server:call(Pid, current_topic).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({message, Text}, State) ->
    NewMessages = lists:append(State#state.messages, [Text ++ "\n"]),
    
    if
        length(NewMessages) > ?BACKLOG_SIZE ->
            Trimmed = lists:nthtail(length(NewMessages) - ?BACKLOG_SIZE, NewMessages);
            
        true ->
            Trimmed = NewMessages
    end,
    
    {NewTopic, TopicWeight} = semantic:request(lists:append(Trimmed)),
    
    case {NewTopic =/= State#state.topic, TopicWeight > 0.6} of
        {true, true} ->
            io:format("~p: Topic is now ~s (weight ~p)~n",
                      [?MODULE, NewTopic, TopicWeight]),
            {noreply, State#state{messages=Trimmed, topic=NewTopic}};
            
        {true, false} -> 
            io:format("~p: Not enough confidence~n", [?MODULE]),
            {noreply, State#state{messages=Trimmed}};
            
        {false, _} ->
            io:format("~p: Same topic~n", [?MODULE]),
            {noreply, State#state{messages=Trimmed}};
            
        _ ->
            {noreply, State#state{messages=Trimmed}}
            
    end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
