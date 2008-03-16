%%%-------------------------------------------------------------------
%%% File    : irc_connection.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(irc_connection).

%% API
-export([start/3, send/2, terminate/1]).

-include("irc.hrl").

-record(state, {socket, pid, buffer = []}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: connect(Host, Port)
%% Description:
%%--------------------------------------------------------------------
start(Host, Port, ReportPid) ->
    spawn(fun() ->
        {ok, Sock} = connect(Host, Port),
        ReportPid ! socket_opened, 
        loop(#state{socket=Sock, pid = ReportPid})        
    end).
    
send(Pid, Cmd) ->
    Pid ! {send, Cmd}.
    
terminate(Pid) ->
    Pid ! terminate.
    
%%====================================================================
%% Internal functions
%%====================================================================
connect(Host, Port) ->
    Opts = [binary,
            {nodelay, true},
            {keepalive, true},
            {reuseaddr, true},
            {active, true}],
            
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Sock} ->           
            {ok, Sock};
        {error, _Reason} ->
            timer:sleep(30000),
            connect(Host, Port)
    end.
    
loop(State) ->
    receive
        {send, Msg} -> % See irc_parser:encode_message/1 for format of Msg
            gen_tcp:send(State#state.socket, irc_parser:encode_message(Msg)),
            loop(State);
            
        {tcp, _Port, Data} ->
            RawData = lists:concat([State#state.buffer, binary_to_list(Data)]),
            RawParsed = irc_parser:parse_data(RawData),

            case tl(RawParsed) of
                {bad_data, BadData} ->
                    Parsed = lists:sublist(RawParsed, lists:len(RawParsed)-1),
                    Buffer = BadData;
                
                _ ->
                    Parsed = RawParsed,
                    Buffer = ""
            end,
                        
            lists:foreach(fun(Line) ->
                case Line of
                    {bad_data, _} ->
                        io:format("~p: Warning, ignoring bad line~n", [?MODULE]);
                    
                    Cmd when is_record(Cmd, irc_cmd) ->
                        case Cmd#irc_cmd.name of
                            ping ->
                                io:format("~p: PING? PONG!~n", [?MODULE]),
                                self() ! {send, {pong, Cmd#irc_cmd.args}};
                            _ ->
                                State#state.pid ! {received, Line}
                        end
                end
            end, Parsed),
            loop(State#state{buffer = Buffer});
            
        {tcp_closed, _Port} ->
            io:format("~p: Connection closed~n", [?MODULE]),
            State#state.pid ! socket_closed,
            ok;
            
        terminate ->
            io:format("~p: Terminating~n", [?MODULE]),
            ok;
            
        Other ->
            io:format("~p: Unknown message ~p~n", [?MODULE, Other]),
            loop(State)
    end.