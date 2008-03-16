%%% File    : irc_parser.erl
%%% Author  : Geoff Cant <nem@lisp.geek.nz>
%%% Description : 
%%% Created : 27 Mar 2006 by Geoff Cant <nem@lisp.geek.nz>

-module(irc_parser).

-include_lib("irc.hrl").
-include_lib("logging.hrl").
-include_lib("eunit.hrl").

-compile(export_all).

-define(IS_DIGIT(Var), $0 =< Var, Var =< $9).

cut_line(Data) ->
    Pos = string:str(Data, "\r\n"),
    case Pos of
        0 -> {Data, []};
        _ -> {string:substr(Data, 1, Pos-1),
              string:substr(Data, Pos+2, string:len(Data) - (Pos-1))}
    end.

parse_data([]) ->
    [];
parse_data(Data) ->
    {Line, Rest} = cut_line(Data),
    [parse_line(Line)|parse_data(Rest)].

parse_line(Line) ->
    try parse_line(Line, #irc_cmd{raw=Line}) of
        Value -> Value
    catch
        _:_ -> 
            {bad_data, Line}
    end.

parse_line(Line = [$E,$R,$R,$O,$R,$\s|_], Cmd) ->
    parse_command_part(Line, Cmd);
parse_line([A,B,$\s|Rest], Cmd) when ((A == $[) or (A == $]) or
                                      (($0 =< A) and (A =< $9)) or
                                      (($a =< A) and (A =< $z)) or
                                      (($A =< A) and (A =< $Z))),
                                     ((B == $[) or (B == $]) or
                                      (($0 =< B) and (B =< $9)) or
                                      (($a =< B) and (B =< $z)) or
                                      (($A =< B) and (B =< $Z))) ->
    parse_command_part(Rest, Cmd#irc_cmd{source=#p10server{numeric=irc_numerics:p10b64_to_int([A,B])}});
parse_line([A,B,C,D,E,$\s|Rest], Cmd) ->
    case irc_numerics:p10b64_to_int([A,B,C,D,E]) of 
        Num when is_integer(Num) ->
            parse_command_part(Rest, Cmd#irc_cmd{source=#p10user{numeric=Num}});
        {error, Reason} ->
            ?ERR("Need to fix this code.", []),
            erlang:error(Reason)
    end;
parse_line([$:|Rest], Cmd) ->
    parse_prefix_part(Rest, Cmd);
parse_line(Line, Cmd) ->
    parse_command_part(Line, Cmd).

parse_prefix_part(Line, Cmd) ->
    {Prefix, Rest} = split(Line),
    CmdWithUser = parse_prefix(Prefix, Cmd#irc_cmd{source=#user{}}),
    parse_command_part(Rest, CmdWithUser).

parse_prefix(Prefix, #irc_cmd{source=User} = Cmd) ->
    case string:tokens(Prefix, "@") of
        [Nick] -> 
            case lists:member($., Prefix) of
                true ->
                    Cmd#irc_cmd{source=#irc_server{host=Nick}};
                false ->
                    Cmd#irc_cmd{source=User#user{nick=Nick}}
            end;
        [NickSpec, HostSpec] -> 
            case string:tokens(NickSpec, "!") of
                [Nick, [$~|HostUser]] ->
                    Cmd#irc_cmd{source=User#user{nick=Nick, name=HostUser, host=HostSpec}};
                [Nick, HostUser] ->
                    Cmd#irc_cmd{source=User#user{nick=Nick, name=HostUser, host=HostSpec}};
                [_Host] ->
                    Cmd#irc_cmd{source=User#user{nick=NickSpec, host=HostSpec}}
            end
    end.

parse_command_part([D1,D2,D3,$\s|Rest], Cmd) when ?IS_DIGIT(D1),
                                              ?IS_DIGIT(D2),
                                              ?IS_DIGIT(D3) ->
    Cmd#irc_cmd{name=irc_numerics:numeric_to_atom([D1,D2,D3]),
            args=nonl(Rest)};
parse_command_part([D1,D2,D3|Rest], Cmd) when ?IS_DIGIT(D1),
                                              ?IS_DIGIT(D2),
                                              ?IS_DIGIT(D3) ->
    Cmd#irc_cmd{name=irc_numerics:numeric_to_atom([D1,D2,D3]),
                args=nonl(Rest)};
parse_command_part(Rest, Cmd) ->
    case split(Rest) of
        {CommandName, ""} ->
            Cmd#irc_cmd{name=irc_commands:from_list(nonl(CommandName)),
                        args=[]};
        {CommandName, Args} ->
            Cmd#irc_cmd{name=irc_commands:from_list(CommandName),
                        args=nonl(Args)}
    end.

encode_message({pong, Id})      -> io_lib:format("PONG ~s\r\n", [Id]);
encode_message({join, Channel}) -> io_lib:format("JOIN ~s\r\n", [Channel]);
encode_message({quit, Message}) -> io_lib:format("QUIT :~s\r\n", [Message]);
encode_message({say, Channel, Message}) ->
    io_lib:format("PRIVMSG ~s :~s\r\n", [Channel, Message]);
encode_message({action, Channel, Message}) ->
    io_lib:format("PRIVMSG ~s :~sACTION ~s~s\r\n",
                  [Channel, [1],
                   Message, [1]]);
                   
encode_message({nick, Nickname}) -> io_lib:format("NICK ~s\r\n", [Nickname]);
encode_message({pass, Password}) -> io_lib:format("PASS ~s\r\n", [Password]);
encode_message({user, Nickname, HostName, ServerName, RealName}) ->
    io_lib:format("USER ~s ~s ~s :~s~n",
                  [Nickname, HostName, ServerName, RealName]).

split(Line) ->
    split($\s, Line).

split(Char, Line) when is_integer(Char) ->
    split(fun (X) when X == Char -> false; (_) -> true end, Line);
split(Fun, Line) when is_function(Fun) ->
    case lists:splitwith(Fun, Line) of
        {First, Second} when length(Second) > 0 ->            
            {First, tl(Second)};
        {First, []} ->
            {First, []}
    end.
    
split_test() ->
    ?assertMatch({"this", "is a test"}, split("this is a test")).

is_digit(D) when ?IS_DIGIT(D) ->
    true;
is_digit(_) -> false.

%% No newline (nonl)
nonl([$\r,$\n]) -> [];
nonl([$\n]) -> [];
nonl([]) -> [];
nonl([H|T]) -> [H|nonl(T)].

join(_, []) ->
    [];
join(_, [String]) when is_list(String) ->
    String;
join(Sep, Strings) when is_integer(Sep) ->
    join([Sep], Strings);
join(Sep, Strings) when is_list(Sep), length(Strings) > 1 ->
     join(Sep, tl(Strings), [hd(Strings)]).

join(_Sep, [], Acc) ->
    lists:append(lists:reverse(Acc));
join(Sep, [Hd|Tl], Acc) ->
    join(Sep, Tl, [Hd,Sep|Acc]).

join_test() ->
    ?assertMatch("This is a test.",
                 join($\s, ["This", "is", "a", "test."])).

join_2_test() ->
    ?assertMatch("This",
                 join($\s, ["This"])).
