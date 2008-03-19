%%%-------------------------------------------------------------------
%%% File    : semantic.erl
%%% Author  : Michael Melanson
%%% Description : 
%%%
%%% Created : 2008-03-19 by Michael Melanson
%%%-------------------------------------------------------------------
-module(semantic).

%% API
-export([request/1]).

-include_lib("xmerl.hrl").

-define(TOKEN, "eqqhgu0d").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: request(Data) -> Result
%% Description:
%%--------------------------------------------------------------------
request(Data) ->
    Token = ?TOKEN,
    Url = io_lib:format("http://api.semantichacker.com/sh/api?token=~s&type=text&showLabels=true", [Token]),

    inets:start(),
    {ok, Result} = http:request(post, {Url, [], "text/text", Data}, [], []),
    process(Result).

process(Result) ->
    {_Protocol, _Headers, Contents} = Result,
    {Xml, _Rest} = xmerl_scan:string(Contents),
    case {xmerl_xpath:string("/response/signature/dimension[1]/@label", Xml),
          xmerl_xpath:string("/response/signature/dimension[1]/@weight", Xml)} of
        {[LabelAttr], [WeightAttr]} ->
            {Weight, _} = string:to_float(WeightAttr#xmlAttribute.value),
            {LabelAttr#xmlAttribute.value, 
             Weight};
             
        _ -> ""
    end.

%%====================================================================
%% Internal functions
%%====================================================================
