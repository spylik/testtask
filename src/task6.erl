%% --------------------------------------------------------------------------------
%% File:    task6.erl
%% @author  Oleksii Semilietov <spylik@gmail.com>
%%
%% @doc Task6:
%% Implement an Erlang/OTP application that has a POST /capture method which 
%% receives input requests like the attached file Test.xml. If PROD_COVER_GTIN, 
%% PROD_NAME can be captured from a request then the request is accepted, the 
%% values of PROD_COVER_GTIN, PROD_NAME and additional values of PROD_DESC, 
%% BRAND_OWNER_NAME are written to external CSV file with 4 columns:
%% GTIN,NAME,DESC,COMPANY.
%% 
%% NOTE: if PROD_DESC, BRAND_OWNER_NAME are missed in an input request then empty 
%% values will be saved to the CSV.
%% --------------------------------------------------------------------------------

-module(task6).

-define(NOTEST, true).
-ifdef(TEST).
    -compile(export_all).
-endif.

-export([init/2]).

-spec init(Req,Opts) -> Result when
    Req :: cowboy_req:req(),
    Opts :: cowboy_req:body_opts(),
    Result :: {ok, cowboy_req:req(), Opts}.
    
init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
    Req = process_req(Method, HasBody, Req0),
    {ok, Req, Opts}.

process_req(<<"POST">>, true, Req0) -> 
    {ok, XML, Req} = cowboy_req:read_body(Req0),

    FullData = extract(XML),

    case validate(FullData) of
        {Gtin, Name} -> 
            gen_server:cast(task6_fw, {newreq, Gtin, Name, FullData}), 
            cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, <<"accepted">>, Req);
        false -> 
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/plain; charset=utf-8">>}, <<"Wrong data.">>, Req)
    end.

% @doc extract values and names of values from XML (only from BaseAttributeValues in dataObjectId="PACK_BASE_UNIT")
-spec extract(Bin) -> Result when
    Bin :: binary(),
    Result :: list().

extract(Bin) ->
    % we don't need waste resources and parse all xml, so we going to strip what we don't need with BIF
    % (we need only BaseAttributeValues for dataObjectId="PACK_BASE_UNIT")
    BinS = hd(binary:split(hd(tl(binary:split(hd(tl(binary:split(Bin,<<"dataObjectId=\"PACK_BASE_UNIT\"">>))),<<$>>>))),<<"</BaseAttributeValues>">>)),
    AddEnd = unicode:characters_to_binary("</BaseAttributeValues>"),
    BinF = <<BinS/binary,AddEnd/binary>>,

    % since we have only what we exactrly need, we can use sax here.
    % (need test performance, maybe it make sence to replace it with own solution, cuz it looks like quite slow)
    {ok, Values, _Rest} = erlsom:parse_sax(BinF, [], 
        fun (_Event = {startElement,[],"value",[],[{attribute,"value",[],[],Val},
                {attribute,"baseAttrId",[],[],Name},{attribute,"attrType",[],[],"STRING"}]}, Acc) -> 
                    [{unicode:characters_to_binary(Name), unicode:characters_to_binary(Val)}|Acc];
            (_Event, Acc) -> 
                Acc 
        end),
    Values.

% @doc validate if "PROD_COVER_GTIN" and "PROD_NAME" exists in xml
% we returning tuple here {GTIN, NAME} to avoid lists:keyfind for this variables in future. 
-spec validate(Elements) -> Result when
    Elements :: list(),
    Result :: 'false' | {term(), term()}.

validate(Elements) ->
    try 
        begin
            {<<"PROD_COVER_GTIN">>, GTIN} = lists:keyfind(<<"PROD_COVER_GTIN">>,1,Elements),
            {<<"PROD_NAME">>, NAME} = lists:keyfind(<<"PROD_NAME">>,1,Elements),
            {GTIN, NAME}
        end
    catch _:_ -> false
    end.
