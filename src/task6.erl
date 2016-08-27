-module(task6).
-include("deps/teaser/include/utils.hrl").

-compile(export_all).

process(Bin) -> 
    FullData = extract(Bin),
    case validate(FullData) of
        {Gtin, Name} -> save_csv(Gtin, Name, FullData);
        false -> false
    end.

% @doc extract values and names of values from XML (only from BaseAttributeValues in dataObjectId="PACK_BASE_UNIT")
-spec extract(Bin) -> Result when
    Bin :: binary(),
    Result :: list().

extract(Bin) ->
    % strip what we don't need (we need only BaseAttributeValues for dataObjectId="PACK_BASE_UNIT")
    BinS = hd(tl(binary:split(hd(binary:split(hd(tl(binary:split(hd(tl(binary:split(Bin,<<"dataObjectId=\"PACK_BASE_UNIT\"">>))),<<$>>>))),<<"</BaseAttributeValues>">>)),<<"<BaseAttributeValues>">>))),

    % since we have only what we exactrly need, we can use sax here.
    % (need test performance, maybe it make sence to replace it to own solution)
    {ok, Values, _Rest} = erlsom:parse_sax(BinS, [], 
        fun ({startElement,[],"value",[],[{attribute,"value",[],[],Val},
                {attribute,"baseAttrId",[],[],Name},{attribute,"attrType",[],[],"STRING"}]}, Acc) -> 
                    [{Name, Val}|Acc];
            (_Event, Acc) -> Acc 
        end),
    Values.

% @doc validate if "PROD_COVER_GTIN" and "PROD_NAME" exists in xml
% we returning here tuple {GTIN, NAME} to avoid lists:keyfind for this variables in future. 
-spec validate(Elements) -> Result when
    Elements :: list(),
    Result :: boolean().

validate(Elements) ->
    try 
        begin
            {"PROD_COVER_GTIN", GTIN} = lists:keyfind("PROD_COVER_GTIN",1,Elements),
            {"PROD_NAME", NAME} = lists:keyfind("PROD_NAME",1,Elements),
            {GTIN, NAME}
        end
    catch _:_ -> false
    end.

save_csv(_Gtin, _Name, _FullDate) -> 
    ok.
