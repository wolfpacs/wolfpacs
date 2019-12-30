-type decoded() :: {ok, binary(), binary()} | {error, binary()}.
-type decoded_list() :: {ok, list(binary()), binary()} | {error, binary()}.

-record(pdv_item, {pr_cid, is_command, is_last, pdv_data}).
