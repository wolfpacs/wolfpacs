-type decoded() :: {ok, binary(), binary()} | {error, binary()}.
-type decoded_list() :: {ok, list(binary()), binary()} | {error, binary()}.

-record(pdv_item, {pr_cid :: integer(), is_last :: boolean(), is_command :: boolean(), pdv_data :: binary()}).

-type strategy() :: {explicit, little} |
		    {explicit, big}    |
		    {implicit, little} |
		    {implicit, big}.

-type flow() :: pid() | no_flow.

-type ui() :: list() | binary().

-record(wolfpacs_remote, { host = <<"localhost">> :: binary()
			 , port = 1112 :: integer()
			 , ae = <<"ae">> :: binary()
			 }).
