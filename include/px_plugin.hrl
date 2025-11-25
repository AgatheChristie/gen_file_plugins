-ifndef(PX_PLUGIN_HRL).
-define(PX_PLUGIN_HRL, true).

-define(LOG_DEBUG(F, A), rebar_api:debug(F, A)).
-define(LOG_INFO(F, A), rebar_api:info(F, A)).
-define(LOG_ERROR(F, A), rebar_api:error(F, A)).

-ifndef(IF).
-define(IF(C, T, F), case (C) of true -> (T); _ -> (F) end).
-define(IF(C, T), ?IF(C, T, skip)).
-endif.

-define(NONE, none).
-define(PLUGIN_NAMESPACE, px).

-endif.
