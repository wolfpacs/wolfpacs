-module(wolfpacs_implicit).
-export([vr/2]).

vr(16#0000, 16#0000) -> ul;
vr(16#0000, 16#0002) -> ui;
vr(16#0000, 16#0100) -> us;
vr(16#0000, 16#0110) -> us;
vr(16#0000, 16#0800) -> us;

%% Last catch all
vr(Group, Element) -> unknown.
