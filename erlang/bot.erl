#!/usr/bin/env escript
%%! -kernel error_logger false

-record(planet, {id, x, y, production, owner, population}).
-record(world, {myid, planets = []}).

-mode(compile).

readInput(#world{planets = P} = W) ->
	Tokens = string:tokens(io:get_line(""), " "),
	case Tokens of
		["\n"] -> readInput(W);
		[".\n"] -> W;
		["P" | T] -> 
			[Id, X, Y, Prod, Owner, Population] 
				= lists:map(fun(X) -> {R, _} = string:to_integer(X), R end, T),
			NewPlanet = #planet{id = Id, x = X, y = Y, 
								production = Prod, owner = Owner, population = Population},
			readInput(W#world{planets = [NewPlanet | P]});
		["Y", Id] -> {MId, _} = string:to_integer(Id), readInput(W#world{myid = MId})
	end.

upgrade([], _) -> ok;
upgrade([#planet{id = PId, owner = Id, production = P, population = Pop} | T], Id) ->
	Pow = math:pow(2, P),
	if Pow < Pop -> io:format("B ~p\n",[PId]);
		true -> ok
	end,
	upgrade(T, Id);
upgrade([_ | T], Id) -> upgrade(T, Id).

takeTwo([], OId) -> {undefined, OId, []};
takeTwo([#planet{id = Id, owner = OId, population = Pop} = P| T], OId) when Pop > 1 ->
	{Id, OId, [P#planet{population = Pop - 2} | T]};
takeTwo([H | T], OId) -> {Id, _, TT} = takeTwo(T, OId), {Id, OId, [H| TT]}.

sendToNext({undefined, _, L}) -> L;
sendToNext({_, _, []}) -> [];
sendToNext({FId, Id, [#planet{id = TId, owner = NOId} | _] = L}) when NOId =/= Id ->
	io:format("F ~p ~p 2\n", [FId, TId]),L;
sendToNext({FId, Id, [H | T]}) -> [H| sendToNext({FId, Id, T})].

takeTurn(#world{planets = P, myid = Id}) ->
	upgrade(sendToNext(takeTwo(P, Id)), Id),
	io:format(".\n").

play() ->
	World = readInput(#world{}),
	takeTurn(World),
	play().

main([]) ->
	play().