%% Copyright (C) 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%%
%% This file is part of Serlvers.
%%
%% Serlvers is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.
%% Serlvers is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Serlvers; if not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
%% Linking Serlvers statically or dynamically with other modules is making a combined work based on Serlvers. Thus, the terms and conditions of the GNU General Public License cover the whole combination.
%% In addition, as a special exception, the copyright holders of Serlvers give you permission to combine Serlvers program with code included in the standard release of Erlang/OTP under the Erlang Public Licence (or modified versions of such code, with unchanged license). You may copy and distribute such a system following the terms of the GNU GPL for Serlvers and the licenses of the other code concerned, provided that you include the source code of that other code when and as the GNU GPL requires distribution of source code.

%% @author José Pablo Ezequiel "Pupeno" Fernández Silva <pupeno@pupeno.com> [http://pupeno.com]
%% @copyright 2006 José Pablo Ezequiel "Pupeno" Fernández Silva
%% @doc The gen_time behaviour is used to implement Time servers according to <a href="http://www.ietf.org/rfc/rfc868.txt">RFC868</a>.
%% <p>To make a time server make a module implementing the behaviour gen_time, that requires the following functions:</p>
%% <ul>
%%  <li>init/1: Initialization code. It gets the arguments passed to one of the start functions and it should return the state of the time server.</li>
%%  <li>time/1: The main function. The parameter is the state of the server. The result should be a tuple of two items, the first being the string that would be sent as time, the second being the new state.</li>
%%  <li>terminate/2: Any clean up code should be here. The first parameter is the reason to terminate, the second is the state.</li>
%% </ul>
%% @see launcher.
%% @since 0.1.0

-module(gen_time).
-behaviour(gen_server).
-export([start/3, start/4, start_link/3, start_link/4, stop/1]).
-export([init/1, handle_call/3,  handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([behaviour_info/1]).

%% @doc Function used by Erlang (compiler?) to ensure that a module implementing gen_time really is exporting the needed functions.
%% @private Only Erlang itself should call this function.
%% @since 0.1.0
behaviour_info(callbacks) ->
    [{init, 1}, {time, 1}, {terminate, 2}];
behaviour_info(_) ->
    undefined.

%% @doc Start an unnamed time server.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/4
%% @see start_link/3
%% @since 0.1.0
%% @spec (Module::atom(), Args::term(), Options) -> Result
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(Module, Args, Options) ->
    %%io:fwrite("~w:start(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start(?MODULE, {Module, Args}, Options).

%% @doc Start a named time server.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start/3
%% @see start_link/4
%% @since 0.1.0
%% @spec (SupName, Module::atom(), Args::term(), Options) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start(SupName, Module, Args, Options) ->
    %%io:fwrite("~w:start(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start(SupName, ?MODULE, {Module, Args}, Options).

%% @doc Start an unnamed time server and link to it.
%% <p>See {@link start_link/4} for further explanation of the parameters.</p>
%% @see start_link/4
%% @see start/3
%% @since 0.1.0
%% @spec (Module::atom(), Args::term(), Options) -> Result
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(Module, Args, Options) ->
    %%io:fwrite("~w:start_link(~w, ~w, ~w)~n", [?MODULE, Module, Args, Options]),
    gen_server:start_link(?MODULE, {Module, Args}, Options).

%% @doc Start a named time server and link to it.
%% <p>The parameters are exactly the same as gen_server and most of them (like Options) are passed as is to gen_server.</p>
%% @see start_link/3
%% @see start/4
%% @since 0.1.0
%% @spec (SupName, Module::atom(), Args::term(), Options) -> Result
%%   SupName = {local, atom()} | {global, atom()}
%%   Options = [Option]
%%     Option = {debug, Dbgs} | {timeout, Time} | {spawn_opt, SOpts} 
%%       Dbgs = [Dbg] 
%%         Dbg = trace | log | statistics | {log_to_file, FileName} | {install, {Func, FuncState}} 
%%       SOpts = [term()] 
%%   Result = {ok, Pid} | {error, {already_started, Pid}} | {error, Error}
start_link(SupName, Module, Args, Options) ->
    %%io:fwrite("~w:start_link(~w, ~w, ~w, ~w)~n", [?MODULE, SupName, Module, Args, Options]),
    gen_server:start_link(SupName, ?MODULE, {Module, Args}, Options).

%% @doc Stop a named process.
%% @see start/3
%% @see start/4
%% @see start_link/3
%% @see start_link/4
%% @since 0.1.0
%% @spec (Name) -> ok
%%   Name = atom() | {local, atom()} | {global, atom()}
stop(Module) ->
    gen_server:cast(Module, stop).

%% @doc This function gets called by gen_server to initialize the module. After some basic internal initialization the init function of the module implementing the particular time server gets called (same as this module implementing a particular gen_server).
%% @private Only gen_server should call this function.
%% @since 0.1.0
init({Module, Args}) ->
    %%io:fwrite("~w:init(~w)~n", [?MODULE, {Module, Args}]),
    process_flag(trap_exit, true), % To get terminate/2 called when exiting.
    {ok, ModState} = Module:init(Args),
    {ok, {Module, ModState}}.

%% @doc The base module, gen_server may call this function. Currently there's nothing to be done here.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_call(_Request, _From, State) ->
    %%io:fwrite("~w:handle_call(~w, ~w, ~w)~n", [?MODULE, _Request, _From, State]),
    {noreply, State}.

%% @doc This function handles the stoping of a time server.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_cast(stop, State) ->
    %%io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, stop, State]),
    {stop, normal, State};
handle_cast(_Request, State) ->
    %%io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, _Request, State]),
    {noreply, State}.

%% @doc This function handles the udp and tcp case.
%% @private Only gen_server should call this function.
%% @since 0.1.0
handle_info({connected, Socket}, {Module, ModState}) ->
    %%io:fwrite("~w:handle_cast(~w, ~w)~n", [?MODULE, {started, Socket}, {Module, ModState}]),
    {Reply, NewModState} = Module:time(ModState),
    gen_tcp:send(Socket, Reply),
    {stop, normal, {Module, NewModState}};
handle_info({udp, Socket, IP, InPortNo, _Packet}, {Module, ModState}) -> % Handle UDP packages.
    %%io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, {udp, Socket, IP, InPortNo, _Packet} , {Module, ModState}]),
    {Reply, NewModState} = Module:time(ModState), % Generate the reply.
    gen_udp:send(Socket, IP, InPortNo, Reply),       % Send the reply.
    ok = inet:setopts(Socket, [{active, once}]),     % Enable receiving of packages, get the next one.
    {stop, normal, {Module, NewModState}};
handle_info(_Info, State) ->
    %%io:fwrite("~w:handle_info(~w, ~w)~n", [?MODULE, _Info, State]),
    {noreply, State}.

%% @doc This function get's called by the underling gen_server and we just pass it over to the module implementing a time server.
%% @private Only gen_server should call this function.  
%% @since 0.1.0
terminate(Reason, {Module, ModState}) ->
    %%io:fwrite("~w:terminate(~w, ~w)~n", [?MODULE, Reason, {Module, ModState}]),
    ok = Module:terminate(Reason, ModState),
    ok.

%% @doc Err... &lt;sarcasm&gt;code changes ?&lt;/sarcasm&gt;
%% @private I think no one is interested in this function, yet.
%% @since 0.1.0
code_change(_OldVsn, State, _Extra) ->
    %%io:fwrite("~w:code_change(~w, ~w, ~w)~n", [?MODULE, _OldVsn, State, _Extra]),
    {ok, State}.
