%
% Copyright 2013 Karolis Petrauskas
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
-module(ebi_ls_solver).
-behaviour(ebi_solver).
-behaviour(gen_fsm).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3]).
-export([solve/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([starting/2, waiting/3, solving/3]).
-include_lib("ebi_core/include/ebi.hrl").

-define(RESTART_DELAY, 1000).
-define(SUPPORTED_MODEL_TYPE, bio_solver_xml_v1).



%% =============================================================================
%%  Public API.
%% =============================================================================


%%
%%  Starts the solver.
%%
-spec start_link(integer(), string(), string())
        -> {ok, pid()} | term().

start_link(SolverIndex, WorkDir, PortName) ->
    Args = {SolverIndex, WorkDir, PortName},
    gen_fsm:start_link(?MODULE, Args, []).



%% =============================================================================
%%  Internal state of this module.
%% =============================================================================


-record(state, {
    index,          %%  Solver index.
    work_dir,       %%  Work dir.
    port_name,      %%  Port program name.
    port,           %%  Current port.
    model           %%  Current model.
}).



%% =============================================================================
%%  Callbacks for ebi_solver.
%% =============================================================================

%%
%%
%%
-spec solve(pid(), #model{})
        -> ok | {error, Reason :: term()}.

solve(Pid, Model) ->
    gen_fsm:sync_send_event(Pid, {solve, Model}).



%% =============================================================================
%%  Callbacks for gen_fsm.
%% =============================================================================

%%
%%  FSM Initialization.
%%
init({SolverIndex, WorkDir, PortName}) ->
    gen_fsm:send_event(self(), start),
    State = #state{
        index = SolverIndex,
        work_dir = WorkDir,
        port_name = PortName
    },
    {ok, starting, State}.


%%
%%  FSM State: starting
%%
starting(start, StateData) ->
    receive after ?RESTART_DELAY -> ok end,
    ok = request_new_job(),
    {next_state, waiting, StateData}.


%%
%%  FSM State: waiting
%%
waiting({solve, Model}, _From, StateData) ->
    {ok, NewStateData} = start_solver(Model, StateData),
    {reply, ok, solving, NewStateData}.


%%
%%  FSM State: solving
%%
solving({solve, _Model}, _From, StateData) ->
    {reply, {error, bussy}, solving, StateData}.



%%
%%  Messages from port.
%%
handle_info({Port, {data, {eol, Line}}}, StateName = solving, StateData = #state{port = Port}) ->
    ok = handle_solver_response(Line, StateData),
    NewStateData = StateData#state{
        model = undefined
    },
    {next_state, StateName, NewStateData};

handle_info({'EXIT', Port, _PosixCode}, solving, StateData = #state{port = Port, model = Model}) ->
    case Model of
        #model{} ->
            ok = handle_solver_response("ERROR", StateData);
        undefined ->
            ok
    end,
    ok = request_new_job(),
    NewStateData = StateData#state{
        port = undefined,
        model = undefined
    },
    {next_state, waiting, NewStateData}.


%%
%%  Unused.
%%
handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%
%%  Unused.
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, undefined, StateName, StateData}.


%%
%%  Termination.
%%
terminate(_Reason, _StateName, _StateData) ->
    ok.


%%
%%  Hot updates.
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.



%% =============================================================================
%%  Helper functions.
%% =============================================================================


%%
%%  Ask queue for new job.
%%
request_new_job() ->
    ok = ebi_queue:solver_ready(self(), [?SUPPORTED_MODEL_TYPE]).       % TODO


%%
%%  Start the solver program.
%%
start_solver(Model, StateData) ->
    #state{
        port_name = PortName,
        work_dir = WorkDir
    } = StateData,
    #model{
        ref = ModelRef,
        parameters = ModelParams
    } = Model,
    CfgName = ModelRef ++ ".cfg.xml",
    DirName = ModelRef,
    SymbolArgs = [ format_symbol_arg(P) || P <- ModelParams ],

    {ok, CfgBody} = ebi_store:get_model_representation(ModelRef, ?SUPPORTED_MODEL_TYPE),    % TODO
    ok = file:write_file(CfgName, CfgBody),

    Port = erlang:open_port({spawn_executable, PortName}, [
        use_stdio,
        {line, 1024},
        {cd, WorkDir},
        {args, [CfgName, DirName] ++ SymbolArgs}
    ]),
    NewStateData = StateData#state{
        port = Port,
        model = Model
    },
    {ok, NewStateData}.


handle_solver_response(ResponseLine, #model{ref = ModelRef}) ->
    [Status | Other] = string:tokens(ResponseLine, "\t\n "),
    case Status of
        "DONE" ->
            [TimeStr, _StepCountStr, CurrentDensityStr] = Other,
            ok = ebi_store:simulation_done(             % TODO
                ModelRef,
                {file, ModelRef ++ ".tar.gz"},
                to_number(TimeStr),
                to_number(CurrentDensityStr)
            );
        "ERROR" ->
            ok = ebi_store:simulation_failed(ModelRef)  % TODO
    end.



%%
%%  Format symbol argument for the port.
%%
format_symbol_arg(#param{name = N, value = V}) ->
    io_lib:format("-S~s=~p", [N, V]).


%%
%%  Convert string to number.
%%
to_number(Str) ->
    case string:to_float(Str) of
        {error, no_float} ->
            {Number, []} = string:to_integer(Str),
            Number;
        {Number,[]} ->
            Number
    end.


