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
-module(ebi_ls_app).
-behaviour(application).
-compile([{parse_transform, lager_transform}]).
-export([start/2, stop/1]).
-define(APP, ebi_ls).


%% =============================================================================
%%  Callbacks for application.
%% =============================================================================


%%
%%  Start app.
%%
start(normal, _StartArgs) ->
    {ok, SolverCount} = param(solver_count),
    {ok, WorkDir} = param(work_dir),
    {ok, PortName} = param(port_name),
    ok = make_dir(WorkDir),
    ebi_ls_sup:start_link(SolverCount, WorkDir, PortName).


%%
%%  Stop app.
%%
stop(_State) ->
    ok.



%% =============================================================================
%%  Private functions.
%% =============================================================================

%%
%%  Resolve application parameters.
%%
param(solver_count = ParamName) ->
    case application:get_env(?APP, ParamName) of
        {ok, SolverCount} ->
            {ok, SolverCount};
        undefined ->
            {ok, erlang:system_info(logical_processors)}
    end;

param(work_dir = ParamName) ->
    case application:get_env(?APP, ParamName) of
        {ok, WorkDir} ->
            {ok, WorkDir};
        undefined ->
            {ok, app_file(["priv", "work"])}
    end;

param(port_name = ParamName) ->
    case application:get_env(?APP, ParamName) of
        {ok, PortName} ->
            {ok, PortName};
        undefined ->
            {ok, app_file(["priv", "ebi_ls_solver"])}
    end.


%%
%%  Create dir if not exists and create all its parents.
%%
make_dir(Dir) ->
    case file:make_dir(Dir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        {error, enoent} ->
            case make_dir(filename:dirname(Dir)) of
                ok ->
                    file:make_dir(Dir);
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.


%%
%%  Resolves local file name to an absolute file name.
%%
app_file(LocalFileComponents) ->
    Ebin = filename:dirname(code:which(?MODULE)),
    filename:join([filename:dirname(Ebin)] ++ LocalFileComponents).


