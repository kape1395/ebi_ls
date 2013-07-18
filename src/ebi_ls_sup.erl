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
-module(ebi_ls_sup).
-behaviour(supervisor).
-compile([{parse_transform, lager_transform}]).
-export([start_link/3]).
-export([init/1]).


%% =============================================================================
%%  Public API.
%% =============================================================================

%%
%%  Start supervisor.
%%
start_link(SolverCount, WorkDir, PortName) ->
    supervisor:start_link(?MODULE, {SolverCount, WorkDir, PortName}).



%% =============================================================================
%%  Callbacks for supervisor.
%% =============================================================================

%%
%%  Configure supervisor.
%%
init({SolverCount, WorkDir, PortName}) ->
    SolverSup = ebi_ls_solver_sup,
    {ok, {{one_for_all, 10, 60}, [
        {SolverSup,
            {SolverSup, start_link, [SolverCount, WorkDir, PortName]},
            permanent, brutal_kill, supervisor, [SolverSup]
        }
    ]}}.


