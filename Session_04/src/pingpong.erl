-module(pingpong).

-export([ping/1, pong/1]).

% 1. Start erl -sname pingNode
% 2. Start erl -sname pongNode (start both in the directory containing pingpong.erl!)
% 3. In ping, execute pingpong:ping('pongNode@YOUR-MACHINE-NAME').

ping(PongNode) ->
    register(pingProcess, self()),

    % Connect to other node
    net_kernel:connect_node(PongNode),
    % Start pong on other node
    PongPid = spawn_link(PongNode, ?MODULE, pong, [self()]),

    % TODO
    todo.

pong(PingPid) ->
    register(pongProcess, self()),
    % TODO
    todo.

