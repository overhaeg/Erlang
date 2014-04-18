server_multiple_actors:initialize(),
{A,B} = server_multiple_actors:register_user(),
{C,D} = server_multiple_actors:register_user(),
server_multiple_actors:subscribe(B,A,C).
