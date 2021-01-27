%% Module for client support of the STOMP messaging protocol (http://stomp.codehaus.org/Protocol).
%% Version 0.1
%% Authored by Ian Brown (spam@hccp.org)
%% Documentation can be found at http://www.hccp.org/erlang-stomp-client.html
%% This was an experiment to get a feel for the Erlang language, and provide simple client access to STOMP suppurting message brokers.
%% Please feel free to use and re-distribute as you see fit. Comments, improvements and questions welcome.

-module (stomp).
-export ([connect/4]). %% "You sunk my scrabbleship!"
-export ([connect/5]).
-export ([disconnect/1]).
-export ([subscribe/2]).
-export ([subscribe/3]).
-export ([unsubscribe/2]).
-export ([get_messages/1]).
-export ([get_messages/2]).
-export ([get_message_id/1]).
-export ([ack/2]).
-export ([ack/3]).
-export ([send/4]).
-export ([begin_transaction/2]).
-export ([commit_transaction/2]).
-export ([abort_transaction/2]).
-export ([on_message/2]).
-export ([on_message_with_conn/2]).

tcp_module() ->
  tcp_module(application:get_env(stomp, use_ssl, false)).

tcp_module(true) -> ssl;
tcp_module(_) -> gen_tcp.

tcp_params() -> [{active, false}].

tcp_params(ssl) -> verify_cert() ++ tcp_params();
tcp_params(_) -> tcp_params().

verify_cert() ->
  verify_cert(application:get_env(stomp, ssl_insecure, true)).

verify_cert(false) -> [{verify, verify_peer}];
verify_cert(_) -> [{verify, verify_none}, {reuse_sessions, true}].

%% Example:	Conn = stomp:connect("localhost", 61613, "", "").
connect (Host, PortNo, Login, Passcode)  ->
  connect(Host, PortNo, Login, Passcode, []).

connect (Host, PortNo, Login, Passcode, Options)  ->
	Message=lists:append(["CONNECT", "\nlogin: ", Login, "\npasscode: ", Passcode,
    concatenate_options(Options), "\n\n", [0]]),
  Mod = tcp_module(),
	Tcp = Mod:connect(Host,PortNo, tcp_params(Mod)),
  handle_tcp(Tcp, Message).

handle_tcp({ok, Sock}, Message) ->
  Mod = tcp_module(),
  Mod:send(Sock,Message),
  {ok, Response}=Mod:recv(Sock, 0),
  M = get_message(Response),
  handle_message(M, Sock); %%UGLY!
handle_tcp(Error, _) ->
  Error.

handle_message([{type, Type}, Headers, _, _], Sock) ->
  connection_case(Type, Headers, Sock);
handle_message(Error, _) ->
  Error.

connection_case("CONNECTED", _, Sock) -> Sock;
connection_case(_, {headers, [_, {"message", Msg}]}, _) ->
  {error, "Error occured during connection attempt. " ++ Msg};
connection_case(_, {headers, []}, _) ->
  {error, "The connection could not be started, please verify that server port
    is open and you're using the proper transport"}.

%% Example: stomp:subscribe("/queue/foobar", Conn).

subscribe (Destination, Connection) ->
	subscribe (Destination, Connection, [{"ack","auto"}]),
	ok.

%%  Example: stomp:subscribe("/queue/foobar", Conn, [{"ack", "client"}]).
%%  Example: stomp:subscribe("/queue/foobar", Conn, [{"ack", "client"}, {"activemq.prefetchSize", 1}]).


subscribe (Destination, Connection, Options) ->
	Message=lists:append(["SUBSCRIBE", "\ndestination: ", Destination,
    concatenate_options(Options), "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	ok.


%% Example: stomp:unsubscribe("/queue/foobar", Conn).

unsubscribe (Destination, Connection) ->
	Message=lists:append(["UNSUBSCRIBE", "\ndestination: ", Destination, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	ok.

%% Example: stomp:disconnect(Conn).

disconnect (Connection) ->
	Message=lists:append(["DISCONNECT", "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	Mod:close(Connection),
	ok.

%% Example: stomp:get_message_id(Message).

get_message_id ([_, {headers, Headers}, _]) ->
	get_message_id (Headers);
get_message_id ([H|T]) ->
	case (H) of
		{"message-id", MessageId}->MessageId;
		_ -> get_message_id(T)
	end;
get_message_id ([])	->
	throw("No header with name of 'message-id' was found.").

%% Example: stomp:ack(Conn, Message).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message)).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1").

ack (Connection, [Type, Headers, Body]) ->
	MessageId=get_message_id([Type, Headers, Body]),
	ack(Connection, MessageId);
ack (Connection, MessageId)	->
	AckMessage=lists:append(["ACK", "\nmessage-id: ", MessageId, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,AckMessage),
	ok.

%% Example: stomp:ack(Conn, Message, TransactionId).
%% Example: stomp:ack(Conn, stomp:get_message_id(Message), TransactionId).
%% Example: stomp:ack(Conn, "ID:phosphorus-63844-1247442885553-3:1:1:1:1", TransactionId).

ack (Connection, [Type, Headers, Body], TransactionId) ->
	MessageId=get_message_id([Type, Headers, Body]),
	ack(Connection, MessageId, TransactionId);
ack (Connection, MessageId, TransactionId)	->
	AckMessage=lists:append(["ACK", "\nmessage-id: ", MessageId, "\ntransaction: ", TransactionId, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,AckMessage),
	ok.

%% Example: stomp:send(Conn, "/queue/foobar", [], "hello world").
%% Example: stomp:send(Conn, "/queue/foobar", [{"priority","15"}], "high priority hello world").

send (Connection, Destination, Headers, MessageBody) ->
	Message=lists:append(["SEND", "\ndestination: ", Destination, concatenate_options(Headers), "\n\n", MessageBody, [0]]),
  Mod = tcp_module(),
	Mod:send(Connection, unicode:characters_to_binary(Message)).

%% Example: stomp:get_messages(Conn).

get_messages (Connection) ->
	get_messages (Connection, 5000).

get_messages (Connection, Timeout) ->
	get_messages (Connection, [], Timeout).

get_messages (Connection, Messages, Timeout) ->
  Response = do_recv(Connection, Timeout),
  get_messages(Connection, Messages, Response, Timeout).

get_messages(_, Messages, [], _) ->
	Messages;
get_messages(Connection, Messages, Response, Timeout) ->
  M = get_message(Response),
  handle_message(Response, M, Connection, Messages, Timeout).

handle_message(_,[{type, "MESSAGE"}, {headers, Headers}, {body, MessageBody}, TheRest], Connection, Messages, Timeout) ->
  get_messages (Connection, lists:append(Messages, [[{type, "MESSAGE"}, {headers, Headers}, {body, MessageBody}]]), get_rest(TheRest), Timeout);
handle_message(_,[{type, "ERROR"}, {headers, Headers}, {body, MessageBody}, TheRest], Connection, Messages, Timeout) ->
  get_messages (Connection, lists:append(Messages, [[{type, "ERROR"}, {headers, Headers}, {body, MessageBody}]]), get_rest(TheRest), Timeout);
handle_message(Response, [_, _, _, "\n"], Connection, _, Timeout) ->
  log_error(Response),
  get_messages (Connection, [], Timeout);
handle_message(Response, [_, _, _, TheRest], Connection, Messages, Timeout) ->
  log_error(Response),
  get_messages (Connection, Messages, get_rest(TheRest), Timeout);
handle_message(Response, Error,_, _, _) ->
  log_error(Response),
  {error, Error}.

log_error(Response) ->
  error_logger:error_msg("Stomp message error ~P~n", [Response, 1000]).

%% U.G.L.Y. . . .  you ain't got no alibi.
%% 6/24/11 I think the rest is when more than one message is retrived at at given time...in any case, looks like large messages are sometimes missing an expected terminationg 0 char?
%% 6/24/11 ahh...the actual issue is when the message exceeds the read window size, we don't have the entire message...so it looks like it is not terminated beacuse it is not yet terminated
get_rest([])->
  [];
get_rest([_|TheRest])->
  ltrim(TheRest).

do_recv(Connection, Timeout)->
  do_recv(Connection, [], Timeout).

do_recv(Connection, [], Timeout)->
  Mod = tcp_module(),
  Response = case Mod:recv(Connection, 0, Timeout) of
		     {ok, Result} -> Result;
		     {error, timeout} -> [eof];
		     {error, Err} -> 
			     error_logger:error_msg("Error while receiving datat ~p~n", Err),
					 [eof]
	     end,

  %{ok, Response} = Mod:recv(Connection, 0, Timeout),
  do_recv(Connection, Response, Timeout);
do_recv(Connection, Response, Timeout)->
  case is_eof(Response) of
    true ->
      ltrim(Response);
    _ ->
      Mod = tcp_module(),
      {ok, Data} = Mod:recv(Connection, 0, Timeout),
      case Data of
        [10] -> do_recv(Connection, Response, Timeout);
        _ -> do_recv(Connection, lists:flatten([Response, Data]), Timeout)
      end
  end.

is_eof([_ | [0, 10]]) ->
  true;
is_eof([eof | _]) ->
	true;
is_eof([_ | T]) ->
  is_eof(T);
is_eof(_) ->
  false.

ltrim([eof|T]) ->
	ltrim(T);
ltrim([10 | T]) ->
  ltrim(T);
ltrim(T) ->
  T.

%% Example: MyFunction=fun([_, _, {_, X}]) -> io:fwrite("message ~s ~n", [X]) end, stomp:on_message(MyFunction, Conn).

on_message (F, Conn) ->
	Messages=get_messages(Conn),
	apply_function_to_messages(F, Messages),
	on_message(F, Conn).

on_message_with_conn (F, Conn) ->
	Messages=get_messages(Conn),
	apply_function_to_messages(F, Messages, Conn),
	on_message_with_conn(F, Conn).

%% Example: stomp:begin_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").

begin_transaction (Connection, TransactionId) ->
	Message=lists:append(["BEGIN", "\ntransaction: ", TransactionId, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	ok.

%% Example: stomp:commit_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").

commit_transaction (Connection, TransactionId) ->
	Message=lists:append(["COMMIT", "\ntransaction: ", TransactionId, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	ok.


%% Example: stomp:abort_transaction(Conn, "MyUniqueTransactionIdBlahBlahBlah1234567890").

abort_transaction (Connection, TransactionId) ->
	Message=lists:append(["ABORT", "\ntransaction: ", TransactionId, "\n\n", [0]]),
  Mod = tcp_module(),
	Mod:send(Connection,Message),
	ok.

%% PRIVATE METHODS . . .
concatenate_options ([]) ->
	[];
concatenate_options ([H|T]) ->
	{Name, Value}=H,
	lists:append(["\n", Name, ": ", Value, concatenate_options(T)]).

apply_function_to_messages(_, []) ->
	ok;
apply_function_to_messages(F, [H|T]) ->
	F(H),
	apply_function_to_messages(F, T).

apply_function_to_messages(_, [],_) ->
	ok;
apply_function_to_messages(F, [H|T], Conn) ->
	F(H, Conn),
	apply_function_to_messages(F, T, Conn).

% MESSAGE PARSING  . . . get's a little ugly in here . . . would help if I truly grokked Erlang, I suspect.
% 7/12/09 - yeah, ugly indeed, i need to make this use the same pattern as get_headers_from_raw_src . . . currently scanning header block multiple times and making unnecessary copies
get_message(Message) ->
  handle_type(get_type(Message)). %% Ugly . . .

handle_type([Type, {Headers, MessageBody}, TheRest]) ->
  {ParsedHeaders, _}=get_headers_from_raw_src([], Headers),
  [{type, Type}, {headers, ParsedHeaders}, {body, MessageBody}, TheRest];
handle_type(Error) ->
  Error.

%% extract message body
get_message_body ([H|T]) ->
  get_message_body ([H|T], []).

get_message_body ([H|T], MessageBody) ->
	case(H) of
		0 -> {MessageBody, T};
		_ ->
      {MyMessageBody, TheRest}=get_message_body(T, MessageBody),
      {lists:append([MessageBody, [H], MyMessageBody]), TheRest}
	end;
get_message_body ([],[]) ->
  {[],[]}.

%% extract headers as a blob of chars, after having iterated over . . .

get_headers (Message) ->
	get_headers (Message, []).

get_headers (Message, Headers) ->
	get_headers (Message, Headers, -1).
get_headers ([H|T], Headers, LastChar) ->
	case ({H, LastChar}) of
		{10, 10} ->
            {MessageBody, TheRest}=get_message_body(T),
            [{Headers, MessageBody}, TheRest];
		{_, _} -> get_headers(T, lists:append([Headers, [H]]), H)
	end;
get_headers ([], Headers, _) ->
  [{Headers, []}, []].

%% extract type ("MESSAGE", "CONNECT", etc.) from message string . . .

get_type(Message) ->
	get_type (Message, []).

get_type ([], Type) ->
	Type;
get_type ([H|T], Type) ->
	case (H) of
		10 -> [{Headers, MessageBody}, TheRest] = get_headers(T), [Type, {Headers, MessageBody}, TheRest];
		_ -> get_type(T, lists:append([Type, [H]]))
	end;
get_type (Error, _) ->
  Error.

%% parse header clob into list of tuples . . .
get_headers_from_raw_src (Headers, []) ->
	{Headers, []};
get_headers_from_raw_src(Headers, RawSrc) ->
	{Header, RestOfList}=get_header(RawSrc),
	get_headers_from_raw_src(lists:append([Headers, [Header]]), RestOfList).

get_header (RawSrc) ->
	{HeaderName, RestOfListAfterHeaderExtraction}=get_header_name([], RawSrc),
	{HeaderValue, RestOfListAfterValueExtraction}=get_header_value([], RestOfListAfterHeaderExtraction),
	{{HeaderName, HeaderValue}, RestOfListAfterValueExtraction}.


get_header_name (HeaderName, [H|T]) ->
	case (H) of
		58 ->  {HeaderName, T};
		_ -> get_header_name(lists:append([HeaderName, [H]]), T)
	end;
get_header_name (HeaderName, []) ->
  {HeaderName, []}.

get_header_value (HeaderValue, [H|T]) ->
	case (H) of
		10 -> {HeaderValue, T};
		_ -> get_header_value(lists:append([HeaderValue, [H]]), T)
	end;
get_header_value (HeaderValue, []) ->
  {HeaderValue, []}.
