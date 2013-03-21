-module (riemann_syslog_octet_parser).

-export ([parse/1]).

%% http://tools.ietf.org/html/rfc3164#section-4.1
-define (MAX_LENGTH, 1024).
-define (is_bin_number(Digit), Digit >= $0 andalso Digit =< $9).

%%%%
%% Iterate through the buffer and gather valid octet frames
%%%%
parse(Buffer) ->
  parse(Buffer, []).

parse(<<>>, Frames)->
  {Frames, <<>>};
parse(Buffer, Frames)->
  case frame(Buffer) of
    {ok, Frame, Rest} ->
      parse(Rest, Frames++[Frame]);
    eos ->
      {Frames, <<"">>};
    _ ->
      {Frames, Buffer}
  end.

%%%%
%% Find the next frame in the buffer and analyize it
%%%%
frame(Buffer) ->
  analyze(scan(Buffer, 0)).

%%%%
%% Scan for the next frame
%%%%

%% We have a space and have gathered some numbers
scan(<<" ", Rest/binary>>, Length) when Length =/= 0 andalso Length < ?MAX_LENGTH ->
  {ok, Length, Rest};
%% We have a number
scan(<<Digit, Rest/binary>>, Length) when ?is_bin_number(Digit) ->
  scan(Rest, Length*10+(Digit-$0));
%% We've at the end of the stream and we were in the middle of checking numbers
scan(<<>>, Length) when Length =/= 0  ->
  {eos, Length};
%% We've at the end of the stream and haven't found any numbers
scan(<<>>, _)  ->
  eos;
%% Move on to the next byte, we don't understand this one
scan(<<_, Rest/binary>>, _)->
  scan(Rest, 0).

%%%%
%% analyze the frame
%%%%

%% We are at the end of the stream and have a perfect match
analyze({ok, Length, Rest}) when byte_size(Rest) =:= Length ->
  split(Rest, Length);
%% There's still more in the buffer so we can do some checks
analyze({ok, Length, Rest}) when byte_size(Rest) > Length ->
  %% check the next frame to see if we have a valid frame here
  case split(Rest, Length) of
    %% The next frame starts with a digit; in most cases this means we have
    %% a valid frame
    {ok, Frame, <<Digit,_/binary>>=Rest2} when ?is_bin_number(Digit) ->
      {ok, Frame, Rest2};

    %% The next "frame" after `Length` doesn't start with a number so we're
    %% probably not looking at a valid frame here; move on by skipping this
    %% `Length`
    _ ->
      {ok, _, Rest2} = split(Rest, byte_size(<<Length>>)),
      frame(Rest2)
  end;
%% We need more in the buffer to understand this frame
analyze({ok, _, _}) ->
  continue;
%% Pass on the result if we don't understand it
analyze(Result) ->
  Result.

%%%%
%% Split `Bin` by `Length` in bytes
%%%%
split(Bin, Length)->
  <<First:Length/binary, Second/binary>> = Bin,
  {ok, First, Second}.
