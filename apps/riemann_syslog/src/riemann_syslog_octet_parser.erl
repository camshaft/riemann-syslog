-module (riemann_syslog_octet_parser).

-export ([parse/1]).

-define (UPPER, 58). %% <<"9">> =:= 57
-define (LOWER, 47). %% <<"0">> =:= 48

-define (is_bin_number(Digit), Digit > ?LOWER andalso Digit < ?UPPER).

%%%%
%% Iterate through the buffer and gather valid octet frames
%%%%
parse(Buffer) ->
  parse(Buffer, []).

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
  analyze(scan(Buffer)).

%%%%
%% Scan for the next frame
%%%%
scan(Buffer) ->
  scan(Buffer, []).

%% We have a space
scan(<<" ", Rest/binary>>, Numbers) when length(Numbers) > 0 ->
  {ok, list_to_integer(Numbers), Rest};
%% We have a number
scan(<<Digit, Rest/binary>>, Numbers) when ?is_bin_number(Digit) ->
  scan(Rest, Numbers++[Digit]);
%% We've at the end of the stream and we were in the middle of checking numbers
scan(<<>>, Numbers) when length(Numbers) > 0  ->
  {eos, Numbers};
%% We've at the end of the stream and haven't found any numbers
scan(<<>>, _)  ->
  eos;
%% Move on to the next byte, we don't understand this one
scan(<<_, Rest/binary>>, _)->
  scan(Rest, []).

%%%%
%% analyze the frame
%%
%% TODO we probably want to have a max buffer size so we don't get too big
%%      i.e. if we parse 12345678987654321, that's a pretty big buffer and
%%      it's most likely to be a parse error
%%%%

%% We are at the end of the stream and have a perfect match
analyze({ok, Length, Rest}) when byte_size(Rest) =:= Length ->
  split(Rest, Length);
%% There's still more in the buffer so we can do some checks
analyze({ok, Length, Rest}) when byte_size(Rest) > Length ->
  %% check the next frame to see if we have a valid frame here
  case binary:at(Rest, Length) of
    %% The next frame starts with a digit; in most cases this means we have
    %% a valid frame
    Digit when ?is_bin_number(Digit) ->
      split(Rest, Length);

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
  BitLength = Length*8,
  <<First:BitLength, Second/binary>> = Bin,
  {ok, binary:encode_unsigned(First), Second}.
