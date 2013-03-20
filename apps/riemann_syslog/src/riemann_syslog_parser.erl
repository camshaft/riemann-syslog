-module (riemann_syslog_parser).

-export ([parse/1]).

parse(Data) ->
  parse(Data, []).

parse(Data, Frames)->
  case extract_frame(Data) of
    {ok, Frame, Rest} ->
      parse(Rest, Frames++[Frame]);
    {return, Data} ->
      {Frames, Data};
    eos ->
      {Frames, <<"">>}
  end.

extract_frame(Data) ->
  case bin_match(Data) of
    {ok, Length, Rest} when byte_size(Rest) >= Length ->
      BitLength = Length*8,
      <<First:BitLength, Second/binary>> = Rest,
      {ok, binary:encode_unsigned(First), Second};
    eos ->
      eos;
    {eos, _} ->
      {return, Data};
    _ ->
      {return, Data}
  end.

%% Find the next frame
bin_match(Data) ->
  bin_match(Data, []).

%% We have a space
bin_match(<<" ", Rest/binary>>, Numbers) when (erlang:length(Numbers) > 0) ->
  {ok, list_to_integer(Numbers), Rest};
%% We have a number
bin_match(<<Digit:8, Rest/binary>>, Numbers) when (Digit >= 48 andalso Digit < 58) ->
  bin_match(Rest, Numbers++[Digit]);
%% We've at the end of the stream and we were in the middle of checking numbers
bin_match(<<>>, Numbers) when (erlang:length(Numbers) > 0)  ->
  {eos, Numbers};
%% We've at the end of the stream and haven't found any numbers
bin_match(<<>>, _)  ->
  eos;
%% Move on to the next byte, we don't understand this one
bin_match(<<_:8, Rest/binary>>, _)->
  bin_match(Rest, []).
