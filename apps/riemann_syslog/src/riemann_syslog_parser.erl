-module (riemann_syslog_parser).

-export ([parse/1]).

-define (UPPER, 58).
-define (LOWER, 47).

parse(Data) ->
  parse(Data, []).

parse(Data, Frames)->
  case extract_frame(Data) of
    {ok, Frame, Rest} ->
      parse(Rest, Frames++[Frame]);
    eos ->
      {Frames, <<"">>};
    _ ->
      {Frames, Data}
  end.

extract_frame(Data) ->
  case bin_match(Data) of

    %% We are at the end of the stream and have a perfect match
    {ok, Length, Rest} when byte_size(Rest) =:= Length ->
      split(Rest, Length);

    %% check the next frame to see if we have a valid frame here
    {ok, Length, Rest} when byte_size(Rest) >= Length ->
      
      case binary:at(Rest, Length) of

        %% The next frame starts with a digit; in most cases this means we have 
        %% a valid frame
        Digit when (Digit > ?LOWER andalso Digit < ?UPPER) ->
          split(Rest, Length);

        %% The next "frame" after `Length` doesn't start with a number so we're
        %% probably not looking at a valid frame here; move on by skipping this
        %% `Length`
        _ ->
          {ok, _, Rest2} = split(Rest, byte_size(<<Length>>)),
          extract_frame(Rest2)

      end;
    eos ->
      eos;
    _ ->
      return
  end.

%% Find the next frame
bin_match(Data) ->
  bin_match(Data, []).

%% We have a space
bin_match(<<" ", Rest/binary>>, Numbers) when (erlang:length(Numbers) > 0) ->
  {ok, list_to_integer(Numbers), Rest};
%% We have a number
bin_match(<<Digit:8, Rest/binary>>, Numbers) when (Digit > ?LOWER andalso Digit < ?UPPER) ->
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

split(Bin, Length)->
  BitLength = Length*8,
  <<First:BitLength, Second/binary>> = Bin,
  {ok, binary:encode_unsigned(First), Second}.
