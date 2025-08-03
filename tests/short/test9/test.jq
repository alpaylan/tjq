
def array_add:
  if length ==0 then 0 else .[0] + (.[1:] | array_add) end;

. | array_add
#recursive add func

