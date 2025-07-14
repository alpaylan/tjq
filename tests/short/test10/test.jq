def array_reverse:
  if length ==0 then [] else [.[-1]] + (.[0:length-1] | array_reverse) end ;
. | array_reverse