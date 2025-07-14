def array_map(f):
  if length == 0 then []
  else [.[0] | f] + (.[1:] | array_map(f))
  end;

def array_reduce(f; acc):
  if length == 1 then acc  
  else .[1:] | array_reduce(f; acc + (.[0] | f))  
  end;

[1,2,3,4,5] | array_map(. * 2), array_reduce(. * 2; 0)

