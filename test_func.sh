

./target/debug/tjq --expression="  . |  (def deneme(a):  map(.+1) ; deneme | .[] | . *2 | [.]  | deneme) | map(.+1)  " --input='[10]'