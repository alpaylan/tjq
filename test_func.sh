

#./target/debug/tjq --expression="  . |  (def deneme(a):  map(.+1) ; deneme | .[] | . *2 | [.]  | deneme) | map(.+1) | " --input='[10]'
./target/debug/tjq --expression="def double: . * 2; def triple: . * 3; double | triple" --input='5'


jq 'def asd:  .[] ;  . | asd ' <<< "[4]" 
jq 'def asd:  .[] ;  . | asd) ' <<< "[4]"
jq '(def asd:  .[] ;  . | asd) ' <<< "[4]"

