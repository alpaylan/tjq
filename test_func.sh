

#./target/debug/tjq --expression="  . |  (def deneme(a):  map(.+1) ; deneme | .[] | . *2 | [.]  | deneme) | map(.+1) | " --input='[10]'
./target/debug/tjq --expression="def double: . * 2; def triple: . * 3; double | triple" --input='5'
./target/debug/tjq --expression="[while(.<100; .*2)]" --input='1'
./target/debug/tjq --expression=".[2] --input='[1,2,3]'


jq 'def asd:  .[] ;  . | asd ' <<< "[4]" 
jq 'def asd:  .[] ;  . | asd) ' <<< "[4]"
jq '(def asd:  .[] ;  . | asd) ' <<< "[4]"

