. 
| gsub("emoji[0-9]+"; "")  | (. | test("chatbot"; "i")) and  (. | (match("chatbot"; "i")? | .offset == 0))
