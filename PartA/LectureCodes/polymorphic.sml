datatype 'a option = NONE | SOME of 'a
datatype 'a my_list = Empty | Con of 'a * 'a my_list

datatype ('a, 'b) tree = Node of 'a * ('a, 'b) tree * ('a, ('a, 'b) tree | Leaf of 'b