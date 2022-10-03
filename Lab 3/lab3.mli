(* Interface file for lab3.ml *)

val last_sublist : 'a list -> 'a list
val reverse : 'a list -> 'a list
val square_list : int list -> int list
val square_list2 : int list -> int list
val count_negative_numbers : int list -> int
val power_of_two_list : int -> int list
val prefix_sum : int list -> int list
val deep_reverse : 'a list list -> 'a list list
type 'a nested_list = Value of 'a | List of 'a nested_list list
val deep_reverse_nested : 'a nested_list -> 'a nested_list

val quicksort : 'a list -> ('a -> 'a -> bool) -> 'a list
val insertion_sort : 'a list -> ('a -> 'a -> bool) -> 'a list

val subsets : 'a list -> 'a list list
val accumulate : ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
val map : ('a -> 'b) -> 'a list -> 'b list
val append : 'a list -> 'a list -> 'a list
val length : 'a list -> int
val accumulate_n : ('a -> 'b -> 'b) -> 'b -> 'a list list -> 'b list
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val dot_product : int list -> int list -> int
val matrix_times_vector : int list list -> int list -> int list
val transpose : 'a list list -> 'a list list
val matrix_times_matrix : int list list -> int list list -> int list list

