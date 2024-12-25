val count : ('a -> bool) -> 'a list -> int
val find_all_index : ('a -> bool) -> 'a list -> int list
val remove_duplicates : ('a -> 'a -> bool) -> 'a list -> 'a list
val contains_duplicates : ('a -> 'a -> bool) -> 'a list -> bool
val without_element_at : int -> 'a list -> 'a list
val par_map : ('a -> 'b) -> 'a list -> 'b list
val par_filter : ('a -> bool) -> 'a list -> 'a list
