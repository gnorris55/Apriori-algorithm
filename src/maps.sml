structure Dicts = 
struct

structure Key =
struct
type ord_key = string
val compare = String.compare
end

structure ItemMap = RedBlackMapFn (Key)

structure KeyPair =
struct
fun compare_string_pairs((a,b), (c,d)) = 
    case String.compare(a,c) of
        EQUAL => String.compare(b,d)
      | res =>   res

type ord_key = (string*string)
val compare = compare_string_pairs
end

structure PairMap = RedBlackMapFn (KeyPair)
end
