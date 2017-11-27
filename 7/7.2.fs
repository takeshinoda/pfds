module Hoge

type Elem<'a> = Element of 'a
type Tree<'a> = Node of int * Elem<'a> * List<Tree<'a>>
type Heap<'a> = Lazy<List<Tree<'a>>>

let empty<'a> = lazy [] : Heap<'a>
let isEmpty(ts: Heap<'a>) = ts.Force().IsEmpty
let rank(Node(r, _, _)) = r
let root(Node(_, x, _)) = x
let link((Node(r, x1, c1) as t1), (Node(_, x2, c2) as t2)) =
    if x1 <= x2 then Node(r + 1, x1, t2 :: c1)
                else Node(r + 1, x2, t1 :: c2)

