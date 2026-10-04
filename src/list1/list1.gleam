import gleam/list

pub opaque type List1(a) {
  List1(first: a, next: List(a))
}

pub fn last(lst: List1(a)) -> a {
  case list.last(lst.next) {
    Ok(v) -> {
      v
    }
    Error(_) -> {
      lst.first
    }
  }
}

pub fn first(lst: List1(a)) -> a {
  lst.first
}

pub fn new(first: a, succs: List(a)) -> List1(a) {
  List1(first, next: succs)
}

pub fn to_list(lst: List1(a)) -> List(a) {
  [lst.first, ..lst.next]
}

pub fn map(lst: List1(a), f: fn(a) -> b) -> List1(b) {
  List1(first: f(lst.first), next: list.map(lst.next, f))
}
