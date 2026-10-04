import gleam/list
import gleam/result
import gleam/string

import list1/list1.{type List1}

pub type Fn(i, o) =
  fn(i) -> o

pub type TryFn(i, o, e) =
  Fn(i, Result(o, e))

// Result(output_result, remain_input, error_type)
pub type Parser(i, o, e) =
  TryFn(i, #(o, i), e)

pub fn or_p(p1, p2) -> TryFn(i, o, e) {
  fn(input) {
    use <- result.lazy_or(p1(input))
    p2(input)
  }
}

fn list_or_parser(lp, input, ecomb) -> Result(o, e) {
  case lp {
    [] -> Error(ecomb)
    [first, ..remain] ->
      case first(input) {
        Ok(v) -> Ok(v)
        Error(e) -> list_or_parser(remain, input, e)
      }
  }
}

pub fn choice_p(lp, ecomb) -> TryFn(i, o, e) {
  list_or_parser(lp, _, ecomb)
}

pub fn map_then_p(p1, p2, comb) -> Parser(i, o, e) {
  fn(input) {
    case p1(input) {
      Ok(#(v0, remain0)) ->
        case p2(remain0) {
          Ok(#(v1, remain1)) -> Ok(#(comb(v0, v1), remain1))
          Error(err) -> Error(err)
        }
      Error(err) -> Error(err)
    }
  }
}

pub fn then_p(p1, p2) -> Parser(i, #(o1, o2), e) {
  map_then_p(p1, p2, fn(l, r) { #(l, r) })
}

pub fn ignorethen_p(p1, p2) -> Parser(i, o2, e) {
  map_then_p(p1, p2, fn(_l, r) { r })
}

pub fn thenignore_p(p1, p2) -> Parser(i, o1, e) {
  map_then_p(p1, p2, fn(l, _r) { l })
}

fn map_split_while_parser(input, p) -> #(List(o), i) {
  case p(input) {
    Ok(#(out, remain)) -> {
      let #(out_list, remainremain) = map_split_while_parser(remain, p)
      #([out, ..out_list], remainremain)
    }
    Error(_e) -> #([], input)
  }
}

pub fn many_p(p) -> Parser(i, List(o), e) {
  fn(input) { map_split_while_parser(input, p) |> Ok }
}

pub fn many1_p(p) -> Parser(i, List1(o), e) {
  use #(o, r) <- map_p(then_p(p, many_p(p)))

  list1.new(o, r)
}

pub fn map_p(p, f) -> Parser(i, o, e) {
  fn(input) {
    use #(b, r) <- result.map(p(input))
    #(f(b), r)
  }
}

pub fn trymap_p(p: Parser(i, a, e), f: fn(a) -> Result(o, e)) -> Parser(i, o, e) {
  fn(input) {
    result.map(p(input), fn (dispatch) {
      let #(v, rem) = dispatch
      use v2 <- result.map(f(v))
      #(v2, rem)
    }) |> result.flatten
  }
}

pub fn foldl(init_p: Parser(i, acc, e), p: Parser(i, List(o), e), f: fn(acc, o) -> acc) -> Parser(i, acc, e) {
  fn (input) {
    result.map(init_p(input), fn (dispatch) {
      let #(acc, init_rem) = dispatch
      use #(lst, rem) <- result.map(p(init_rem))
      #(list.fold(lst, acc, f), rem)
    }) |> result.flatten
  }
}

/// example
/// ```
/// fn (dispatch) { then_p(char_p("{"), then_p(dispatch, char_p("}"))) }
/// ```
pub fn fixed_point_combinator(f) -> Fn(i, o) {
  fn(x) { f(fixed_point_combinator(f))(x) }
}

pub fn word_p(word, conb, ecomb) -> Parser(String, o, e) {
  let token_length = string.length(word)
  fn(input) {
    case string.starts_with(input, word) {
      True -> Ok(#(conb, string.drop_start(input, token_length)))
      False -> Error(ecomb)
    }
  }
}

fn list_start_with(lst1, lst2) -> Bool {
  case lst1, lst2 {
    [], [_f2, .._succs2] -> False
    [f1, ..succs1], [f2, ..succs2] -> 
      f1 == f2 && list_start_with(succs1, succs2)
    _, [] -> True
  }
}

pub fn word_with_span_p(word: List(UtfCodepoint), comb: o, ecomb: fn (Span) -> e) -> Parser(#(Int, List(UtfCodepoint)), #(Span, o), e)
{
  let token_length = list.length(word)

  fn(input) {
    let #(start_index, input_stream) = input
    case list_start_with(input_stream, word) {
      True -> {
        Ok(#(#(Span(start: start_index, end: start_index + token_length), comb), #(start_index + token_length, list.drop(input_stream, token_length) )))
      }
      False -> {
        Error(ecomb(Span(start: start_index, end: start_index + token_length)))
      }
    }
  }
}

pub fn end_p(err) -> Parser(String, Nil, e) {
  fn(input) {
    case input {
      "" -> Ok(#(Nil, ""))
      _ -> Error(err)
    }
  }
}

pub fn utf_end_p(err) -> Parser(List(UtfCodepoint), Nil, e) {
  fn(input) {
    case input {
      [] -> Ok(#(Nil, []))
      _ -> Error(err)
    }
  }
}

pub fn utf_end_with_span_p(err) -> Parser(#(Int, List(UtfCodepoint)), Nil, e) {
  fn(input) {
    let #(start, left) = input
    case left {
      [] -> Ok(#(Nil, #(start, [])))
      _ -> Error(err)
    }
  }
}

pub fn pred_char_p(
  char_list,
  ecomb,
) -> Parser(List(UtfCodepoint), UtfCodepoint, e) {
  fn(input) {
    case input {
      [] -> Error(ecomb)
      [first, ..remain] ->
        case list.contains(char_list, first) {
          True -> Ok(#(first, remain))
          False -> Error(ecomb)
        }
    }
  }
}

pub type Span {
  Span(start: Int, end: Int)
}

pub fn pred_char_with_span_p(
  char_list,
  ecomb,
) -> Parser(#(Int, List(UtfCodepoint)), #(Span, UtfCodepoint), e) {
  fn(input) {
    let #(start, left) = input
    case left {
      [] -> Error(ecomb(Span(start, end: start + 1)))
      [first, ..remain] ->
        case list.contains(char_list, first) {
          True ->
            Ok(#(#(Span(start, end: start + 1), first), #(start + 1, remain)))
          False -> Error(ecomb(Span(start, end: start + 1)))
        }
    }
  }
}

pub fn span_gather(
  lst: List1(#(Span, UtfCodepoint)),
) -> #(Span, List1(UtfCodepoint)) {
  #(
    Span(start: list1.first(lst).0.start, end: list1.last(lst).0.end),
    list1.map(lst, fn(x) { x.1 }),
  )
}
