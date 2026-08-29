import gleam/list
import gleam/result
import gleam/string

// Result(output_result, remain_input, error_type)
pub type Parser(i, o, e) =
  fn(i) -> Result(#(o, i), e)

pub fn or_p(p1, p2) -> Parser(i, o, e) {
  fn(input) {
    use <- result.lazy_or(p1(input))
    p2(input)
  }
}

fn list_or_parser(lp, input, ecomb) -> Result(#(o, i), e) {
  case lp {
    [] -> Error(ecomb)
    [first, ..remain] ->
      case first(input) {
        Ok(v) -> Ok(v)
        Error(e) -> list_or_parser(remain, input, e)
      }
  }
}

pub fn choice_p(lp, ecomb) -> Parser(i, o, e) {
  fn(input) { list_or_parser(lp, input, ecomb) }
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

pub fn map_p(p, f) -> Parser(i, o, e) {
  fn(input) {
    use #(b, r) <- result.map(p(input))
    #(f(b), r)
  }
}

/// example
/// ```
/// fn (dispatch) { then_p(char_p("{"), then_p(dispatch, char_p("}"))) }
/// ```
pub fn rec_p(f) -> Parser(i, o, e) {
  fn(input) { f(rec_p(f))(input) }
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

pub fn end_p(err) -> Parser(String, Nil, e) {
  fn(input) {
    case input {
      "" -> Ok(#(Nil, ""))
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
