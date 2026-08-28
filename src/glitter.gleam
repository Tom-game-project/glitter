import gleam/string

// Result(output_result, remain_input, error_type)
pub type Parser(i, o, e) = fn (i) -> Result(#(o, i), e)

pub fn or_p(p1: Parser(i, o, e), p2: Parser(i, o, e)) -> Parser(i, o, e) {
  fn (input) {
    case p1(input) {
      Ok(v) -> {
        Ok(v)
      }
      Error(_err) -> {
        case p2(input) {
          Ok(v) -> {
            Ok(v)
          }
          Error(err) -> {
            Error(err)
          }
        }
      }
    }
  }
}

pub fn map_then_p(p1: Parser(i, o1, e), p2: Parser(i, o2, e), comb: fn (o1, o2) -> o3) -> Parser(i, o3, e) {
  fn (input) {
    case p1(input) {
      Ok(#(v0, remain0)) -> {
        case p2(remain0) {
          Ok(#(v1, remain1)) -> {
            Ok(#(comb(v0, v1), remain1))
          }
          Error(err) -> {
            Error(err)
          }
        }
      }
      Error(err) -> {
        Error(err)
      }
    }
  }
}

pub fn then_p(p1: Parser(i, o1, e), p2: Parser(i, o2, e)) -> Parser(i, #(o1, o2), e) {
  map_then_p(p1, p2, fn (l, r) { #(l, r) })
}

pub fn ignorethen_p(p1: Parser(i, o1, e), p2: Parser(i, o2, e)) -> Parser(i, o2, e) {
  map_then_p(p1, p2, fn (_l, r) { r })
}

pub fn thenignore_p(p1: Parser(i, o1, e), p2: Parser(i, o2, e)) -> Parser(i, o1, e) {
  map_then_p(p1, p2, fn (l, _r) { l })
}

fn map_split_while_parser(input: i, p: Parser(i, o, e)) -> #(List(o), i) {
  case p(input) {
    Ok(#(out, remain)) -> {
      let #(out_list, remainremain) = map_split_while_parser(remain, p)
      #([out, ..out_list], remainremain)
    }
    Error(_e) -> {
      #([], input)
    }
  }
}

pub fn many_p(p: Parser(i, o, e)) -> Parser(i, List(o), e) {
  fn (input) {
    Ok(map_split_while_parser(input, p))
  }
}

pub fn map_p(p: Parser(i, o1, e), f: fn (o1) -> o2) -> Parser(i, o2, e) {
  fn (input) {
    case p(input) {
      Ok(#(out, remain)) -> {
        Ok(#(f(out), remain))
      }
      Error(e) -> {
        Error(e)
      }
    }
  }
}

/// example
/// ```
/// fn (dispatch) { then_p(char_p("{"), then_p(dispatch, char_p("}"))) }
/// ```
pub fn rec_p(f: fn(Parser(i, o, e)) -> Parser(i, o, e)) -> Parser(i, o, e) {
  fn (input) {
    f(rec_p(f))(input)
  }
}

pub fn word_p(word: String, conb: o, econb: e) -> Parser(String, o, e) {
  let token_length = string.length(word)
  fn (input) {
    case string.starts_with(input, word) {
      True -> {
        Ok(#(conb, string.drop_start(input, token_length)))
      }
      False -> {
        Error(econb)
      }
    }
  }
}

