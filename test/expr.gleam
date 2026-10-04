import gleam/io
import gleam/string
import glitter/glitter.{
  type Span, Span, choice_p, fixed_point_combinator,
  ignorethen_p, many1_p, many_p, map_p, pred_char_p,
  pred_char_with_span_p, span_gather, then_p, thenignore_p, utf_end_p,
  utf_end_with_span_p, word_with_span_p, trymap_p, foldl, or_p
}
import list1/list1

pub type ParseErr {
  EndErr
  ParenErr
  CharNotFound
  WordErr

  OtherwiseErr
  InvalidOperator
}

pub type BinOpe {
  Add
  Mul
  Sub
  Div
  Pow
}

pub type UnaryOpe {
  Plus  // +expr
  Minus // -expr
  Point // *expr
}

pub type UntypedExpr {
  Paren(Span, UntypedExpr)
  Bin(#(Span, BinOpe), UntypedExpr, UntypedExpr)
  Unary(UnaryOpe, UntypedExpr)
  Num(Span, String)
  Word(String)
}

pub fn normal_expr_test() -> Nil {
  let open_paren_c = 
    pred_char_with_span_p(string.to_utf_codepoints("("), fn(_s) { CharNotFound })
  let close_paren_c = 
    pred_char_with_span_p(string.to_utf_codepoints(")"), fn(_s) { CharNotFound })

  let number_parser =
    pred_char_with_span_p(string.to_utf_codepoints("1234567890"), fn (_s) { CharNotFound })
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)

      Num(
        span,
        utf_codepoints |> list1.to_list |> string.from_utf_codepoints,
      )
    })

  let binop_p = 
    pred_char_with_span_p(string.to_utf_codepoints("+-*/"), fn(_s) { CharNotFound })

  let expr_parser = {
    use expr <- fixed_point_combinator

    let paren_p = {
        use #(#(#(open_span, _), inner_paren), #(close_span, _)) <- map_p(
          open_paren_c
          |> then_p(expr)
          |> then_p(close_paren_c),
        )
        Paren(
          Span(start: open_span.start, end: close_span.end),
          inner_paren,
        )
      }

    [
      {
        let ident = number_parser 
        |> or_p(
          paren_p
        )
        |> or_p(expr)

        foldl(ident, { binop_p |> trymap_p(
          fn (in) {
            case string.utf_codepoint_to_int(in.1) {
              0x2b -> Ok(#(in.0, Add))
              _ -> Error(InvalidOperator)
            }
          }) 
          |> then_p(ident)
        } |> many_p, fn (acc, o) {
          Bin(o.0, acc, o.1)
        })
      },
      number_parser,
      paren_p
      // paren
    ]
    |> choice_p(OtherwiseErr)
  }
  |> thenignore_p(utf_end_with_span_p(EndErr))

  let str = "123+2+333"

  case expr_parser(#(0, str |> string.to_utf_codepoints)) {
    Ok(#(v, remain)) -> {
      echo str
      echo v
      //io.println("remain :\"" <> remain |> string.from_utf_codepoints <> "\"")
      Nil
    }
    Error(_err) -> {
      io.println("failed to parser")
    }
  }
  Nil
}
