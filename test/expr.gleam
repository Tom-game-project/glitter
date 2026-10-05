import gleam/list
import gleam/io
import gleam/string
import glitter/glitter.{
  type Span, Span, choice_p, fixed_point_combinator,
  ignorethen_p, many1_p, many_p, map_p, pred_char_p,
  pred_char_with_span_p, span_gather, then_p, thenignore_p, utf_end_p,
  utf_end_with_span_p, word_with_span_p, trymap_p, foldl, or_p, separated_by
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
  Unary(#(Span, UnaryOpe), UntypedExpr)
  Num(Span, String)
  Word(Span, String)
  Call(Span, #(Span, String), List(UntypedExpr))
}

fn untyped_expr_to_string(depth: Int, ast: UntypedExpr) -> String {
  case ast {
    Paren(span, inner_ast) -> 
      string.repeat("  ", times: depth) <> "Paren\n" <>
      untyped_expr_to_string(depth + 1, inner_ast)

    Bin(#(span, ope), expr1, expr2) ->
      string.repeat("  ", times: depth) <> case ope {
        Add -> "Add"
        Sub -> "Sub"
        Div -> "Div"
        Mul -> "Mul"
        Pow -> "Pow"
      } <> "\n" <>
      untyped_expr_to_string(depth+1, expr1) <>
      untyped_expr_to_string(depth+1, expr2)

    Unary(#(span, ope), expr) -> 
      string.repeat("  ", times: depth) <> 
      case ope { Minus -> "Minus" Plus -> "Plus" Point -> "Point" } <> "\n" <>
      
      untyped_expr_to_string(depth+1, expr)
    
    Num(span, num_string) -> 
      string.repeat("  ", times: depth) <> "Num:" <> num_string <> "\n"

    Word(span, word) -> 
      string.repeat("  ", times: depth) <> "Word:" <> word <> "\n"

    Call(span, #(_, func_name), args) -> 
      string.repeat("  ", times: depth)<> "Func:" <> func_name <> "\n"
      <> 
      string.join(list.map(args, fn (in) { untyped_expr_to_string(depth + 1, in) }), "")
  }
}

pub fn normal_expr_test() -> Nil {
  let open_paren_c = 
    pred_char_with_span_p(string.to_utf_codepoints("("), fn(_s) { CharNotFound })
  let close_paren_c = 
    pred_char_with_span_p(string.to_utf_codepoints(")"), fn(_s) { CharNotFound })
  let comma_c = 
    pred_char_with_span_p(string.to_utf_codepoints(","), fn(_s) { CharNotFound })

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


  let word_parser_orig =
    pred_char_with_span_p(string.to_utf_codepoints("abcdefghijklmnopqrstuvwxy"), fn (_s) { CharNotFound })
    |> many1_p
    |> map_p(fn(inner) {
      let #(span, utf_codepoints) = span_gather(inner)

      #(
        span,
        utf_codepoints |> list1.to_list |> string.from_utf_codepoints,
      )
    })

  let word_parser = word_parser_orig |> map_p(fn (in) { Word(in.0, in.1)})

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
        let call = {
          word_parser_orig
            |> then_p(open_paren_c)
            |> then_p(separated_by(expr, comma_c))
            |> then_p(close_paren_c)
            |> map_p(fn (in) {
              let #(#(#(word, _open_c), arg_list), close_c) = in
              Call(Span(start: word.0.start, end: close_c.0.end), word, arg_list)
            })
        }

        let ident = number_parser 
        |> or_p(call)
        |> or_p(word_parser)
        |> or_p(paren_p)

        let unary = {binop_p |> trymap_p(
          fn (in) {
            case string.utf_codepoint_to_int(in.1) {
              0x2b -> Ok(#(in.0, Plus))
              0x2d -> Ok(#(in.0, Minus))
              _ -> Error(InvalidOperator)
            }
          }
        )} |> then_p(ident) |> map_p(fn (i) {
          let #(ope, a) = i
          Unary(ope, a)
        }) |> or_p(ident)

        let product = unary |> foldl({ binop_p |> trymap_p(
          fn (in) {
            case string.utf_codepoint_to_int(in.1) {
              0x2a -> Ok(#(in.0, Mul))
              0x2f -> Ok(#(in.0, Div))
              _ -> Error(InvalidOperator)
            }
          })
          |> then_p(unary)
        } |> many_p, fn (acc, o) {
          Bin(o.0, acc, o.1)
        })

        let sum = product |> foldl({ binop_p |> trymap_p(
          fn (in) {
            case string.utf_codepoint_to_int(in.1) {
              0x2b -> Ok(#(in.0, Add))
              0x2d -> Ok(#(in.0, Sub))
              _ -> Error(InvalidOperator)
            }
          }) 
          |> then_p(product)
        } |> many_p, fn (acc, o) {
          Bin(o.0, acc, o.1)
        })

        sum
      },
      number_parser,
      word_parser,
      paren_p
    ]
    |> choice_p(OtherwiseErr)
  }
  |> thenignore_p(utf_end_with_span_p(EndErr))

  let str = "123+42*333+(x+1)+f(x,y+1)"
  // let str = "-1+-1"

  case expr_parser(#(0, str |> string.to_utf_codepoints)) {
    Ok(#(v, remain)) -> {
      // echo str
      // echo v
      io.println(untyped_expr_to_string(0, v))
      Nil
    }
    Error(err) -> {
      echo err
      io.println("failed to parser")
    }
  }
  Nil
}
