import gleam/io
import gleam/list
import gleam/string
import gleeunit
import glitter.{
  type Parser, ignorethen_p, many_p, map_p, map_then_p, or_p, rec_p,
  thenignore_p, word_p,
}

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = "Hello, " <> name <> "!"

  assert greeting == "Hello, Joe!"
}

pub type Node {
  Pre(String)
  A
  B
}

pub type ParseError {
  ExpectedA
  ExpectedB
  Fail
}

pub fn or_p_test() {
  let str = "Ahello world"

  // let #(c, remain) = #(string.first(str), string.drop_start(str, 1))
  // io.println("char: " <> c)
  let p1: Parser(String, Node, ParseError) = word_p("A", A, ExpectedA)
  let p2: Parser(String, Node, ParseError) = word_p("B", B, ExpectedB)

  let n_p = or_p(p1, p2)

  case n_p(str) {
    Ok(#(v, remain)) -> {
      io.println("Success to parse")
      case v {
        Pre(pre) -> {
          io.println("pre: " <> pre)
        }
        A -> {
          io.println("pre: A")
        }
        B -> {
          io.println("pre: B")
        }
      }
      io.println("remain: " <> remain)
    }
    Error(_e) -> {
      io.println("Failed to Parse")
    }
  }
}

pub fn then_p_test() {
  //let str = "letmuthello world"
  let str = "xxxyyyletmuthello world"

  // let #(c, remain) = #(string.first(str), string.drop_start(str, 1))
  // io.println("char: " <> c)
  let p1: Parser(String, String, ParseError) = word_p("let", "let", ExpectedA)
  let p2: Parser(String, String, ParseError) = word_p("mut", "mut", ExpectedB)

  let p3: Parser(String, String, ParseError) = word_p("xxx", "xxx", ExpectedA)
  let p4: Parser(String, String, ParseError) = word_p("yyy", "yyy", ExpectedB)

  let n_p1 = map_then_p(p1, p2, fn(a, b) { Pre(a <> b) })
  let n_p2 = map_then_p(p3, p4, fn(a, b) { Pre(a <> b) })

  let n_p = or_p(n_p1, n_p2)
  let m_p = many_p(n_p)

  case m_p(str) {
    Ok(#(v, remain)) -> {
      io.println("Success to parse")
      print_list(v, fn(a) {
        case a {
          Pre(pre) -> {
            "pre: " <> pre
          }
          A -> {
            "pre: A"
          }
          B -> {
            "pre: B"
          }
        }
      })
      io.println("remain: " <> remain)
    }
    Error(_e) -> {
      io.println("Failed to Parse")
    }
  }
}

fn print_list(lst: List(a), stringify: fn(a) -> String) -> Nil {
  lst
  |> list.map(stringify)
  |> string.join(", ")
  |> io.println
}

pub type Paren {
  Paren(List(Paren))
  AtomA
  AtomB
}

fn print_tree(paren: Paren, depth: Int) -> Nil {
  let space = string.repeat(" ", depth * 4)
  case paren {
    Paren(parens) -> {
      io.println(space <> "{")
      list.map(parens, print_tree(_, depth + 1))
      io.println(space <> "}")
    }
    AtomA -> {
      io.println(space <> "A")
    }
    AtomB -> {
      io.println(space <> "B")
    }
  }
}

fn print_tree_list(parens: List(Paren)) -> Nil {
  list.map(parens, print_tree(_, 0))
  Nil
}

pub fn rec_test() {
  // let str = "{{A}{B}}"
  let str = "{{A}{ABA}}"

  let a_p = word_p("A", AtomA, ExpectedA)
  let b_p = word_p("B", AtomA, ExpectedB)

  let open_c = word_p("{", AtomA, ExpectedA)
  let close_c = word_p("}", AtomA, ExpectedA)

  let p =
    rec_p(fn(dispatch) {
      many_p(or_p(
        a_p,
        or_p(
          b_p,
          ignorethen_p(
            open_c,
            thenignore_p(map_p(dispatch, fn(inner) { Paren(inner) }), close_c),
          ),
        ),
      ))
    })

  case p(str) {
    Ok(#(v, remain)) -> {
      io.println("")
      print_tree_list(v)
      io.println("remain \"" <> remain <> "\"")
      io.println("success to parse")
    }
    Error(_e) -> {
      io.println("failed to parse")
    }
  }
}
