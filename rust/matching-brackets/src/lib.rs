pub fn brackets_are_balanced(string: &str) -> bool {
    let mut stack = vec![];
    for parens in string.chars().filter(|c| "{[()]}".contains(*c)) {
        if "{[(".contains(parens) {
            stack.push(parens);
        } else if let Some(('(', ')')) | Some(('[', ']')) | Some(('{', '}')) =
            stack.pop().map(|opening| (opening, parens))
        {
            continue;
        } else {
            return false;
        }
    }
    stack.is_empty()
}
