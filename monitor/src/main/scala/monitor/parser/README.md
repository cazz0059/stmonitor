// ALL STATEMENTS HERE ARE BASIC, limitations include:
//   no arrays ADD ARRAYS
//   no scala functions
//   no for loops
//   no if-then-else
//   variable naming conventions to avoid nondeterminism/reassignments/confusion in assertions
//       try and name variables in functions and params different from global as much as possible
//       if naming them the same make sure the parameters match the arguments and not get mixed up
// make something that if it cant parse it just return unknown
// dont break the whole system if the util function cannot be parsed
// so still parse the util functions in the beginning, just skip branches that have util functions because we dont know how to parse them
// dont parse util functions in the beginning (while loop needs previous conditions), just parse when accessed.

// RESTRICT NAMING CONDITIONS TO AS MUCH AS POSSIBLE NOT USE "_n"

    // REASONS WHY NOT OPTIMISED
    // doe snot support recursion
    // integers cannot be interpreted as range
    // while loops need previous conditions, or else almost always sat
