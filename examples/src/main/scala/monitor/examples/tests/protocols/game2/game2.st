S_game2 = rec X.(+{
        !Ask().&{
            ?Yes(answer : Boolean)[answer].+{
                !Guess(num: Int)[(num > 0) && (num < 10)].&{
                    ?Correct(ans: Int)[(ans==num) && (ans >= 10)],
                    ?Incorrect().X
                },
                !Quit()
            },
            ?No(answer : Boolean)[!answer].+{
                !Guess(num: Int)[(num > 0) && (num < 10)].&{
                    ?Correct(ans: Int)[(ans==num) && (ans >= 10)],
                    ?Incorrect().X
                },
                !Quit()
            }
        }
    }
)