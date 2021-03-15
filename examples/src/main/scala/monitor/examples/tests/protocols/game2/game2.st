S_game2 = rec X.(+{
        !Ask().&{
            ?Yes(answer : Boolean)[answer].+{
                !Guess(num: Int)[(num > 0) and (num < 10)].&{
                    ?Correct(ans: Int)[(ans==num) and (ans >= 10)],
                    ?Incorrect().X
                },
                !Quit()
            },
            ?No(answer : Boolean)[not answer].+{
                !Guess(num: Int)[(num > 0) and (num < 10)].&{
                    ?Correct(ans: Int)[(ans==num) and (ans >= 10)],
                    ?Incorrect().X
                },
                !Quit()
            }
        }
    }
)