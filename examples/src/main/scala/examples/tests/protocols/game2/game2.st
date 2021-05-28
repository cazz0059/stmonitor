S_game2 = rec X.(+{
        !Ask().&{
            ?Yes(answer : Boolean)[answer].+{
                !Guess1(num: Int)[(num > 0) && (num < 10)].&{
                    ?Correct1(ans: Int)[(ans==num) && (ans >= 10)],
                    ?Incorrect1().X
                },
                !Quit()
            },
            ?No(answer : Boolean)[!answer].+{
                !Guess2(num: Int)[(num > 0) && (num < 10)].&{
                    ?Correct2(ans: Int)[(ans==num) && (ans >= 10)],
                    ?Incorrect2().X
                },
                !Quit()
            }
        }
    }
)