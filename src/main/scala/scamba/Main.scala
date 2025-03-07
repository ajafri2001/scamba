import scamba.collections.{list, $}

@main def start =

    val ls = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val newList = ls where $.o % 2 == 0 as $.o + 3

    val newList_1 = ls where { $.o % 2 == 0 } as { $.o + 3 }

    val newList_2 = ls where {
        val x = 2
        $.o % x == 0
    } as {
        val x = 1
        $.o + x
    }

    println(newList_2) // [3, 5, 7, 9, 11]
