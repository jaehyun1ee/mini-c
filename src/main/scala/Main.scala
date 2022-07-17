package miniC

object Main extends App with miniCDriver {
    println(run("input(x); y := x"))
    println(run("""
        x := 5;
        y := 1;
        while(x < 10) {
            if(y == 2) {
                y := 4
            } else {
                y := 3
            };
            input(z);
            x := x + y
        }
    """))
}
