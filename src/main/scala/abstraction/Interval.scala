package miniC

// Parity Abstraction
trait Interval {
    /*
        Operator Definition
    */

    // + operator
    def +(that: Interval): Interval = (this, that) match {
        case (Bottom(), Bottom()) => Bottom() // Bottom + Bottom case
        case (Interval(a, b), Bottom()) => Interval(a, b) // Bottom + _ cases
        case (Bottom(), Interval(a, b)) => Interval(a, b)

        case (Top(), Top()) => Top() // Top + _ cases
        case (Top(), _) => Top()
        case (_, Top()) => Top()

        case (Interval(a, b), Interval(c, d)) => Interval(a+c, b+d) // Interval + Interval case
    }

    // - operator
    def -(that: Parity): Parity = (this, that) match {
        case (Bottom(), Bottom()) => Bottom() // Bottom - Bottom case
        case (Interval(a, b), Bottom()) => Interval(a, b) // Bottom - _ cases
        case (Bottom(), Interval(a, b)) => Interval(-b, -a)

        case (Top(), Top()) => Top() // Top - _ cases
        case (Top(), _) => Top()
        case (_, Top()) => Top()

        case (Interval(a, b), Interval(c, d)) => Interval(a-d, b-c) // Interval - Interval case
    }

    // == operator
    def same(that: Parity): Option[Boolean] = (this, that) match {
        case (Interval(a, b), Interval(c, d)) => {
            if(a==c && b==d) Some(true)
            else Some(false)
        }
        case _ => None
    }

    // union operator (find an enclosing parity for THIS and THAT)
    def union(that: Parity): Parity = (this, that) match {
        case (Bottom(), Bottom()) => Bottom()
        case (Bottom(), Interval(a, b)) => Interval(a, b)
        case (Interval(a, b), Bottom()) => Interval(a, b)

        case (Top(), _) => Top()
        case (_, Top()) => Top()

        case (Interval(a, b), Interval(c, d)) => {
            if(a < c){
                if(b < d){
                    Interval(a, d)
                } else {
                    Interval(a, b)
                }
            } else {
                if(b < d){
                    Interval(c, d)
                } else {
                    Interval(c, b)
                }
            }
        }
    }

    // contain operator (return true if THIS contains THAT)
    def contain(that: Parity): Boolean = (this, that) match {
        case (Top(), Top()) => true
        case (_, Top()) => false
        case (Top(), _) => true
        case (Bottom(), Bottom()) => true
        case (_, Bottom()) => true
        case (Bottom(), _) => false
        case (Interval(a, b), Interval(c, d)) => {
            if(a <= c && b >= d) true
            else false 
        }
        case _ => false
    }
}

/*
    Alternative Definition
*/

case class Bottom() extends Interval
case class Interval(lowerbound : Int, upperbound : Int) extends Interval
case class Top() extends Interval
