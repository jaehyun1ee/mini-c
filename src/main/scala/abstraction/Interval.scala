package miniC

// Parity Abstraction
trait IntervalDomain extends Abstraction {
    /*
        Operator Definition
    */

    // + operator
    def +(that: IntervalDomain): IntervalDomain = (this, that) match {
        case (IntBottom(), IntBottom()) => IntBottom() // IntBottom + IntBottom case
        case (Interval(a, b), IntBottom()) => Interval(a, b) // IntBottom + _ cases
        case (IntBottom(), Interval(a, b)) => Interval(a, b)

        case (IntTop(), IntTop()) => IntTop() // IntTop + _ cases
        case (IntTop(), _) => IntTop()
        case (_, IntTop()) => IntTop()

        case (Interval(a, b), Interval(c, d)) => Interval(a+c, b+d) // Interval + Interval case
    }

    // - operator
    def -(that: IntervalDomain): IntervalDomain = (this, that) match {
        case (IntBottom(), IntBottom()) => IntBottom() // IntBottom - IntBottom case
        case (Interval(a, b), IntBottom()) => Interval(a, b) // IntBottom - _ cases
        case (IntBottom(), Interval(a, b)) => Interval(-b, -a)

        case (IntTop(), IntTop()) => IntTop() // IntTop - _ cases
        case (IntTop(), _) => IntTop()
        case (_, IntTop()) => IntTop()

        case (Interval(a, b), Interval(c, d)) => Interval(a-d, b-c) // Interval - Interval case
    }

    // == operator
    def same(that: IntervalDomain): Option[Boolean] = (this, that) match {
        case (Interval(a, b), Interval(c, d)) => {
            if(a==b && b==c && c==d) Some(true)
            else if (b < c) Some(false)
            else if (a > d) Some(false)
            else None
        }
        case _ => None
    }

    // < operator
    def lt(that: IntervalDomain): Option[Boolean] = (this, that) match {
        case (Interval(a, b), Interval(c, d)) => {
            if(b < c) Some(true)
            else if(d <= a) Some(false)
            else None
        }
        case _ => None
    }

    // union operator (find an enclosing interval for THIS and THAT)
    def union(that: IntervalDomain): IntervalDomain = (this, that) match {
        case (IntBottom(), IntBottom()) => IntBottom()
        case (IntBottom(), Interval(a, b)) => Interval(a, b)
        case (Interval(a, b), IntBottom()) => Interval(a, b)

        case (IntTop(), _) => IntTop()
        case (_, IntTop()) => IntTop()

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
    def contain(that: IntervalDomain): Boolean = (this, that) match {
        case (IntTop(), IntTop()) => true
        case (_, IntTop()) => false
        case (IntTop(), _) => true
        case (IntBottom(), IntBottom()) => true
        case (_, IntBottom()) => true
        case (IntBottom(), _) => false
        case (Interval(a, b), Interval(c, d)) => {
            if(a <= c && b >= d) true
            else false 
        }
        case _ => false
    }

    override def abstraction(n: Int):IntervalDomain = Interval(n, n)
    override def bottom:IntervalDomain = IntBottom()
    override def top:IntervalDomain = IntTop()
}

/*
    Alternative Definition
*/

case class IntBottom() extends IntervalDomain
case class Interval(lowerbound : Int, upperbound : Int) extends IntervalDomain
case class IntTop() extends IntervalDomain