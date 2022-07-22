package miniC

// Parity Abstraction
trait Sign {
    /*
        Operator Definition
    */

    // + operator
    def +(that: Sign): Sign = (this, that) match {
        case (SignBottom(), SignBottom()) => SignBottom()

        case (SignBottom(), Positive()) => Positive()
        case (SignBottom(), Zero()) => Zero()
        case (SignBottom(), Negative()) => Negative()
        case (SignBottom(), Nonpositive()) => Nonpositive()
        case (SignBottom(), Nonzero()) => Nonzero()
        case (SignBottom(), Nonnegative()) => Nonnegative()

        case (Positive(), SignBottom()) => Positive()
        case (Zero(), SignBottom()) => Zero()
        case (Negative(), SignBottom()) => Negative()
        case (Nonpositive(), SignBottom()) => Nonpositive()
        case (Nonzero(), SignBottom()) => Nonzero()
        case (Nonnegative(), SignBottom()) => Nonnegative()

        case (SignTop(), _) => SignTop()
        case (_, SignTop()) => SignTop()

        case (Positive(), Positive()) => Positive()
        case (Positive(), Zero()) => Positive()
        case (Positive(), Negative()) => SignTop()
        case (Positive(), Nonpositive()) => SignTop()
        case (Positive(), Nonzero()) => SignTop()
        case (Positive(), Nonnegative()) => Positive()

        case (Zero(), Positive()) => Positive()
        case (Zero(), Zero()) => Zero()
        case (Zero(), Negative()) => Negative()
        case (Zero(), Nonpositive()) => Nonpositive()
        case (Zero(), Nonzero()) => Nonzero()
        case (Zero(), Nonnegative()) => Nonnegative()

        case (Negative(), Positive()) => SignTop()
        case (Negative(), Zero()) => Negative()
        case (Negative(), Negative()) => Negative()
        case (Negative(), Nonpositive()) => Negative()
        case (Negative(), Nonzero()) => SignTop()
        case (Negative(), Nonnegative()) => SignTop()

        case (Nonpositive(), Positive()) => SignTop()
        case (Nonpositive(), Zero()) => Nonpositive()
        case (Nonpositive(), Negative()) => Negative()
        case (Nonpositive(), Nonpositive()) => Nonpositive()
        case (Nonpositive(), Nonzero()) => SignTop()
        case (Nonpositive(), Nonnegative()) => SignTop()

        case (Nonzero(), Positive()) => SignTop()
        case (Nonzero(), Zero()) => Nonzero()
        case (Nonzero(), Negative()) => SignTop()
        case (Nonzero(), Nonpositive()) => SignTop()
        case (Nonzero(), Nonzero()) => SignTop()
        case (Nonzero(), Nonnegative()) => SignTop()

        case (Nonnegative(), Positive()) => Positive()
        case (Nonnegative(), Zero()) => Nonnegative()
        case (Nonnegative(), Negative()) => SignTop()
        case (Nonnegative(), Nonpositive()) => SignTop()
        case (Nonnegative(), Nonzero()) => SignTop()
        case (Nonnegative(), Nonnegative()) => Nonnegative()
    }

    // - operator
    def -(that: Sign): Sign = this + (that match {
        case Positive() => Negative()
        case Negative() => Positive()
        case Nonpositive() => Nonnegative()
        case Nonnegative() => Nonpositive()
        case _ => that
    })

    // == operator
    def same(that: Sign): Option[Boolean] = (this, that) match {
        case (Positive(), Zero()) => Some(false)
        case (Positive(), Negative()) => Some(false)
        case (Positive(), Nonpositive()) => Some(false)

        case (Zero(), Positive()) => Some(false)
        case (Zero(), Zero()) => Some(true)
        case (Zero(), Negative()) => Some(false)
        case (Zero(), Nonzero()) => Some(false)

        case (Negative(), Positive()) => Some(false)
        case (Negative(), Zero()) => Some(false)
        case (Negative(), Nonnegative()) => Some(false)

        case (Nonpositive(), Positive()) => Some(false)

        case (Nonzero(), Zero()) => Some(false)

        case (Nonnegative(), Negative()) => Some(false)

        case _ => None
    }

    def lt(that: Sign): Option[Boolean] = (this, that) match {
        case (Positive(), Zero()) => Some(false)
        case (Positive(), Negative()) => Some(false)
        case (Positive(), Nonpositive()) => Some(false)

        case (Zero(), Positive()) => Some(true)
        case (Zero(), Zero()) => Some(false)
        case (Zero(), Negative()) => Some(false)
        case (Zero(), Nonpositive()) => Some(false)

        case (Negative(), Positive()) => Some(true)
        case (Negative(), Zero()) => Some(true)
        case (Negative(), Nonnegative()) => Some(true)

        case (Nonpositive(), Positive()) => Some(true)

        case (Nonnegative(), Zero()) => Some(false)
        case (Nonnegative(), Negative()) => Some(false)
        case (Nonnegative(), Nonpositive()) => Some(false)

        case _ => None
    }

    // union operator (find an enclosing parity for THIS and THAT)
    def union(that: Sign): Sign = {
        val thisbools = this match {
            case SignBottom() => (false, false, false)
            case Positive() => (false, false, true)
            case Zero() => (false, true, false)
            case Negative() => (true, false, false)
            case Nonpositive() => (true, true, false)
            case Nonzero() => (true, false, true)
            case Nonnegative() => (false, true, true)
            case SignTop() => (true, true, true)
        }
        val thatbools = that match {
            case SignBottom() => (false, false, false)
            case Positive() => (false, false, true)
            case Zero() => (false, true, false)
            case Negative() => (true, false, false)
            case Nonpositive() => (true, true, false)
            case Nonzero() => (true, false, true)
            case Nonnegative() => (false, true, true)
            case SignTop() => (true, true, true)
        }
        (thisbools._1 || thatbools._1, thisbools._2 || thatbools._2, thisbools._3 || thatbools._3) match {
            case (false, false, false) => SignBottom()
            case (false, false, true) => Positive()
            case (false, true, false) => Zero()
            case (true, false, false) => Negative()
            case (true, true, false) => Nonpositive()
            case (true, false, true) => Nonzero()
            case (false, true, true) => Nonnegative()
            case (true, true, true) => SignTop()
        }
    }

    // contain operator (return true if THIS contains THAT)
    def contain(that: Sign): Boolean = {
        val thisbools = this match {
            case SignBottom() => (false, false, false)
            case Positive() => (false, false, true)
            case Zero() => (false, true, false)
            case Negative() => (true, false, false)
            case Nonpositive() => (true, true, false)
            case Nonzero() => (true, false, true)
            case Nonnegative() => (false, true, true)
            case SignTop() => (true, true, true)
        }
        val thatbools = that match {
            case SignBottom() => (false, false, false)
            case Positive() => (false, false, true)
            case Zero() => (false, true, false)
            case Negative() => (true, false, false)
            case Nonpositive() => (true, true, false)
            case Nonzero() => (true, false, true)
            case Nonnegative() => (false, true, true)
            case SignTop() => (true, true, true)
        }
        (!thisbools._1 || thatbools._1) && (!thisbools._3 || thatbools._3) && (!thisbools._3 || thatbools._3)
    }

    def abstraction(n: Int) = if (n > 0) Positive() else if (n == 0) Zero() else Negative()
    def bottom:Sign = SignBottom()
    def top:Sign = SignTop()
}

/*
    Alternative Definition
*/

case class SignBottom() extends Sign
case class Positive() extends Sign
case class Zero() extends Sign
case class Negative() extends Sign
case class Nonpositive() extends Sign
case class Nonzero() extends Sign
case class Nonnegative() extends Sign
case class SignTop() extends Sign
