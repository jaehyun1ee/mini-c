package miniC

// Parity Abstraction
trait Parity {
    /*
        Operator Definition
    */

    // + operator
    def +(that: Parity): Parity = (this, that) match {
        case (Bottom(), Bottom()) => Bottom()
        case (Bottom(), Even()) => Even()
        case (Bottom(), Odd()) => Odd()
        case (Even(), Bottom()) => Even()
        case (Odd(), Bottom()) => Odd()
        case (Top(), _) => Top()
        case (_, Top()) => Top()
        case (Even(), Even()) => Even()
        case (Even(), Odd()) => Odd()
        case (Odd(), Even()) => Odd()
        case (Odd(), Odd()) => Even()
    }

    // - operator
    def -(that: Parity): Parity = this + that

    // == operator
    def same(that: Parity): Option[Boolean] = (this, that) match {
        case (Even(), Odd()) => Some(false)
        case (Odd(), Even()) => Some(false)
        case _ => None
    }

    // union operator (find an enclosing parity for THIS and THAT)
    def union(that: Parity): Parity = (this, that) match {
        case (Bottom(), Bottom()) => Bottom()
        case (Bottom(), Even()) => Even()
        case (Bottom(), Odd()) => Odd()
        case (Even(), Bottom()) => Even()
        case (Odd(), Bottom()) => Odd()
        case (Top(), _) => Top()
        case (_, Top()) => Top()
        case (Even(), Even()) => Even()
        case (Odd(), Odd()) => Odd()
        case (Even(), Odd()) => Top()
        case (Odd(), Even()) => Top()
    }

    // contain operator (return true if THIS contains THAT)
    def contain(that: Parity): Boolean = (this, that) match {
        case (Top(), Top()) => true
        case (_, Top()) => false
        case (Top(), _) => true
        case (Bottom(), Bottom()) => true
        case (_, Bottom()) => true
        case (Bottom(), _) => false
        case (Even(), Even()) => true
        case (Odd(), Odd()) => true
        case _ => false
    }
}

/*
    Alternative Definition
*/

case class Bottom() extends Parity
case class Even() extends Parity
case class Odd() extends Parity
case class Top() extends Parity
