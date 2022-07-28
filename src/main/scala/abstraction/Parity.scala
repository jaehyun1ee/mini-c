package miniC

// Parity Abstraction
trait Parity extends Abstraction {
    /*
        Operator Definition
    */

    // + operator
    def +(that: Abstraction): Abstraction = (this, that) match {
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
    def -(that: Abstraction): Abstraction = this + that

    // == operator
    def same(that: Abstraction): Option[Boolean] = (this, that) match {
        case (Even(), Odd()) => Some(false)
        case (Odd(), Even()) => Some(false)
        case _ => None
    }

    // < operator
    def lt(that: Abstraction): Option[Boolean] = None

    // union operator (find an enclosing parity for THIS and THAT)
    def union(that: Abstraction): Abstraction = (this, that) match {
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
    def contain(that: Abstraction): Boolean = (this, that) match {
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

    override def abstraction(n: Int):Abstraction = if(n % 2 == 0) Even() else Odd()
    override def bottom:Abstraction = Bottom()
    override def top:Abstraction = Top()
}

/*
    Alternative Definition
*/

case class Bottom() extends Parity
case class Even() extends Parity
case class Odd() extends Parity
case class Top() extends Parity
