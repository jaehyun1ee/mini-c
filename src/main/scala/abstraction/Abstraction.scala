package miniC

trait Abstraction extends miniCError {
    def +(that: Abstraction): Abstraction = (this, that) match {
        case (l: Parity, r: Parity) => l + r
        case (l: IntervalDomain, r: IntervalDomain) => l + r
        case (l: Sign, r: Sign) => l + r
        case _ => analyzeError("wrong abstraction")
    }
    def -(that: Abstraction): Abstraction = (this, that) match {
        case (l: Parity, r: Parity) => l - r
        case (l: IntervalDomain, r: IntervalDomain) => l - r
        case (l: Sign, r: Sign) => l - r
        case _ => analyzeError("wrong abstraction")
    }
    def same(that: Abstraction): Option[Boolean] = (this, that) match {
        case (l: Parity, r: Parity) => l same r
        case (l: IntervalDomain, r: IntervalDomain) => l same r
        case (l: Sign, r: Sign) => l same r
        case _ => analyzeError("wrong abstraction")
    }
    def lt(that: Abstraction): Option[Boolean] = (this, that) match {
        case (l: Parity, r: Parity) => l lt r
        case (l: IntervalDomain, r: IntervalDomain) => l lt r
        case (l: Sign, r: Sign) => l lt r
        case _ => analyzeError("wrong abstraction")
    }
    def union(that: Abstraction): Abstraction = (this, that) match {
        case (l: Parity, r: Parity) => l union r
        case (l: IntervalDomain, r: IntervalDomain) => l union r
        case (l: Sign, r: Sign) => l union r
        case _ => analyzeError("wrong abstraction")
    }
    def contain(that: Abstraction): Boolean = (this, that) match {
        case (l: Parity, r: Parity) => l contain r
        case (l: IntervalDomain, r: IntervalDomain) => l contain r
        case (l: Sign, r: Sign) => l contain r
        case _ => analyzeError("wrong abstraction")
    }
    def abstraction(n: Int): Abstraction = this match {
        case parity: Parity => parity.abstraction(n)
        case interval: IntervalDomain => interval.abstraction(n)
        case sign: Sign => sign.abstraction(n)
        case _ => analyzeError("wrong abstraction")
    }
    def bottom: Abstraction = this match {
        case parity: Parity => parity.bottom
        case interval: IntervalDomain => interval.bottom
        case sign: Sign => sign.bottom
        case _ => analyzeError("wrong abstraction")
    }
    def top: Abstraction = this match {
        case parity: Parity => parity.top
        case interval: IntervalDomain => interval.top
        case sign: Sign => sign.top
        case _ => analyzeError("wrong abstraction")
    }
}
