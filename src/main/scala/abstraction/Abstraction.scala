package miniC

trait Abstraction {
    def +(that: Abstraction): Abstraction
    def -(that: Abstraction): Abstraction
    def same(that: Abstraction): Option[Boolean]
    def lt(that: Abstraction): Option[Boolean]
    def union(that: Abstraction): Abstraction
    def contain(that: Abstraction): Boolean
    def abstraction(n: Int): Abstraction
    def bottom: Abstraction
    def top: Abstraction
}
