object TypeClass {

  trait Semigroup[T] {
    def append(x: T, y: T): T
  }

  object Semigroup {
    def apply[T: Semigroup](): Semigroup[T] = implicitly[Semigroup[T]]


    trait Op[T] {
      def typeClassInstance: Semigroup[T]

      def self: T

      def |+|(a: T): T = typeClassInstance.append(self, a)
    }

    object op {
      implicit def toSemigroup[T: Semigroup](target: T): Op[T] = new Op[T] {
        def self: T = target

        def typeClassInstance: Semigroup[T] = implicitly[Semigroup[T]]
      }
    }

  }
}
