package object sorting {

    val random = new scala.util.Random

    def randomInts(n: Int) =
        Stream.continually(random.nextInt % 50).take(n)

}
