package examples.traits

class Fish extends Animal with MuteAnimal{

  def noise: Unit ={
    voice(mute)
  }

}

object Fish {
  def main(args: Array[String]): Unit = {
    val fish = new Fish()
    fish.noise
  }
}