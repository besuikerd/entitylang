package org.metaborg.entitylang.util.profiler

class Profiler {

  private var initialTime: Long = -1
  private var lastTick: Long = -1

  def start() = {
    initialTime = System.currentTimeMillis()
  }

  def get(): Long = {
    val time = System.currentTimeMillis()
    if(lastTick == -1)
      time - initialTime
    else
      time - lastTick
  }

  def total(): Long = {
    val time = System.currentTimeMillis()
    time - initialTime
  }


  def tick() = {
    val time = System.currentTimeMillis()
    lastTick = time
  }


  def getAndTick(): Long = {
    val t = get()
    tick()
    t
  }

  def reset() = {
    initialTime = -1
    lastTick = -1
  }

  def restart() = {
    reset()
    start()
  }

}

object Profiler{
  def apply(): Profiler = new Profiler()
}
