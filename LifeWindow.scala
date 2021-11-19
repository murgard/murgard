package life

import introprog.PixelWindow
import introprog.PixelWindow.Event
import java.awt.Color

object LifeWindow:
  val EventMaxWait = 1
  var NextGenerationDelay = 200
  val rosa = Color(242, 128, 161)
  val blockSize = 24

  val UnderPopulated = java.awt.Color.cyan
  val OverPopulated = java.awt.Color.red 
  val WillBeBorn = java.awt.Color(40, 0, 0) 
  val gridcolor = java.awt.Color(30,30,30)
  val old = Color.gray

  

class LifeWindow(rows: Int, cols: Int, isMultiColor: Boolean):
  import LifeWindow._

  var life = Life.empty(rows, cols)
  val window: PixelWindow = 
     PixelWindow(rows*blockSize,cols*blockSize,
                    "Game of Life",
                    Color.black, gridcolor)
  var quit = false
  var play = false

  def drawGrid(): Unit =
    //window.fill(0, 0, rows * blockSize + 1, cols * blockSize + 2, Color.gray)

    for i <- 0 to cols do
      window.line(0,i*blockSize, rows*blockSize,i*blockSize) // ritar linjerna
    for i <- 1 to rows do
      window.line(i*blockSize,0, i*blockSize,cols*blockSize) 

  def isIn(row: Int, col: Int): Boolean = 
    window.isInside(row, col)

        //Vanliga världen
  def drawCell(row: Int, col: Int): Unit =
    var a = Color.black
    if life(row, col) then  // Ritar rosa där apply är true
      a = rosa
    window.fill(row * blockSize + 1, col * blockSize + 1, blockSize - 1, blockSize - 1, a)
  

      //Färgglad värld
  def drawCell2(row: Int, col: Int): Unit =
    var a = Color.black
    if life.nbrOfNeighbours(row, col) < 2 && life(row, col) == true then
      a = UnderPopulated
    else if life.nbrOfNeighbours(row, col) > 3 && life(row, col) == true then
      a = OverPopulated
    else if !(life(row,col)) && life.nbrOfNeighbours(row,col)==3 then
      a = WillBeBorn
    else if life(row, col) then  // Ritar rosa där apply är true
      a = rosa 
    window.fill(row * blockSize + 1, col * blockSize + 1, blockSize - 1, blockSize - 1, a)
  
  def update(newLife: Life): Unit =
    val oldLife = life
    life = newLife
    life.cells.foreachIndex{
        (x, y) => if oldLife(x, y) != life(x, y) then 
        if !isMultiColor then drawCell(x, y) else drawCell2(x,y)  //
    }
  

  def handleKey(key: String): Unit = 
    key match
      case "r" => life = Life.random((rows, cols)); drawGrid() // Ritar random celler
      case "Enter" => play = false; update(life.evolved())// Nästa generation
      case " " => if !play then play = true else play = false
      case "Backspace" => life = Life.empty(rows, cols); drawGrid()
      case "s" => val saveText = introprog.Dialog.file("Save Life"); introprog.IO.saveString(life.toString, saveText)  
      case "p" => print(life.toString)
      case _ => println("Bad key")
      
 

  def handleClick(pos: (Int, Int)): Unit =
    val clickedAt = (pos._1 / blockSize, pos._2 / blockSize)

    println(clickedAt)
    life = life.toggled(clickedAt._1, clickedAt._2)  // Ändra tillstånd på klickat block
    drawCell(clickedAt._1, clickedAt._2)    // Rita ny


  def loopUntilQuit(): Unit = 
    while !quit do
      val t0 = System.currentTimeMillis
      if play then 
        update(life.evolved())
      window.awaitEvent(EventMaxWait)
      while window.lastEventType != PixelWindow.Event.Undefined do
        window.lastEventType match
          case Event.KeyPressed => handleKey(window.lastKey)
          case Event.MousePressed => handleClick(window.lastMousePos)
          case Event.WindowClosed => quit = true
          case _ =>
        window.awaitEvent(EventMaxWait)
      val elapsed = System.currentTimeMillis - t0
      Thread.sleep((NextGenerationDelay - elapsed) max 0)

  def start(): Unit =
    
    drawGrid()
    loopUntilQuit()