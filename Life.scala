package life

case class Life(cells: Matrix[Boolean]):
  

  def apply(row: Int, col: Int): Boolean = 
    if (0 until cells.dim._1).contains(row) && (0 until cells.dim._2).contains(col) then
      cells(row, col)
    else false
  
    
  def updated(row: Int, col: Int, value: Boolean): Life =
    Life(cells.updated(row, col)(value))
    //copy

  def toggled(row: Int, col: Int): Life =
    updated(row, col, !apply(row, col)) // sätter row,col till motsatsen
    

  def nbrOfNeighbours(row: Int, col: Int): Int =
    var n = 0
    for (x <- (row-1) to (row+1); y <- (col-1) to (col+1)) do //kolla i omgivningen till platsen
      if !(x==row && y==col) && apply(x,y) then     //x och y får inte vara på aktuella platsen och ligga inom fönstret
        n += 1
    n


  def evolved(rule: (Int, Int, Life) => Boolean = Life.defaultRule): Life =
    var nextGeneration = Life.empty(cells.dim)                // nästa gen är en tom Life
    cells.foreachIndex { 
      (r,c) => nextGeneration = nextGeneration.updated(r, c, rule(r, c, this))    // uppdatera för varje index
    }
    nextGeneration

  override def toString =
    cells.data.map(xs => xs.map(cell => if (cell) "0" else "-").mkString("")).mkString("\n")

object Life:
  def empty(dim: (Int, Int)): Life =
    Life(Matrix(Vector.fill(dim._1,dim._2)(false)))

  def random(dim: (Int, Int)): Life =
    Life(Matrix(Vector.fill(dim._1, dim._2)(if util.Random.nextInt(2)==1 then true else false)))

  def defaultRule(row: Int, col: Int, current: Life): Boolean =
    var liv = current(row, col)
    val neighbours = current.nbrOfNeighbours(row, col)
    var = isAlive
    if liv then
      if neighbours ==2 || neighbours ==3 then true // lever vidare
    else
      if neighbours == 3 then true  // död cell återuppstår
      else false
    false

/*     if liv then
      if neighbours ==2 || neighbours ==3 then true
      else if neighbours < 2 then    
        false
      else 
        LifeWindow.OverPopulated
        false
    else 
      if neighbours == 3 then 
        true
      else false
*/