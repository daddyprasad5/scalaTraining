def classifiedColumns(columnNames: List[String]): (List[(String, String)], List[(String, String)], List[(String, String)]) = {

  List("t01", "t03", "t11", "t1801", "t1803"),
  List("t05", "t1805"),
  List("t02", "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16", "t18")

  val primary = List("t01", "t03", "t11", "t1801", "t1803")
  val working = List("t05", "t1805")
  val other = List("t02", "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16", "t18")

  val pCols = for {
    cname <- columnNames
    p <- primary
    if cname.startsWith(p)
  } yield (p, "primaryNeeds")

  val wCols = for {
    cname <- columnNames
    w <- working
    if cname.startsWith(w)
  } yield (w, "working")

  val oCols = for {
    cname <- columnNames
    o <- other
    if cname.startsWith(o)
    if !cname.startsWith("t1801")
    if !cname.startsWith("t1803")
    if !cname.startsWith("t1805")
  } yield (o, "other")

  (pCols, wCols, oCols)

}

val colNames = List("t1112","t180101", "t0102", "t0301", t010101, t180301)

val c = classifiedColumns(colNames)

println(c)






