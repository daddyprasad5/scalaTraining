trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = ???
}

def foldLeft[S](z: S)(f: (S, T) => S): S = {
  var result = z

  result
}