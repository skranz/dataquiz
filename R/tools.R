first.non.null = function(...) {
  args = list(...)
  for (arg in args) {
    if (!is.null(arg)) return(arg)
  }
  return(NULL)
}

format.zero.left = function(num, length=max(1,nchar(as.character(max(num))))) {
  restore.point("format.zero.left")

  char = as.character(num)
  zeros = paste0(rep("0",length),collapse="")
  char = paste0(substring(zeros, 1,length-nchar(char)), char)
  char

}
