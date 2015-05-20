# Helper functions that allow string arguments for  dplyr's data modification functions like arrange, select etc. 
# Author: Sebastian Kranz (https://gist.github.com/skranz/9681509)

# Modified version of dplyr's filter that uses string arguments

s_filter = function(.data, ...) {
  eval.string.dplyr(.data,"filter", ...)
}

# Modified version of dplyr's select that uses string arguments

s_select = function(.data, ...) {
  eval.string.dplyr(.data,"select", ...)
}

# Modified version of dplyr's arrange that uses string arguments

s_arrange = function(.data, ...) {
  eval.string.dplyr(.data,"arrange", ...)
}

# Modified version of dplyr's arrange that uses string arguments

s_mutate = function(.data, ...) {
  eval.string.dplyr(.data,"mutate", ...)
}

# Modified version of dplyr's summarise that uses string arguments

s_summarise = function(.data, ...) {
  eval.string.dplyr(.data,"dplyr::summarise", ...)
}

# Modified version of dplyr's group_by that uses string arguments

s_group_by = function(.data, ...) {
  eval.string.dplyr(.data,"group_by", ...)
}

# Internal function used by s_filter, s_select etc.
eval.string.dplyr = function(.data, .fun.name, ...) {
  args = list(...)
  args = unlist(args)
  code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
  df = eval(parse(text=code,srcfile=NULL))
  df  
}
