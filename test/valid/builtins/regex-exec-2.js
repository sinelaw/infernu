function getGroup(regex, str, idx) { 
   return regex.exec(str)[idx]; 
}    

var result = getGroup(/bla/, 'hi', 2)
