function getGroup(regex, str, idx) { 
   return regex.exec(str)[idx]; 
}    

var myRegex = { exec: function(s) { return [123]; } };
var result = getGroup(myRegex, 'hi', 2);

