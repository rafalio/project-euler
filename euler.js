
var end89 = {}
var end1 = {}

var i = 1;

var result = 0;

var howmany = 10000000;

function sq(num){
  var arr = num.toString().split("").map(function(f){
    return Math.pow(parseInt(f),2);
  })

  return arr.reduce( function(previous, current, index, array){
    return previous + current;
 })


}


function iterate(num){

  if(num == 1 || num in end1) return false;
  else if(num == 89 || num in end89) return true;
  else return iterate(sq(num));
}


for(i = 1; i < howmany; i++){
  
  if(iterate(i)){ 
    end89.i = i;
    result++;
  } else{
    end1.i = i;
  }

}

console.log(result);
