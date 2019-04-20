

object Main {

case class Key(freq:Int,ch:Char) extends Ordered[Key]{  
	def compare(that:Key)=freq.compare(that.freq)
}

def main(args:Array[String]){

	val str="AAAAAAAAKKKKMMMT".toLowerCase();
	//	for(inc <-0 until str.length()){
	for(inc <-0 until str.length()){
		loopPositions(str)(inc)
	}
}


def loopPositions(str:String)={
		(incr:Int)=>
		
		reArrangeChars(str,incr)
		
}



def reArrangeChars(str:String,incr:Int){
	val len:Int=str.length();

val count:Array[Int]=new Array[Int](26);

for(i<-0 to len-1){

	count(str.charAt(i)-'a')+=1;
}


val pq=new scala.collection.mutable.PriorityQueue[Key];

for(c<-'a' until 'z'){
	val chars=c-'a';

	if(count(chars)>0){

		pq.enqueue(new Key(count(chars),c))

	}
}

val resArr:Array[Char]=new Array[Char](str.length);
val emptyList=new java.util.ArrayList[Int]
val filledPositons=new java.util.ArrayList[Int];
val defaultIndexPosition:Boolean=false
for(i<-0 until str.length())
{
	emptyList.add(i);
}

funcArrangeChars(resArr:Array[Char],pq,emptyList:java.util.ArrayList[Int],str,filledPositons,defaultIndexPosition,incr)
println(String.valueOf(resArr))
}
//---- Recurring function to extract priorityQueue
def funcArrangeChars(resArr:Array[Char],pq:scala.collection.mutable.PriorityQueue[Key],lst:java.util.ArrayList[Int],str:String,filledPositons:java.util.ArrayList[Int],defaultIndexPosition:Boolean,incr:Int){

  if(!defaultIndexPosition){
	if(pq.size > 0){
		val  key=pq.dequeue();
		val emptyPositions=new java.util.ArrayList[Int];

		val alternate:Boolean=false
				val pos:Int=lst.get(incr);

		//get the position from specific index
		someFunc(key,resArr,pos,lst,alternate,emptyPositions,filledPositons)
		val newDefaultIndexPostion:Boolean=true
		funcArrangeChars(resArr,pq,emptyPositions,str,filledPositons,newDefaultIndexPostion,incr)
	}
  }
  else{
  	if(pq.size > 0){
		val  key=pq.dequeue();
		val emptyPositions=new java.util.ArrayList[Int];

		val alternate:Boolean=false
				val pos:Int=0;//used a variable defaultindex postion so for next iternation it starts from 0

		//get the position from specific index
		someFunc(key,resArr,pos,lst,alternate,emptyPositions,filledPositons)
		val newDefaultIndexPostion:Boolean=true
		funcArrangeChars(resArr,pq,emptyPositions,str,filledPositons,newDefaultIndexPostion,incr)
	}
     
  }
}



def someFunc(key:Key,resArr:Array[Char],index:Int,lst:java.util.ArrayList[Int],alternate:Boolean,emptyPositions:java.util.ArrayList[Int],filledPositions:java.util.ArrayList[Int]){
  
  if(!alternate){
    if(key.freq>0){
    val position=lst.get(index)
    val newPosition=checkIfPositionIsFilled(index,position,lst,filledPositions)
   // print(key.ch)
    resArr(newPosition)=key.ch
    filledPositions.add(newPosition)
    val newAlternate=true
    val newIndex=checkAndRotateIndex(index+1,lst)
    val newKey=new Key(key.freq-1,key.ch)
    someFunc(newKey,resArr,newIndex,lst,newAlternate,emptyPositions,filledPositions)
    }
  }
  else{
    val position=lst.get(index)
    //println(" "+position+"to leave")
    emptyPositions.add(position)
    val newAlternate=false
    val newIndex=checkAndRotateIndex(index+1,lst)
    someFunc(key,resArr,newIndex,lst,newAlternate,emptyPositions,filledPositions)
  }
}

def checkAndRotateIndex(index:Int,lst:java.util.ArrayList[Int]):Int={
  //return 1
  if(index ==lst.size()-1){
    return index
  }
  else if(index == lst.size()){
    return index-lst.size()
  }
  else{
    index
  }
}

def checkIfPositionIsFilled(index:Int,position:Int,lst:java.util.ArrayList[Int],filledPositions:java.util.ArrayList[Int]):Int={
  
  if(filledPositions.contains(position)){
   val newPosition=lst.get(index+1)
   return checkIfPositionIsFilled(index+1,newPosition,lst,filledPositions)
  }
  else{
    return position
  }
}

}


