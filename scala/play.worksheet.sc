


val s = """
  	#F0F8FF aliceblue  	   	#FFA07A lightsalmon  
   	#FAEBD7 antiquewhite  	   	#20B2AA lightseagreen  
   	#00FFFF aqua  	   	#87CEFA lightskyblue  
   	#7FFFD4 aquamarine  	   	#778899 lightslategray  
   	#F0FFFF azure  	   	#B0C4DE lightsteelblue  
   	#F5F5DC beige  	   	#FFFFE0 lightyellow  
   	#FFE4C4 bisque  	   	#00FF00 lime  
   	#000000 black  	   	#32CD32 limegreen  
   	#FFEBCD blanchedalmond  	   	#FAF0E6 linen  
   	#0000FF blue  	   	#FF00FF magenta  
   	#8A2BE2 blueviolet  	   	#800000 maroon  
   	#A52A2A brown  	   	#66CDAA mediumaquamarine  
   	#DEB887 burlywood  	   	#0000CD mediumblue  
   	#5F9EA0 cadetblue  	   	#BA55D3 mediumorchid  
   	#7FFF00 chartreuse  	   	#9370DB mediumpurple  
   	#D2691E chocolate  	   	#3CB371 mediumseagreen  
   	#FF7F50 coral  	   	#7B68EE mediumslateblue  
   	#6495ED cornflowerblue  	   	#00FA9A mediumspringgreen  
   	#FFF8DC cornsilk  	   	#48D1CC mediumturquoise  
   	#DC143C crimson  	   	#C71585 mediumvioletred  
   	#00FFFF cyan  	   	#191970 midnightblue  
   	#00008B darkblue  	   	#F5FFFA mintcream  
   	#008B8B darkcyan  	   	#FFE4E1 mistyrose  
   	#B8860B darkgoldenrod  	   	#FFE4B5 moccasin  
   	#A9A9A9 darkgray  	   	#FFDEAD navajowhite  
   	#006400 darkgreen  	   	#000080 navy  
   	#BDB76B darkkhaki  	   	#FDF5E6 oldlace  
   	#8B008B darkmagenta  	   	#808000 olive  
   	#556B2F darkolivegreen  	   	#6B8E23 olivedrab  
   	#FF8C00 darkorange  	   	#FFA500 orange  
   	#9932CC darkorchid  	   	#FF4500 orangered  
   	#8B0000 darkred  	   	#DA70D6 orchid  
   	#E9967A darksalmon  	   	#EEE8AA palegoldenrod  
   	#8FBC8F darkseagreen  	   	#98FB98 palegreen  
   	#483D8B darkslateblue  	   	#AFEEEE paleturquoise  
   	#2F4F4F darkslategray  	   	#DB7093 palevioletred  
   	#00CED1 darkturquoise  	   	#FFEFD5 papayawhip  
   	#9400D3 darkviolet  	   	#FFDAB9 peachpuff  
   	#FF1493 deeppink  	   	#CD853F peru  
   	#00BFFF deepskyblue  	   	#FFC0CB pink  
   	#696969 dimgray  	   	#DDA0DD plum  
   	#1E90FF dodgerblue  	   	#B0E0E6 powderblue  
   	#B22222 firebrick  	   	#800080 purple  
   	#FFFAF0 floralwhite  	   	#FF0000 red  
   	#228B22 forestgreen  	   	#BC8F8F rosybrown  
   	#FF00FF fuchsia  	   	#4169E1 royalblue  
   	#DCDCDC gainsboro  	   	#8B4513 saddlebrown  
   	#F8F8FF ghostwhite  	   	#FA8072 salmon  
   	#FFD700 gold  	   	#FAA460 sandybrown  
   	#DAA520 goldenrod  	   	#2E8B57 seagreen  
   	#808080 gray  	   	#FFF5EE seashell  
   	#008000 green  	   	#A0522D sienna  
   	#ADFF2F greenyellow  	   	#C0C0C0 silver  
   	#F0FFF0 honeydew  	   	#87CEEB skyblue  
   	#FF69B4 hotpink  	   	#6A5ACD slateblue  
   	#CD5C5C indianred  	   	#708090 slategray  
   	#4B0082 indigo  	   	#FFFAFA snow  
   	#FFFFF0 ivory  	   	#00FF7F springgreen  
   	#F0E68C khaki  	   	#4682B4 steelblue  
   	#E6E6FA lavender  	   	#D2B48C tan  
   	#FFF0F5 lavenderblush  	   	#008080 teal  
   	#7CFC00 lawngreen  	   	#D8BFD8 thistle  
   	#FFFACD lemonchiffon  	   	#FF6347 tomato  
   	#ADD8E6 lightblue  	   	#40E0D0 turquoise  
   	#F08080 lightcoral  	   	#EE82EE violet  
   	#E0FFFF lightcyan  	   	#F5DEB3 wheat  
   	#FAFAD2 lightgoldenrodyellow  	   	#FFFFFF white  
   	#90EE90 lightgreen  	   	#F5F5F5 whitesmoke  
   	#D3D3D3 lightgrey  	   	#FFFF00 yellow  
   	#FFB6C1 lightpink  	   	#9ACD32 yellowgreen  
"""

val cs = s.split("\n").toSeq.flatMap(
  _.split("  ").toSeq.map(_.trim).filterNot(_=="")
).map(
  _.split(" ").toSeq
).collect{
  case Seq(hex,name) => name -> hex
}
  

val good = s"""
Map(
${cs.map((name,hex) => s"\"$name\" -> \"$hex\"").mkString("\n")}
)
"""