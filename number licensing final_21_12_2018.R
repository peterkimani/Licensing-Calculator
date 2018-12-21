licensingcalculator<-function(x,y)
{
  ######################################################################################
  ##############PART A: CREATING A DOMAIN WITH NUMERIC INDEX OF NUMBER PLATES###########
  ######################################################################################
  
  ###CREATING A DOMAIN OF ALL POSSIBLE LETTERS AND NUMBERS IN A  LICENSCE PLATE###
  myalphabet<-toupper(letters)
  #KENYAN NUMBER PLATE DOESN'T HAVE LETTER I AND O, HENCE REMOVING THEM 
  myalphabet<-myalphabet[-c(9,15)]
  #REVERSE ASSIGNING OF CALCULATION POINTS TO THE ALPHABET CREATED WHERE A HAS THE HIGHEST POINTS OF 24
  #AND Z HAS THE LOWEST OF ONE
  revindex<-order(1:length(myalphabet),decreasing = T)
  revdataframe<-cbind(myalphabet,revindex)
  
  ### CREATING NUMERIC INDEX WITHOUT REVERSING THE POINTS ASSIGNED
  mnumber<-as.character(c(0:9))
  datnum<-data.frame(cbind(mnumber,mnumber))
  colnames(datnum)<-colnames(revdataframe)
  frevdataframe<-rbind(revdataframe,datnum)
  frevdataframe$myalphabet<-as.character(frevdataframe$myalphabet)
  frevdataframe$revindex<-as.character(frevdataframe$revindex)
  
  ################################################################################
  ######PART TWO: MANIPULATING THE INPUT NUMBER PLATES AND MATCHING###############
  ##########THE NUMBER PLATES WITH THE DOMAIN (NUMERIC INDEX)######################
  ##############################################################################
  #chaning the input number plates into capital letters
  plateone<-toupper(x)
  platetwo<-toupper(y)
  plateone<-unlist(strsplit(plateone,split = ""),use.names = F)
  platetwo<-unlist(strsplit(platetwo,split = ""),use.names= F)
  
  #converting number plate one from a combination of character and numeric into a numeric 
  #index using the reference in frevdataframe object
  plate1.index<-c()
  onelength<-for(i in 1:length(plateone))
  {
    plate1.index[i]<-frevdataframe[frevdataframe[,1]==plateone[i],][2]  
  }
  
  plate1.index<-as.numeric(plate1.index)
  
  #converting number plate two from a combination of character and numeric into a numeric 
  #index using the reference in frevdataframe object
  plate2.index<-c()
  twolength<-for(i in 1:length(platetwo))
  {
    plate2.index[i]<-frevdataframe[frevdataframe[,1]==platetwo[i],][2]  
  }
  plate2.index<-as.numeric(plate2.index)
  
  #manipulating the indexes obtained by collapsing the three digit numerics in a plate 
  #into one vector with one element insead of a vector into three elements (see object flava.c and rata.c below)
  flava.a<-plate1.index[2]
  flava.b<-plate1.index[3]
  flava.c<-paste(plate1.index[4:6],collapse = "")
  flava.d<-plate1.index[7]
  
  
  rata.a<-plate2.index[2]
  rata.b<-plate2.index[3]
  rata.c<-paste(plate2.index[4:6],collapse = "")
  rata.d<-plate2.index[7]
  
  
  #obtaining the NUMERIC INDEX difference between number plate one and number plate 2 
  one<-c(flava.a,flava.b,flava.c,flava.d)
  two<-c(rata.a,rata.b,rata.c,rata.d)
  mytabplate<-data.frame(as.numeric(one),as.numeric(two))
  names(mytabplate)<-c("one","two")
  mytabplate$three<-mytabplate$one-mytabplate$two
  #############################################################################################
  ###############PART THREE: CALCULATING THE NUMBER OF CARS BOUGHT USING####################### 
  #THE DIFFERENCE IN THE NUMERIC INDEX IN PART TWO OF PLATE ONE AND PLATE TWO##################
  boughtcars<-mytabplate$three[1]*24*999*24+mytabplate$three[2]*999*24-mytabplate$three[3]*24+mytabplate$three[4]
  
  
  #######PART 4: WORKING ON THE OUTPUT###############
  #cars sold between number plate 1 and number plate 2 
  final.boughtcars<-boughtcars-1
  ffinal.boughtcars<-paste("The number of cars bought are",final.boughtcars,"between number plate",x,"and",y)
  readline("NOTE 1 out of 3:
1.Kindly, ensure the formart you entered 
  your function and the input number plates 
  is in the following formart 
  licensingplate('kca002a','kcd003b'). 
  PRESS ENTER TO CONTINUE") 
  readline("NOTE 2/ 3:
2.Kindly, ensure the old number plate should
  be the first one in your 
  input followed by the new one.
  PRESS ENTER TO CONTINUE") 
  readline("NOTE 3/3:
3.your number plate has the following format: 
  4 alphabet letters (excluding letter i and o),
  3 integers(excluding 000) and 1 alphabet letter 
  (excluding  letter i and o). 
  PRESS ENTER TO CONTINUE")
  
  if(nchar(x)!=7|nchar(y)!=7)
  {
    print("Either one or both of your number plates inputs is not of lenght 7 in the following format: four alphabet letters three numeric numbers and one alphabet letter.FOR EXAMPLE 'KCB001A'.Countercheck the number plate and run the function again")
  } else 
  {
    print(ffinal.boughtcars)
  }
}


#Example to find the number of cars bought between kca001a and kcb002b
licensingcalculator("kca001a","kcb002b")
