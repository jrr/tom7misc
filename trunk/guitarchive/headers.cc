#include "headers.h"

#include <initializer_list>

// Actual headers seen in files from OLGA (etc.). I thought there would only
// be a few variants, but there turn out to be dozens, and so I probably
// should have done this a different way (regex/edit distance..?)

static const char *HEADER1 =
" #----------------------------------PLEASE NOTE-----"
"----------------------------#\n"
"#This file is the author's own work and represents "
"their interpretation of the #\n"
"#song. You may only use this file for private study, "
"scholarship, or research. #\n"
"#----------------------------------------------------"
"--------------------------##\n";

static const char *HEADER2 =
" #----------------------------------PLEASE NOTE-----"
"----------------------------#\n"
"#This file is the author's own work and represents "
"their interpretation of the #\n"
"#song. You may only use this file for private study, "
"scholarship, or research. #\n"
"#----------------------------------------------------"
"--------------------------#\n";

static const char *HEADER3 =
" #----------------------------------PLEASE NOTE------"
"--------------------------#\n"
"#This file is the author's own work and represents their "
"interpretation of the#\n"
"#song. You may only use this file for private study, "
"scholarship, or research.#\n"
"#------------------------------------------------------"
"-----------------------#\n";

static const char *HEADER4 =
"#----------------------------------PLEASE NOTE-----"
"----------------------------#\n"
"#This file is the author's own work and represents "
"their interpretation of the #\n"
"#song. You may only use this file for private study, "
"scholarship, or research. #\n"
"#----------------------------------------------------"
"--------------------------##\n";

static const char *HEADER5 =
"#-----------------------------PLEASE NOTE-------"
"------------------------------#\n"
"#This OLGA file is the author's own work and "
"represents their interpretation  #\n"
"#of the song. You may only use this file for private "
"study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or "
"some other monospaced    #\n"
"#font. See http://www.olga.net/faq/ for more "
"information.                     #\n"
"#------------------------------------------------"
"-----------------------------#\n";

static const char *HEADER6 =
"#----------------------------PLEASE NOTE-----------------------------#\n"
"# This represents the author's own work and interpretation of the    #\n"
"# song. To be used only for private study, scholarship, or research. #\n"
"#--------------------------------------------------------------------#\n";

static const char *HEADER7 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n"
"#\n";

static const char *HEADER8 =
"#----------------------------------PLEASE NOTE--------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the#\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER9 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#--------------------------------------------------------------------------------##\n";

static const char *HEADER10 =
"#----------------------------------PLEASE NOTE-----------------------------------##\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER11 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER12 =
"#----------------------------------PLEASE NOTE------------------------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research#\n"
"#---------------------------------------------------------------------------------------------#\n";

static const char *HEADER13 =
"#-------------------------------PLEASE NOTE-------------------------------------#\n"
"# This file is the author's own work and represents their interpretation of the #\n"
"# song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------#\n";

static const char *HEADER14 =
"#-------------------------------PLEASE NOTE-------------------------------------#\n"
"# This file is the author's own work and represents their interpretation of the #\n"
"# song. You may only use this file for private study, scholarship, or research. #\n"
"#-------------------------------------------------------------------------------#\n";

static const char *HEADER15 =
"#-------------------------------PLEASE NOTE--------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the#\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#--------------------------------------------------------------------------#\n"
"#-- File created with Instab- http://www.pconline.com/~smcarey/instab.html #\n"
"#--------------------------------------------------------------------------#\n";

static const char *HEADER16 =
"#-----------------------------PLEASE NOTE-------------------------------------#\n"
"#This OLGA file is the author's own work and represents their interpretation  #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or other monospaced font.   #\n"
"#See http://www.olga.net/faq/ for more information.                           #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER17 =
"#--------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents his interpretation of the #\n"
"#song. You may only use this file for private study, scholarship or research.#\n"
"#----------------------------------------------------------------------------#\n";

static const char *HEADER18 =
"#-----------------------------PLEASE NOTE-------------------------------------#\n"
"#This OLGA file is the author's own work and represents their interpretation  #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or other monospaced font.   #\n"
"#See http://www.olga.net/faq/ for more information.                           #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER19 =
"#---------------------------------PLEASE NOTE------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the#\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#--------------------------------------------------------------------------#\n"
"#-File created with Instab - http://www.pconline.com/~smcarey/instab.html -#\n"
"#--------------------------------------------------------------------------#\n";

static const char *HEADER20 =
"#----------------------------------PLEASE NOTE-----------------------------#\n"
"#This file is the author's own work and represents their interpretation of #\n"
"#the song. You may only use this file for private study, scholarship, or   #\n"
"#research.                                                                 #\n"
"#--------------------------------------------------------------------------#\n";

static const char *HEADER21 =
"#---------------------------------PLEASE NOTE---------------------------------#\n"
"#This tab is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this tab for private study, scholarship, or research. #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER22 =
"(This file is the author's own work and represents\n"
"their interpretation of the song. You may only use\n"
"this file for private study, scholarship, or research.)\n";

static const char *HEADER23 =
"#---------------------------------PLEASE NOTE---------------------------------#\n"
"#This tab is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this tab for private study, scholarship, or research. #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER24 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER25 =
"#---------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents his interpretation of the  #\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#-----------------------------------------------------------------------------#\n";


static const char *HEADER26 =
"#----------------------------------PLEASE NOTE----------------------------------\n"
"#This file is the author's own work and represents his/her interpretation of the\n"
"#song. You may only use this file for private study, scholarship, or research.\n"
"#-------------------------------------------------------------------------------#\n";

static const char *HEADER27 =
"X===============================PLEASE NOTE==============================X\n"
"X  This tab is all my own and represents my interpretation of the song.  X\n"
"X You may only use this tab for private study, scholarship, or research. X\n"
"X  Do not change, deface, mutilate, or in any other way alter this tab.  X\n"
"X   You must email me first if you want to use this tab on your site.    X\n"
"X========================================================================X\n";

static const char *HEADER28 =
"#-----------------------------PLEASE NOTE-------------------------------------# #This OLGA file is the author's own work and represents their interpretation  #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or some other monospaced    #\n"
"#font. See http://www.olga.net/faq/ for more information.                     #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER29 =
"NOTE: This file is the author's own work and represents his\n"
"interpretation of the song. You may only use this file for\n"
"private study, scholarship, or research.\n";

static const char *HEADER30 =
"#----------------------------------PLEASE NOTE---------------------------------# #\n"
"This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER31 =
"#---------------------------PLEASE NOTE------------------------------#\n"
"#This file is the author's own work and represents their interpreta- #\n"
"#tion of the song. You may only use this file for private study,scho-#\n"
"#larship, or research.                                               #\n"
"#--------------------------------------------------------------------#\n";

static const char *HEADER32 =
"#----------------------------------PLEASE NOTE----------------------------------#\n"
"# This file is the author's own work and represents their interpretation of the #\n"
"# song. You may only use this file for private study, scholarship, or research. #\n"
"#-------------------------------------------------------------------------------#\n";

static const char *HEADER33 =
"This file is my own work and represents my\n"
"interpretation of the song. This file is meant only\n"
"for private study, scholarship, or research.\n";

static const char *HEADER34 =
"#---------------------------------PLEASE NOTE---------------------------------#\n"
"#this file is the author's own work and represents his interpretation of the  #\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER35 =
"----------------------------------PLEASE NOTE--------------------------------\n"
"This file is the author's own work and represents their interpretation ofthe\n"
"song. You may only use this file for private study, scholarship,or research.\n"
"-----------------------------------------------------------------------------\n";

static const char *HEADER36 =

"#----------------------------------PLEASE NOTE---------------------------------#\n"
"\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER37 =
"#----------------------------PLEASE NOTE-------------------------------------#\n"
"#This OLGA file is the author's own work and represents their interpretation  #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or some other monospaced    #\n"
"#font. See http://www.olga.net/faq/ for more information.                     #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER38 =
"This file is an original transcription by me and represents my\n"
"interpretation of the song. You may only use this file for private\n"
"study, scholarship, or research.\n";

static const char *HEADER39 =
"#----------------------------------PLEASE NOTE--------------------------------\n"
"#This file is the author's own work and represents their interpretation of the\n"
"#song. You may only use this file for private study, scholarship, or research.\n"
"#-----------------------------------------------------------------------------\n";

static const char *HEADER40 =
"# ---------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER41 =
"#-------------------PLEASE NOTE-------------------------#\n"
"#This file is the author's own work and represents their#\n"
"#interpretation of the song. You may only use this file #\n"
"#for private study, scholarship, or research.           #\n"
"#-------------------------------------------------------#\n";

static const char *HEADER42 =
"#--------------------------------- PLEASE NOTE --------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song.  You may only use this file for private study, scholarship, or research.#\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER43 =
"#------------------------------------PLEASE  NOTE----------------------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research#\n"
"#----------------------------------------------------------------------------------------------#\n";

static const char *HEADER44 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the-#\n"
"#song. You may only use this file for private study, scholarship, or research.-#\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER45 =
"-----------------------------PLEASE NOTE-------------------------------------#\n"
"#This OLGA file is the author's own work and represents their interpretation  #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or some other monospaced    #\n"
"#font. See http://www.olga.net/faq/ for more information.                     #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER46 =
"#-------------------------------PLEASE NOTE------------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";
  
static const char *HEADER47 =
"X===============================PLEASE NOTE=====================================x\n"
"X This tab belongs to its author and represents his interpretation of the song. x\n"
"X You may only use this tab for private study, scholarship, or research.        x\n"
"X Do not change, deface, mutilate, or in any other way alter this tab.          x\n"
"X You must email the author first if you want to use this tab on your site.     x\n"
"X===============================================================================x\n";
  
static const char *HEADER48 =
"# This file is the author's own work and represents their interpretation of the #\n"
"# song. You may only use this file for private study, scholarship, or research. #\n";

static const char *HEADER49 =
"#----------------------------------PLEASE NOTE-------------------------------#\n"
"#This file is the author's own work and represents his interpretation of the #\n"
"#song. You may only use this file for private study, scholarship or research.#\n"
"#----------------------------------------------------------------------------#\n";

static const char *HEADER50 =
"#This file is the author's own work and represents their interpretation of the song.\n"
"You may only use this file for private study, scholarship, or research.#\n";

static const char *HEADER51 =
"***********************************************************************\n"
"You may only use this file for private study, scholarship, or research.\n"
"It cannot be distributed freely.\n"
"***********************************************************************\n";

static const char *HEADER52 =
"NOTE: All tab files that appear on this site are the author's own work and represents their interpretation\n"
"the song. You may only use this file for private study, scholarship, or research. All songs in this\n"
"are copyrighted so please respect these copyrights.\n";

static const char *HEADER53 =
"#-----------------------------------PLEASE NOTE----------------------------#\n"
"#This file is the authors own work and represents their interpretation of this\n"
"##song. you may only use this for private study, scholarship, or research.\n"
"#--------------------------------------------------------------------------#\n";

static const char *HEADER54 =
"#------------------------------------------------------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";  

static const char *HEADER55 =
"#-------------------------------PLEASE NOTE---------------------------------#\n"
"#This file represents the author's own work, please only use it for research#\n"
"#or private study. Rights are strictly property of their respective authors #\n"
"#artists or labels. Chords and lyrics bellow are for non-commercial use only#\n"
"#---------------------------------------------------------------------------#\n";

static const char *HEADER56 =
"#---------------------------------YOU BEST NOTE-------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the#\n"
"#song. You may only use this file for private study, scholarship, or research.#\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER57 =
"#---------------------------------PLEASE NOTE-----------------------------------#\n"
"# This file is the author's own work and represents their interpretation of the #\n"
"# song.  You may only use this file for private study, scholarship, or research.#\n"
"#-------------------------------------------------------------------------------#\n";

static const char *HEADER58 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may use this file only for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER59 =
"#---------------------------------PLEASE NOTE----------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n"
"#------------Created with Taborama Tab Creator-----www.taborama.com------------#\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER60 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents his   interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER61 =
"#----------------------------------PLEASE NOTE-----------------------------#\n"
"#This file is the author's own work and represents their interpretation of the song.\n"
"You may only use this file for private study, scholarship, or research.\n"
"#--------------------------------------------------------------------------#\n";

static const char *HEADER62 =
"##################################################################################\n"
"# This file is the author's own work and represents their interpretation of the  #\n"
"# song. You may only use this file for private study, scholarship, or research.  #\n"
"##################################################################################\n";

static const char *HEADER63 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the-#\n"
"#song. You may only use this file for private study, scholarship, or research.-#\n"
"#---------------This file was downloaded from www.taborama.com-----------------#\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER64 =
"#----------------------PLEASE NOTE----------------------#\n"
"#This file is the author's own work and represents their#\n"
"#interpretation of the song. You may only use this file #\n"
"#for private study, scholarship, or research.           #\n"
"#-------------------------------------------------------#\n";
  
static const char *HEADER65 =
"#---------------------------------PLEASE NOTE----------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n"
"#------------Created with Taborama Tab Creator-----------------#\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER66 =
"#----------------------------------PLEASE NOTE------------------------------\n"
"#This file is the author's own work and represents their interpretation of\n"
"the song. You may only use this file for private study, scholarship, or research.\n"
"#---------------------------------------------------------------------------\n";

static const char *HEADER67 =
"#---------------------------------PLEASE NOTE----------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER68 =
"#----------------------------------PLEASE NOTE-------------------------------\n"
"#This file is the author's own work and represents their interpretation of the song. You\n"
"only use this file for private study, scholarship, or research.\n"
"#----------------------------------------------------------------------------\n"
"#-- File created with Instab - http://www.pconline.com/~smcarey/instab.html -\n"
"#----------------------------------------------------------------------------\n";

static const char *HEADER69 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author\\\\\\'s own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------##\n";

static const char *HEADER70 =
"#-----------------------------PLEASE NOTE-------------------------------------#\n"
"#This file is the author's own work and represents their interpretation       #\n"
"#of the song. You may only use this file for private study, scholarship, or   #\n"
"#research. Remember to view this file in Courier, or some other monospaced    #\n"
"#font.                                                                        #\n"
"#-----------------------------------------------------------------------------#\n";

static const char *HEADER71 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents his interpretation of the   #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"================================================================================\n";

static const char *HEADER72 =
"#----------------------------PLEASE NOTE------------------------------------#\n"
"#This file is the author's own work and represents their interpretation of  #\n"
"#the song. You may use this file only for private study, scholarship, or    #\n"
"#research.                                                                  #\n"
"#---------------------------------------------------------------------------#\n";

static const char *HEADER73 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the song.\n"
"You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER74 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents their interpretation of the #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#----------------------------------------------------------------\n";

static const char *HEADER75 =
"#------------------------------- PLEASE NOTE -------------------------------#\n"
"# This file is the author's own work and represents their interpretation of #\n"
"# the song.                                                                 #\n"
"# You may only use this file for private study, scholarship, or research.   #\n"
"#---------------------------------------------------------------------------#\n";

static const char *HEADER76 =
"#----------------------------------PLEASE NOTE---------------------------------#\n"
"#This file is the author's own work and represents his interpretation of the   #\n"
"#song. You may only use this file for private study, scholarship, or research. #\n"
"#------------------------------------------------------------------------------#\n";

static const char *HEADER77 =
"#----------------------------------PLEASE NOTE----------------------------------#\n"
"#This file is the author's own work and represents their interpretation of  the #\n"
"#song. You may only use this file for private study, scholarship, or research.  #\n"
"#------------------------------------------------------------------------------##\n";
  

std::initializer_list<const char *> Headers::headers = {
  HEADER1, HEADER2, HEADER3, HEADER4, HEADER5, HEADER6, HEADER7,
  HEADER8, HEADER9, HEADER10, HEADER11, HEADER12, HEADER13, HEADER14,
  HEADER15, HEADER16, HEADER17, HEADER18, HEADER19, HEADER20,
  HEADER21, HEADER22, HEADER23, HEADER24, HEADER25, HEADER26,
  HEADER27, HEADER28, HEADER29, HEADER30, HEADER31, HEADER32,
  HEADER33, HEADER34, HEADER35, HEADER36, HEADER37, HEADER38,
  HEADER39, HEADER40, HEADER41, HEADER42, HEADER43, HEADER44,
  HEADER45, HEADER46, HEADER47, HEADER48, HEADER49, HEADER50,
  HEADER51, HEADER52, HEADER53, HEADER54, HEADER55, HEADER56,
  HEADER57, HEADER58, HEADER59, HEADER60, HEADER61, HEADER62,
  HEADER63, HEADER64, HEADER65, HEADER66, HEADER67, HEADER68,
  HEADER69, HEADER70, HEADER71, HEADER72, HEADER73, HEADER74,
  HEADER75, HEADER76, HEADER77,
};

  
