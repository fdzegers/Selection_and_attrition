# Draw the flowchart of the cohort selection

library(DiagrammeR)

grViz("digraph flowchart {
      #rankdir='LR';
      #graph [splines=ortho, nodesep=1]
      # node definitions 
      node [fontname = Arial, shape = rectangle]        
      main1 [label = <Completed <I>Rheumatic?</I>  questionnaires,<BR/>n=38,137>, group=main]
      main2 [label = <Final population of answers to <I>Rheumatic?</I>  questionnaire,<BR/> n=31,457>, group=main]
     
#     main3 [label = <Eligible answers to T0b,<BR/> n=31,457>, group=main]
      main4 [label = <Completed answers to T0b,<BR/> n=15,591>, group=main]
      
      sec1 [label = <Duplicate, fake and missing e-mail adresses,<BR/> and entries with missing basic information,<BR/> n=6,978>,  group='']
      sec2 [label = <Not answered or incomplete answers,<BR/> n=15,866>,  group='']
      
      # Arrows in between steps
      main12[label = '', fixedsize='false', width=0, height=0, shape=none]
      main34[label = '', fixedsize='false', width=0, height=0, shape=none]
      

      # edge definitions with the node IDs
      main1 -> main12[arrowhead=None, weight = 2] # weight makes it be placed to the left as the secondary (the deleted) will have a lower weight
      main12 -> main2[weight = 2]

      
#      main2 -> main4
      main2 -> main34[arrowhead=None, weight = 2]
      main34 -> main4[weight = 2]
      
      main12 -> sec1 
      main34 -> sec2

      }      ")

