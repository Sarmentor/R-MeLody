
library(sound)
library(audio)


notes <- function(choice, dur){

# Note  <-	Frequency (Hz) 
C0 	<- 16.35 
Db0 <- 17.32 	
D0 	<- 18.35 
Eb0 <- 19.45
E0 	<- 20.60 
F0 	<- 21.83
Gb0 <- 23.12
G0 	<- 24.50
Ab0 <- 25.96
A0 	<- 27.50
Bb0 <- 29.14
B0 	<- 30.87
C1 	<- 32.70
Db1 <- 34.65 	
D1 	<- 36.71 	
Eb1 <- 38.89 	
E1 	<- 41.20 	
F1 	<- 43.65 	
Gb1 <- 46.25 	
G1 	<- 49.00 	
Ab1 <- 51.91 	
A1 	<- 55.00 	
Bb1 <- 58.27 	
B1 	<- 61.74 	
C2 	<- 65.41 	
Db2 <- 69.30 	
D2 	<- 73.42 	
Eb2 <- 77.78 	
E2 	<- 82.41 	
F2 	<- 87.31 	
Gb2 <- 92.50 	
G2 	<- 98.00 	
Ab2 <- 103.83 	
A2 	<- 110.00 	
Bb2 <- 116.54 	
B2 	<- 123.47 	
C3 	<- 130.81 	
Db3 <- 138.59 	
D3 	<- 146.83 	
Eb3 <- 155.56 	
E3 	<- 164.81 	
F3 	<- 174.61 	
Gb3 <- 185.00 	
G3 	<- 196.00 	
Ab3 <- 207.65 	
A3 	<- 220.00 	
Bb3 <- 233.08 	
B3 	<- 246.94 	
C4 	<- 261.63 	
Db4 <- 277.18 	
D4 	<- 293.66 	
Eb4 <- 311.13 	
E4 	<- 329.63 	
F4 	<- 349.23 	
Gb4 <- 369.99 	
G4 	<- 392.00 	
Ab4 <- 415.30 	
A4 	<- 440.00 	
Bb4 <- 466.16 	
B4 	<- 493.88 	
C5 	<- 523.25 	
Db5 <- 554.37 	
D5 	<- 587.33 	
Eb5 <- 622.25 	
E5 	<- 659.26 	
F5 	<- 698.46 	
Gb5 <- 739.99 	
G5 	<- 783.99 	
Ab5 <- 830.61 	
A5 	<- 880.00 	
Bb5 <- 932.33 	
B5 	<- 987.77 	
C6 	<- 1046.50 	
Db6 <- 1108.73 	
D6 	<- 1174.66 	
Eb6 <- 1244.51 	
E6 	<- 1318.51 	
F6 	<- 1396.91 	
Gb6 <- 1479.98 	
G6 	<- 1567.98 	
Ab6 <- 1661.22 	
A6 	<- 1760.00 	
Bb6 <- 1864.66 	
B6 	<- 1975.53 	
C7 	<- 2093.00 	
Db7 <- 2217.46 	
D7 	<- 2349.32 	
Eb7 <- 2489.02 	
E7 	<- 2637.02 	
F7 	<- 2793.83 	
Gb7 <- 2959.96 	
G7 	<- 3135.96 	
Ab7 <- 3322.44 	
A7 	<- 3520.00 	
Bb7 <- 3729.31 	
B7 	<- 3951.07 	
C8 	<- 4186.01 	
Db8 <- 4434.92 	
D8 	<- 4698.64 	
Eb8 <- 4978.03
silence <- "silence"


#duration of notes(beats) per bar
longa <- 1/0.25
double_whole_note <- 1/0.5
whole_note <- 1
half_note <- 1/2
quarter_note <- 1/4
eighth_note <- 1/8
triplet <- 1/12
sixteenth_note <- 1/16
thirty_second_note <- 1/32
sixty_fourth_note <- 1/64
hundred_twenty_eighth_note <- 1/128

choice <- get(choice)
#browser()
dur <- get(dur)

#vector of notes duration
notes_per_beat <- c(longa,double_whole_note,whole_note,half_note,quarter_note,eighth_note,triplet,sixteenth_note,thirty_second_note,sixty_fourth_note,hundred_twenty_eighth_note)
#browser()
if (choice == "silence"){
  output <- Silence(as.double(tempo(bpm, dur, 44100,0)), rate=44100, bits=16, channels=1)
}else{
  output <- Sine(choice, as.double(tempo(bpm, dur, 44100,0)), rate=44100, bits=16, channels=2)
}


return(output)
}

silence <- function(dur){

output <- Silence(dur, rate=8000, bits=8, channels=1)
return(output)
}

tempo <- function(bpm, note_length, rate, swing=0){
#60bpm => 1beat/second
#note_length = number of notes per bar
  note_length <- 4 * note_length
duration <- ((60/bpm)*note_length)

sample_length <- duration#*rate
output <- sample_length
return(output)
}

melody <- function(key, tone, struct, max_note_per_beat, harmony_first=0, harmony_in, rythm = 0){
#melody constructed according to tonal rules - 3 notes
major_tonal <- c(2 ,2 ,1 ,2 ,2 ,2 ,1) #steps for constructing the major scale tones
minor_tonal <- c(2 ,1 ,2 ,2 ,1 ,2 ,2) #steps for constructing relative minor scale
scale_vector <- c('C','Db','D','Eb','E','F','Gb','G','Ab','A','Bb','B') #12 note scale
melody_sample <- NULL
melody_df <- NULL
sum_dur <- 0
aux_dur <- 0
dur <- "longa"
bar <- 1
i <- 0
j <- 0

#duration of notes(beats) per bar
longa <- 1/0.25
double_whole_note <- 1/0.5
whole_note <- 1
half_note <- 1/2
quarter_note <- 1/4
eighth_note <- 1/8
triplet <- 1/12
sixteenth_note <- 1/16
thirty_second_note <- 1/32
sixty_fourth_note <- 1/64
hundred_twenty_eighth_note <- 1/128

#vector of notes duration
notes_per_beat <- c('longa','double_whole_note','whole_note','half_note','quarter_note','eighth_note','triplet', 'sixteenth_note','thirty_second_note','sixty_fourth_note','hundred_twenty_eighth_note')

#browser()

root <- key #the root of the chord
index <- which(scale_vector==root)
#major scale, get relative minor and key chord tones
if (tone == "maj"){
if (index > 3) root_relative_minor_scale <- index-3
else root_relative_minor_scale <- index+9;
if (index > 1 & index <=5){
key_tones <- c(index,index+4,index+7,index-1)
} else if(index > 5 & index <= 8){
key_tones <- c(index,index+4,index-5,index-1)
}else if(index > 8){
key_tones <- c(index,index-8,index-5,index-1)
} else 
key_tones <- c(index,index+4,index+7,index+11);
}
#minor scale, get relative major and key chord tones
if (tone == "min"){
if (index > 9){
  root_relative_major_scale <- index-9
  key_tones <- c(index,index-9,index-5,index-1)
}else root_relative_major_scale <- index+3

if (index > 1 & index <=5){
key_tones <- c(index,index+3,index+7,index-1)
}else if(index > 5 & index <= 8){
key_tones <- c(index,index+3,index-5,index-1)
}else 
key_tones <- c(index,index+3,index+7,index+11);

}

beat <- 1

#time for each beat
beats_1 <- 1/4
beats_2 <- 2/4
beats_3 <- 3/4
beats_4 <- 4/4

#constructs vector of beats dependent of number of bars for each section
for (k in 1:struct[2])
{
beats_1 <- c(beats_1, beats_1[k-1] + 1)
beats_2 <- c(beats_2, beats_2[k-1] + 1)
beats_3 <- c(beats_3, beats_3[k-1] + 1)
beats_4 <- c(beats_4, beats_4[k-1] + 1)
}

##browser()

while(sum_dur < (as.integer(struct[2]))) #while does not end section A keep generating notes/silence and duration
{
  ##browser()
  contador <- 0
  while ((beat==1 || beat==3) && sum_dur < (as.integer(struct[2])))
  {
      i <- round(runif(1,1,4)) #pick scale key tones randomly
      ###browser()
      #pick duration of notes randomly and inside limits of the section
      while(sum_dur + aux_dur > beats_4[as.integer(struct[2])] || contador < 101){
          j <- round(rnorm(1,5.5,0.5))
          ###browser()
          dur <- notes_per_beat[j]
          #dur <- "eighth_note"
          aux_dur <- get(dur)
          contador <- contador + 1
      }
      ###browser()
      note_tone <- scale_vector[key_tones[i]] #note tone
      #dur <- notes_per_beat[j] #note duration
      note_octave <- round(rnorm(1,4,0)) #pick note octave randomly around octave 4 (0 <=> no deviation)
      note <- paste(note_tone,note_octave, sep="") #complete note
      #browser()
      melody_df <- cbind(melody_df,c(note,dur,aux_dur,bar,beat)) #build dataframe with music notes and duration for further studies 
      rownames(melody_df) = c("Tone","duration","dur value","bar","beat")
      #melody_sample <- appendSample(melody_sample, notes(note,dur)) #contructs sample with music , calls function notes for that
      sum_dur <- sum_dur + get(dur) #calculation of duration of melody until now
      ##browser()
      
      #in which bar is the code?
      for (z in 1:struct[2]){
      if (floor(z-sum_dur) == 0) bar=z
      print(paste(bar, " ", beat))
      } 
      
      #browser()
      if (sum_dur < beats_1[bar] & bar==1)
      {
        beat=1
     
      }else if (sum_dur>= beats_4[bar])#& sum_dur<=beats_1[bar])
      {
        
         #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
          beat=1
          bar <- bar +1
        
      }else if ((sum_dur>=beats_2[bar]-1/4) & (sum_dur<beats_2[bar])) { #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
        ##browser()
        beat=2
        break
      }else if ((sum_dur>=beats_3[bar]-1/4) & sum_dur<beats_3[bar]) { #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
        beat=3
      
      }else if ((sum_dur>=beats_4[bar]-1/4) & sum_dur<=beats_4[bar]){ #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
        beat=4
        break
      }
      ##browser()
  }
  
  while ((beat==2 || beat==4) && sum_dur < (as.integer(struct[2]))) 
  {
      #Escolhe tons meio tom acima ou abaixo dos tons
      #da tonalidade e permite também tons da tonalidade
      #com possibilidade de ter silêncios
      approach.tones <- unique(c(key_tones, key_tones-1, key_tones+1))
      #retira 0 e 13 do vector
      approach.tones <- approach.tones[approach.tones != c(0,13)]
      i <- round(runif(1,1,length(approach.tones)+1))
      
      
  
      while(dur<(beats_2[bar]-sum_dur)||dur < (beats_4[bar]-sum_dur)){
        j <- round(rnorm(1,5.5,0.5))
        dur <- notes_per_beat[j]
        #dur <- "eighth_note"
        aux_dur <- get(dur)
      }
      
      if (i!=length(approach.tones)+1)  
      {
      note_tone <- scale_vector[i]
      note_octave <- round(rnorm(1,4,0))
      note <- paste(note_tone,note_octave,sep="")
      melody_df <- cbind(melody_df, c(note,dur,aux_dur,bar,beat))
      rownames(melody_df) = c("Tone","duration","dur value","bar","beat")
      #melody_sample <- appendSample(melody_sample, notes(note,dur))
      sum_dur <- sum_dur + get(dur)
      ##browser()
      }else{ 
      note_tone <- "silence"
      note <- paste(note_tone)
      melody_df <- cbind(melody_df, c(note,dur,aux_dur,bar,beat))
      rownames(melody_df) = c("Tone","duration","dur value","bar","beat")
      #melody_sample <- appendSample(melody_sample, silence(dur))
      sum_dur <- sum_dur + get(dur)
      ##browser()
      }
      
      #in which bar is the code?
      for (z in 1:struct[2]){
        ##browser()
      if (floor(z-sum_dur) == 0) bar=z
      print(paste(bar, " ", beat))
      }
      
      ##browser()
      if (sum_dur <= beats_1[bar] & bar==1){
        ##browser()
        beat=1 
        break
      }
      else if (sum_dur>=beats_4[bar] & sum_dur<=beats_1[bar] ) {
        #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
          #browser()
          bar <- bar +1
          beat=1
          break
        
      }else if ((sum_dur>=beats_2[bar]-1/4) & (sum_dur<beats_2[bar])) { #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
        #browser()
        beat=2
      }
      
      else if ((sum_dur>=beats_3[bar]-1/4) & sum_dur<beats_3[bar]) { #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
      #browser()
        beat=3
      break
      }
      
      else if ((sum_dur>=beats_4[bar]-1/4) & sum_dur<=beats_4[bar]) #checks in which beat it is, is it a 1 or a 3 in the bar or is it a 2 or 4?
      {
        #browser()
      beat=4
      if(any(endbar <- which(beats_4 == sum_dur))){
        if(endbar != length(beats_4)){
          bar <- endbar +1
          beat = 1
        }
          
        }
      }
     
   #browser()
  }
  # for (z in 1:struct[2]){
  # if (floor(sum_dur-z) == 0) bar=z
  # }
  #browser()
}


#regras para a constru??o de melodia se a harmonia vier primeiro
#inspeciona notas presentes nas batidas 1 e 3
#calcula escala - outra fun??o



#output <- Sine(,as.double(paste(dur)), rate=44100,bits=16,channels=2)
output <- "OK"
#play(melody_sample)

return(melody_df)
}

chord <- function(number_notes=3, dur=1, key="C", tone=maj){
#chords constructed according to tonal rules - 3 notes
major_tonal <- c("maj","min","min","maj","maj","min","dim","maj")
minor_tonal <- c("min","dim","min","maj","aug","min","maj","min","maj","maj","dim","maj","dim","min")
scale_vector <- c('C','Db','D','Eb','E','F','Gb','G','Ab','A','Bb','B')
notes_per_beat <- c('longa','double_whole_note','whole_note','half_note','quarter_note','eighth_note','triplet','sixteenth_note','thirty_second_note','sixty_fourth_note','hundred_twenty_eighth_note')

root <- key #the root of the chord
index <- which(scale_vector==root)
#major chord
if (tone == "maj"){
second_chord_note <- index+4
third_chord_note <- index+7

if (second_chord_note > 12) {second_chord_note <- second_chord_note - 12}
if (third_chord_note > 12) {third_chord_note <- third_chord_note - 12}
}
#minor chord
if (tone == "min"){
second_chord_note <- index+3
third_chord_note <- index+7

if (second_chord_note > 12) {second_chord_note <- second_chord_note - 12}
if (third_chord_note > 12) {third_chord_note <- third_chord_note - 12}
}
#augmented chord
if (tone == "aug"){
second_chord_note <- index+4#third note
third_chord_note <- index+8 #fifth note

if (second_chord_note > 12) {second_chord_note <- second_chord_note - 12}
if (third_chord_note > 12) {third_chord_note <- third_chord_note - 12}
}
#diminished chord
if (tone == "dim"){
second_chord_note <- index+3
third_chord_note <- index+6

if (second_chord_note > 12) {second_chord_note <- second_chord_note - 12}
if (third_chord_note > 12) {third_chord_note <- third_chord_note - 12}
}


chord_output <- c(scale_vector[index],scale_vector[second_chord_note],scale_vector[third_chord_note]) #builds chord for output
return(chord_output)
}

harmony <- function(key, melody_first = 0, melody_in){
major_tonal <- c("maj","min","min","maj","maj","min","dim","maj")
minor_tonal <- c("min","dim","min","maj","aug","min","maj","min","maj","maj","dim","maj","dim","min")
scale_vector <- c('C','Db','D','Eb','E','F','Gb','G','Ab','A','Bb','B')

#regras para a constru??o de sequ?ncia de acordes
#major Key



#regras para a constru??o de sequ?ncia de acordes
#minor Key



#se a melodia vier primeiro regras para a harmoniza??o
#Verifica notas utilizadas no upbeat - batidas 1 e 3 
#manda outra fun??o construir acordes com estas notas



}


struct <- function(form,number_chorus, number_bars, number_chorus_improv){
#estrutura??o da m?sica - formas AABA, ABAB..., middle Eight, Bridge
# Middle Eight and Bridge sections present alternate musical themes to the main sections of the song.  
# While they are most strongly needed in the Verse/Chorus format, they also appear in AABA songs for additional contrast.

# A Middle Eight section (named because it is usually eight bars long) is a relatively brief diversion,
# typically appearing only once in a song before leading back to a Chorus.

# A Bridge plays a similar role to a Section B for a Verse/Chorus song, providing a distinctly different lyrical and musical space.  
# It can lead to a verse, chorus or back to itself.
#pode chamar outras fun??es em etapas e ir agregando resultados de modo a obter estrutura repetitiva

number_bars_a_section <- number_bars/4
number_bars_b_section <- number_bars_a_section

number_chorus_improv <- number_chorus - 2 #1st and last chorus for melody
struct <- c(form,number_bars_a_section,number_bars_b_section,number_chorus_improv) #structure of music

return(struct)

}


sync <- function(){




}

record <- function(){
# grava resultado final em wav
# e grava resultado final sob a forma de matriz ou dataframe com acordes e melodia e respectivas dura??es das notas


return()
}



# Falta fazer gerador aleat?rio de pares notas/dura??o
# para a melodia e acorde/dura??o para a harmonia

# conforme a melodia tem notas mais breves a harmonia deve ter notas mais longas e vice-versa, 
# respectivamente se a harmonia vem primeiro ou depois da melodia

# gerador aleat?rio para a ordem da constru??o da m?sica -> harmonia primeiro ou melodia primeiro?

#inicializa??o de vari?veis
form <- "AABA"
number_chorus <- 2
number_bars <- 32
number_chorus_improv <- 0
max_note_per_beat <- 0
##browser()
key <- "F"
tone <- "maj"
key.bridge <- "D"
tone.bridge <- "min"
struct <- c("AABABA",2,2,0)
#struct.bridge <- c("AABABA",2,2,0)
struct.bridge <- c("AABABA",4,4,0)
bpm <- 120
#2bars
melody.head <- melody(key, tone, struct, max_note_per_beat, harmony_first=0, harmony_in, rythm = 0)
#4bars
melody.bridge <- melody(key.bridge, tone.bridge, struct.bridge, max_note_per_beat, harmony_first=0, harmony_in, rythm = 0)
#24bars
#melody.final <- cbind(melody.head,melody.head,melody.head,melody.head[,ncol(melody.head):1],melody.bridge, melody.bridge[,ncol(melody.bridge):1], melody.head,melody.head,melody.bridge,melody.bridge[,ncol(melody.bridge):1],melody.head,melody.head)
melody.final <- cbind(melody.head,melody.head,melody.head,melody.head[,ncol(melody.head):1],melody.bridge, melody.head,melody.head,melody.bridge,melody.head,melody.head)
#browser()
sample <- nullSample(rate=44100, bits=16, channels=1)
for(i in 1:ncol(melody.final)){
  #browser()
  output <- notes(melody.final[1,i], melody.final[2,i])
  #wait(play(output))
  sample <- appendSample(sample, output)
}
#sample <- as.Sample(sample)
wait(play(sample))
s <- saveSample(sample,"music-25-Cmaj-AABABA.wav")
